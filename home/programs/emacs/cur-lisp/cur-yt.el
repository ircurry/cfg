;;; cur-yt.el --- a front end for playing YouTube videos with mpv -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'rx)
(require 'cl-lib)

(defgroup cur-yt ()
  "UI for working with YouTube videos in Emacs."
  :group 'external)

;;; Helper Functions

(defvar cur-yt--youtube-view-key-regexp
  (rx bol
      (*? not-newline)
      "watch?v="
      (group-n 1 (= 11 anything))
      (*? not-newline)
      eol)
  "Regexp used to get view key from a YouTube URL.")

(defun cur-yt-get-view-key (url)
  "Get view key from URL."
  (when (not (string-empty-p url))
    (save-match-data
      (string-match cur-yt--youtube-view-key-regexp url)
      (match-string-no-properties 1 url))))

(defun cur-yt--format-key-as-yt-link (key)
  "Format KEY as a YouTube link."
  (format "https://www.youtube.com/watch?v=%s" key))

(defun cur-yt--convert-to-yt-link (url)
  "Convert URL to a YouTube link."
  (let ((key (cur-yt-get-view-key url)))
    (when key
      (cur-yt--format-key-as-yt-link key))))

(defun cur-yt--mpv-ytdl-format (height)
  "Return the `mpv' option to get the best video of HEIGHT."
  (pcase height
    ("best" "--ytdl-format=bestvideo+bestaudio/best")
    ("worst" "--ytdl-format=worstvideo+worstaudio/worst")
    ((guard (string-match-p (rx (+ (any "0-9"))) height))
     (format "--ytdl-format=bestvideo[height<=?%s]+bestaudio/best" height))
    (_ (error "Cannot format option for mpv, '%s' is not a number" height))))

;;; Play Video

(defcustom cur-yt-play-default-resolution "1080"
  "The default video resolution for `cur-yt-play-video'.
If this video resolution is not available the next best resolution will be
downloaded instead.  It should be one of the common resolution heights YouTube
uses, \"best\" or \"worst\"."
  :type '(choice (const "best")
		 (const "2160")
		 (const "1440")
		 (const "1080")
		 (const "720")
		 (const "480")
		 (const "360")
		 (const "240")
		 (const "144")
		 (const "worst"))
  :group 'cur-yt)

(defvar cur-yt-play-url-history nil
  "Previous YouTube URLs played with `cur-yt-play-video'.")

(defun cur-yt--play-check-exetutables ()
  "Check if `mpv' and `yt-dlp'/`youtube-dl' are installed.
This function with error if it finds a missing program."
  (unless (executable-find "mpv")
    (error "`mpv' is not installed on the system, please install it"))
  (unless (or (executable-find "yt-dlp")
	      (executable-find "youtube-dl"))
    (error "Neither `yt-dlp' nor `youtube-dl' are installed, please install one of them"))
  t)

(defcustom cur-yt-play-always-prompt-resolution nil
  "Whether or not `cur-yt-play-video' should always prompt for resolution."
  :type '(boolean)
  :group 'cur-yt)

(cl-defun cur-yt--get-resolution (&optional (prompt-p t))
  "Interactively get the resolution for the YouTube video.
The primary factor in determining whether or not to prompt is the variable
`cur-yt-play-always-prompt-resolution'.
Optional argument PROMPT-P determines whether to prompt, which is true by
default."
  (when (or cur-yt-play-always-prompt-resolution prompt-p)
    (completing-read "Resolution: "
		     '("worst" "144" "240" "360" "480" "720" "1080" "1440" "2160" "best")
		     nil 'confirm nil t)))

(defun cur-yt-key-from-kill-ring ()
  "Search for a YouTube view-key in the first element of the `kill-ring'."
  (cur-yt-get-view-key (substring-no-properties (or (car kill-ring) ""))))

(defcustom cur-yt-context-alist '((elfeed-search-mode . cur-yt-elfeed-search-key)
				  (elfeed-show-mode . cur-yt-elfeed-show-key)
				  (yeetube-mode . cur-yt-yeetube-key)
				  (t . cur-yt-key-from-kill-ring))
  "An alist of `major-mode' names to functions.
The functions take no arguments and return the view-key or link of a YouTube
video.  If there is no view-key or link in the current context the function
should return nil.
If no function is bound to the `major-mode', the function associated with t is
consulted instead, should it exist."
  :type '(alist :key-type symbol :value-type function)
  :group 'cur-yt)

(defun cur-yt--get-default-key ()
  "Get the default view key based on the current context.
The current context is gotten through the function associated with the current
`major-mode' in `cur-yt-context-alist'."
  (if-let* ((fn (cdr (assoc major-mode cur-yt-context-alist))))
      (funcall fn)
    (if-let* ((default-fn (cdr (assoc t cur-yt-context-alist))))
	(funcall default-fn)
      "")))

;;;###autoload
(defun cur-yt-play-video (url &optional resolution no-video)
  "Play video URL at RESOLUTION.
If NO-VIDEO is non-nil novideo will be played."
  (interactive
   (let* ((default-key (cur-yt--get-default-key))
	  (prompt (if default-key
		      (format "YouTube URL (%s): " default-key)
		    "YouTube URL: ")))
     (list (read-string prompt nil
			cur-yt-play-url-history
			(cur-yt--format-key-as-yt-link default-key))
	   (cur-yt--get-resolution current-prefix-arg)
	   nil)))
  (cur-yt--play-check-exetutables)
  (let ((res (or resolution cur-yt-play-default-resolution))
	(yt-url (cur-yt--convert-to-yt-link url)))
    (unless yt-url
      (user-error "URL did not contain a YouTube view key"))
    (let ((args (list "cur-yt-play-video"
		      (get-buffer-create "*cur-yt-process-buffer*")
		      "mpv"
		      (cur-yt--mpv-ytdl-format res)
		      yt-url)))
      (when no-video (setq args (append args '("--no-video"))))
      (apply #'start-process args))
    (message "Playing %s at %s" yt-url (pcase res
					 ((or "best" "worst") (concat res " resolution"))
					 (_ (concat res "p"))))))

;;; YeeTube integration

;;;###autoload
(defun cur-yt-yeetube-key ()
  "Get the view-key of the YouTube video at pint in the current *yeetube* buffer."
  (when (and (fboundp 'tabulated-list-get-id)
	     (fboundp 'yeetube-get-url))
    (save-excursion
      (and (null (tabulated-list-get-id)) (end-of-line))
      (cur-yt-get-view-key (yeetube-get-url (tabulated-list-get-id))))))

;;;###autoload
(defun cur-yt-yeetube-play (url &optional _)
  "This is a function intended to be used as the value of `yeetube-play-function'.
URL is the url link to the YouTube video.  This function is more or less the
same as `cur-yt-yeetube-play-resolution' but plays the default resolution
instead of prompting for it."
  (let ((no-video (if (boundp 'yeetube-mpv-disable-video)
		      yeetube-mpv-disable-video
		    nil)))
    (cur-yt-play-video url nil no-video)))

;;;###autoload
(defun cur-yt-yeetube-play-resolution (url &optional _)
  "This is a function intended to be used as the value of `yeetube-play-function'.
URL is the url link to the YouTube video.  This function is more or less the
same as `cur-yt-yeetube-play' but prompts for the resolution instead of using
the default value."
  (let ((no-video (if (boundp 'yeetube-mpv-disable-video)
		      yeetube-mpv-disable-video
		    nil)))
    (cur-yt-play-video url (cur-yt--get-resolution) no-video)))

;;; Elfeed integration

;;;###autoload
(defun cur-yt-elfeed-show-key ()
  "Return the view key of the link in the current *elfeed-search* buffer."
  (when (and (boundp 'elfeed-show-entry)
	     (fboundp 'elfeed-entry-link))
    (cur-yt-get-view-key (elfeed-entry-link elfeed-show-entry))))

;;;###autoload
(defun cur-yt-elfeed-search-key ()
  "Return the view key of the link in the current *elfeed-search* buffer."
  (when (and (fboundp 'elfeed-search-selected)
	     (fboundp 'elfeed-entry-link))
    (if-let* ((entries (elfeed-search-selected))
	      ((length= entries 1)))
	(cur-yt-get-view-key (elfeed-entry-link (car entries))))))

(provide 'cur-yt)
;;; cur-yt.el ends here
