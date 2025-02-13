;;; cur-yt.el --- a front end for playing YouTube videos with mpv -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'rx)

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

(defun cur-yt--get-view-key (url)
  "Get view key from URL."
  (save-match-data
    (string-match cur-yt--youtube-view-key-regexp url)
    (match-string-no-properties 1 url)))

(defun cur-yt--convert-to-yt-link (url)
  "Convert URL to a YouTube link."
  (let ((key (cur-yt--get-view-key url)))
    (when key
	(format "https://www.youtube.com/watch?v=%s" key))))

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

;;;###autoload
(defun cur-yt-play-video (url &optional resolution)
  "Play video URL at RESOLUTION."
  (interactive
   (list (read-string "YouTube URL: " nil
		      cur-yt-play-url-history
		      (when kill-ring
			(cur-yt--convert-to-yt-link (substring-no-properties (car kill-ring)))))
	 (when (or cur-yt-play-always-prompt-resolution current-prefix-arg)
	   (completing-read "Resolution: "
			    '("worst" "144" "240" "360" "480" "720" "1080" "1440" "2160" "best")
			    nil 'confirm nil t))))
  (cur-yt--play-check-exetutables)
  (let ((res (or resolution cur-yt-play-default-resolution))
	(yt-url (cur-yt--convert-to-yt-link url)))
    (unless yt-url
      (user-error "URL did not contain a YouTube view key"))
    (start-process "my-process" (get-buffer-create "sugma")
		   "mpv" (cur-yt--mpv-ytdl-format res) yt-url)
    (message "Playing %s at %s" yt-url (pcase res
					 ((or "best" "worst") (concat res " resolution"))
					 (_ (concat res "p"))))))

(provide 'cur-yt)
;;; cur-yt.el ends here
