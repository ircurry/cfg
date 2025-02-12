;;; cur-yt.el --- a front end for playing YouTube videos with mpv -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'rx)

(defgroup cur-yt ()
  "UI for working with YouTube videos in Emacs."
  :group 'external)

(defcustom cur-yt-default-resolution "1080"
  "The target resolution for the video.
If this video resolution is not available the next best resolution will be
downloaded instead.  It should be one of the common resolution heights YouTube
uses."
  :type '(choice (const "2160")
		 (const "1440")
		 (const "1080")
		 (const "720")
		 (const "480")
		 (const "360")
		 (const "240")
		 (const "144"))
  :group 'cur-yt)

(defun cur-yt--check-exetutables ()
  "Check if `mpv' and `yt-dlp'/`youtube-dl' are installed."
  (unless (executable-find "mpv")
    (error "`mpv' is not installed on the system, please install it"))
  (unless (or (executable-find "yt-dlp")
	      (executable-find "youtube-dl"))
    (error "Neither `yt-dlp' nor `youtube-dl' are installed, please install one of them"))
  t)

(defvar cur-yt--mpv-path nil
  "The executable path to `mpv'.")

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
  (string-match cur-yt--youtube-view-key-regexp url)
  (match-string-no-properties 1 url))

(defun cur-yt--convert-to-yt-link (url)
  "Convert URL to a YouTube link."
  (let ((key (cur-yt--get-view-key url)))
    (when key
	(format "https://www.youtube.com/watch?v=%s" key))))

(defun cur-yt--ytdl-format (height)
  "Return the `mpv' option to get the best video of HEIGHT."
  (format "--ytdl-format=bestvideo[height<=?%s]+bestaudio/best" height))

(defvar cur-yt-url-history nil
  "Previous YouTube URLs played.")

;;;###autoload
(defun cur-yt-play-video (url &optional resolution)
  "Play video URL at RESOLUTION."
  (interactive
   (list (read-string "YouTube URL: " nil
		      cur-yt-url-history
		      (when kill-ring
			(cur-yt--convert-to-yt-link (substring-no-properties (car kill-ring)))))
	 (when current-prefix-arg
	   (completing-read "Resolution: "
			    '("144" "240" "360" "480" "720" "1080" "1440" "2160")
			    nil 'confirm nil t))))
  (cur-yt--check-exetutables)
  (when (and resolution (not (string-match-p (rx (+ (any "0-9"))) resolution)))
    (user-error "Resolution must be a number"))
  (let ((res (or resolution cur-yt-default-resolution))
	(yt-url (cur-yt--convert-to-yt-link url)))
    (unless yt-url
      (user-error "URL did not contain a YouTube view key"))
    (start-process "my-process" (get-buffer-create "sugma")
		   "mpv" (cur-yt--ytdl-format res) yt-url)
    (message "Playing %s at %sp" yt-url res)))

(provide 'cur-yt)
;;; cur-yt.el ends here
