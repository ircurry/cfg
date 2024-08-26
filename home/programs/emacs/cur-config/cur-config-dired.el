;; ===Dired Icons===
(use-package nerd-icons-dired
  :after (dired)
  :hook (dired-mode . nerd-icons-dired-mode))

;; ===Dired===
(use-package dired
  :ensure nil
  :hook
  (dired-mode . dired-hide-details-mode) ; don't show file details by default
  (dired-mode . hl-line-mode) ; Highlight the line the cursor is on
  ;; TODO: create a dired-find-file-dwim for external programs
  :bind ( :map dired-mode-map
          ;; ===Top Row===
          ;; ("1" . )
          ("!" . dired-do-shell-command)
          ;; ("2" . )
          ;; ("@" . )
          ;; ("3" . )
          ;; ("#" . )
          ;; ("4" . )
          ;; ("$" . )
          ;; ("5" . )
          ;; ("%" . )
          ;; ("6" . )
          ;; ("^" . )
          ;; ("7" . )
          ("&" . dired-do-async-shell-command)
          ;; ("8" . )
          ;; ("*" . )
          ;; ("9" . )
          ;; ("(" . )
          ;; ("0" . )
          ;; (")" . )

          ;; ===2nd Row===
          ;; ("TAB"  . )
          ;; ("BTAB" . )
          ("q" . quit-window)
          ;; ("Q" . )
          ;; ("w" . )
          ;; ("W" . )
          ;; ("e" . )
          ;; ("E" . )
          ("r" . dired-do-rename)
          ("R" . dired-toggle-read-only)
          ("t" . dired-toggle-marks)
          ;; ("T" . )
          ;; ("y" . )
          ;; ("Y" . )
          ("u" . dired-unmark)
          ("U" . dired-unmark-all-marks)
          ("i" . dired-isearch-filenames-regexp)
          ("I" . dired-do-isearch-regexp)
          ;; ("o" . )
          ("O" . dired-do-chown)
          ("p" . dired-previous-line)
          ;; ("P" . )
          ("-" . negative-argument)
          ;; ("_" . )

          ;; ===3rd Row===
          ("<escape>" . keyboard-quit)
          ;; ("a" . )
          ;; ("A" . )
          ;; ("s" . )
          ;; ("S" . )
          ("d" . dired-do-kill-lines)
          ("D" . dired-do-delete)
          ("f" . find-file)
          ;; ("F" . )
          ("g" . revert-buffer)
          ;; ("G" . )
          ("h" . dired-up-directory)
          ;; ("H" . )
          ("j" . dired-next-line)
          ;; ("J" . )
          ("k" . dired-previous-line)
          ;; ("K" . )
          ("l" . dired-find-file)
          ("L" . dired-find-file-other-window)
          ;; (";" . )
          ;; (":" . )
          ("RET" . dired-do-async-shell-command)
          ;; ("S-RET" . )

          ;; ===4th Row===
          ("z" . dired-undo)
          ;; ("Z" . )
          ("x" . dired-mark)
          ;; ("X" . )
          ("c" . dired-do-copy)
          ("C" . dired-do-copy-regexp)
          ("v" . dired-find-file-other-window)
          ;; ("V" . )
          ;; ("b" . )
          ;; ("B" . )
          ("n" . dired-next-line)
          ;; ("N" . )
          ("m" . dired-mark-files-regexp)
          ("M" . dired-do-chmod)
          ;; ("," . )
          ;; ("<" . )
          ;; ("." . )
          ;; (">" . )
          ("/" . dired-goto-file)
          ;; ("?" . )
          ;; ("'"  . ) ; Leave these blank, usually embark-act
          ;; ("\"" . ) ; Leave these blank, usually embark-dwim
          :map cur/sub-leader-keymap
          ("d" . dired))
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  (dired-listing-switches "-A -G -F -h -l -v --group-directories-first --time-style=long-iso")
  (dired-guess-shell-alist-user '(("\\.\\(png\\|jpe?g\\|tiff\\|gif\\)" "xdg-open" "imv" "feh")
                                  ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\|mov\\)" "xdg-open" "mpv" "vlc")
                                  ("\\.pdf" "zathura" "xdg-open")
                                  (".*" "xdg-open")))
  (dired-auto-revert-buffer #'dired-directory-changed-p)
  (dired-free-space nil)
  (dired-make-directory-clickable t)
  (dired-mouse-drag-files t)
  :config
  (setq dired-deletion-confirmer 'y-or-n-p))

(provide 'cur-config-dired)

(use-package cur-wallpaper
  :after (dired)
  :bind ( :map dired-mode-map
	  ("W" . cur-wallpaper-set-wallpaper-dired)))

(use-package cur-wallpaper
  :after (image-dired)
  :bind ( :map image-dired-thumbnail-mode-map
	  ("W" . cur-wallpaper-set-wallpaper)))

(provide 'cur-config-dired)
