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
          ("TAB"  . dired-hide-subdir)
          ("<backtab>" . dired-hide-all)
          ("q" . quit-window)
          ;; ("Q" . )
          ("w" . dired-mark-subdir-files)
          ;; ("W" . ) ; Bound it custom wallpaper settings
          ("e" . dired-next-subdir)
          ;; ("E" . )
          ("r" . dired-do-rename)
          ("R" . dired-do-rename-regexp)
          ("t" . dired-toggle-marks)
          ("T" . dired-kill-tree)
          ("y" . dired-copy-filename-as-kill)
          ;; ("Y" . )
          ("u" . dired-unmark)
          ("U" . dired-unmark-all-marks)
          ("i" . dired-toggle-read-only)
          ("I" . image-dired)
          ("o" . dired-do-async-shell-command)
          ("O" . dired-do-chown)
          ("p" . dired-next-marked-file)
          ;; ("P" . )
          ("-" . negative-argument)
          ;; ("_" . )

          ;; ===3rd Row===
          ("<escape>" . keyboard-quit)
          ("a" . dired-find-alternate-file)
          ;; ("A" . dired-find-alternate-file)
          ("s" . dired-kill-subdir)
          ("S" . dired-kill-tree)
          ("d" . dired-do-kill-lines)
          ("D" . dired-do-delete)
          ("f" . dired-goto-file)
          ("F" . dired-goto-subdir)
          ("g" . revert-buffer)
          ;; ("G" . )
          ("h" . dired-up-directory)
          ;; ("H" . )
          ;; ("j" . ) ; already bound in meow-motion-mode
          ;; ("J" . )
          ;; ("k" . ) ; already bound in meow-motion-mode
          ;; ("K" . )
          ;; ("l" . ) ; bound in cur-dired declaration
          ("L" . dired-find-file-other-window)
          (";" . dired-toggle-marks)
          (":" . scroll-down-command)
          ("RET" . dired-find-file)
          ("S-RET" . dired-find-file-other-window)

          ;; ===4th Row===
          ("z" . dired-undo)
          ;; ("Z" . )
          ("x" . dired-mark)
          ("X" . dired-mark-files-regexp)
          ("c" . dired-do-copy)
          ("C" . dired-do-copy-regexp)
          ("v" . dired-view-file)
          ;; ("V" . )
          ("b" . dired-prev-subdir)
          ;; ("B" . )
          ("n" . dired-next-marked-file)
          ;; ("N" . )
          ;; ("m" . )
          ("M" . dired-do-chmod)
          ("," . dired-prev-dirline)
          ("<" . beginning-of-buffer)
          ("." . dired-next-dirline)
          (">" . end-of-buffer)
          ;; ("/" . )
          ("?" . scroll-up-command)
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
                                  ("\\.pdf" "xdg-open" "zathura")
                                  (".*" "xdg-open")))
  (dired-auto-revert-buffer #'dired-directory-changed-p)
  (dired-free-space nil)
  (dired-make-directory-clickable t)
  (dired-mouse-drag-files t)
  :config
  (setq dired-deletion-confirmer 'y-or-n-p)
  (setopt dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir)))))

(use-package cur-dired
  :after (dired)
  :bind ( :map dired-mode-map
	  ("l" . cur-dired-maybe-insert-subdir-or-find-file)))

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
