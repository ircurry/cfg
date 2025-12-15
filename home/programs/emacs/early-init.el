;;; early-init.el --- Pre-initialization Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda (&rest _) (setq gc-cons-threshold 16777216)))

(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable tool bar
(menu-bar-mode -1)   ; Disable menu bar
(tooltip-mode -1)    ; Disable tooltips

(setq initial-frame-alist '((left-fringe . 0)
			    (right-fringe . 0)
			    (tool-bar-lines . 0))
      inhibit-startup-message t     ; Starts on blank screen
      inhibit-x-resources t         ; No Xresources
      inhibit-startup-buffer-menu t ; Don't open buffer list when more than 3 files
      initial-buffer-choice t       ; Scratch as initial buffer
      initial-scratch-message ";; Scratch Buffer\n\n"
      frame-title-format "%b"       ; Set window title to buffer name
      frame-resize-pixelwise t)     ; Pixel perfect frame size

(set-fringe-mode 10) ; Set fringe size

(provide 'early-init)
;;; early-init.el ends here
