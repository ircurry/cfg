;;; early-init.el --- Pre-initialization Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda (&rest _) (setq gc-cons-threshold 16777216)))

(setopt initial-frame-alist '((left-fringe . 0)
			      (right-fringe . 0)
			      (tool-bar-lines . 0))
	inhibit-startup-message t ; Starts on blank screen
	initial-buffer-choice t   ; Scratch as initial buffer
	;; initial-major-mode 'fundamental-mode
	initial-scratch-message ";; Scratch Buffer\n\n"
	scroll-bar-mode nil ; Disable visible scrollbar
	tool-bar-mode nil   ; Disable tool bar
	menu-bar-mode nil   ; Disable menu bar
	tooltip-mode nil)   ; Disable tooltips

(set-fringe-mode 10)           ; Set fringe size
(setq frame-title-format "%b") ; Set window title to buffer name

(provide 'early-init)
;;; early-init.el ends here
