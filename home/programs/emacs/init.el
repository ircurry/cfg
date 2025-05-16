;; ===Check Emacs Version===
(when (version< emacs-version "29.4")
  (error "This setup is only guarunteed to work on 29.4, disable this line to load anyways."))

;; ===Basic Settings===
(setq frame-title-format "%b")      ; Set window title to buffer name
(setq initial-scratch-message ";; Scratch Buffer\n\n")
(setq initial-buffer-choice t)      ; Scratch as initial buffer
(setq make-backup-files nil)        ; No more file~ everywhere
(setq inhibit-startup-message t)    ; Starts on blank screen
(setq scroll-conservatively 101)    ; Enable Line by line scrolling
(setq scroll-margin 0)              ; Start scrolling when at the very top/bottom of window
(setq recenter-positions '(top bottom middle)) ; recenter line on top, then bottom, then middle
(setq use-dialog-box nil)           ; No yes-no-p for mouse clicks
(setq use-short-answers t)          ; Use y-or-n-p for yes-no dialogue.
(setq async-shell-command-buffer 'new-buffer) ; Create new buffer or each async-shell-command
(scroll-bar-mode -1)                ; Disable visible scrollbar
(tool-bar-mode -1)                  ; Disable tool bar
(menu-bar-mode -1)                  ; Disable menu bar
(tooltip-mode -1)                   ; Disable tooltips
(set-fringe-mode 10)                ; No idea what this does

;; ===Enable Disabled Functions===
(dolist (c '(narrow-to-region narrow-to-page upcase-region downcase-region))
  (put c 'disabled nil))

;; ===Disable Custom===
(setq custom-file (make-temp-file "emacs-custom-"))

;; ===Package Initialization===
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/"))) ;; Setting Repos
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; ===use-package Initialization===
(require 'use-package)
;; (setq use-package-compute-statistics t) ; Uncomments to track package load times
(setopt use-package-always-pin    nil
	use-package-always-defer  t
	use-package-always-demand nil
	use-package-always-ensure nil)

;; ===Load Modules and Packages===
(dolist (path '("cur-lisp" "cur-config"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

(require 'cur-config-essentials)
(require 'cur-config-bindings)
(require 'cur-config-dired)
(require 'cur-config-faces)
(require 'cur-config-completion)
(require 'cur-config-window)
(require 'cur-config-ide)
(require 'cur-config-shell)
(require 'cur-config-org)
(require 'cur-config-net)
(require 'cur-config-direnv) ; Direnv is the last to be loaded

(provide 'init)
