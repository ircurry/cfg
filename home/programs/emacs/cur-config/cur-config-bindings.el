;; ===Repeat Mode===
(use-package repeat
  :hook (after-init . repeat-mode))

(use-package avy
  :custom
  (avy-timeout-seconds 1.0)
  :bind ( :map goto-map
	  ("a" . avy-isearch)
	  ("c" . avy-goto-char-2)
	  (";" . avy-goto-char-timer)))

;; ===Meow Setup===
(use-package meow
  :demand t
  :preface
  (defvar cur/sub-leader-keymap
    (let ((map (make-sparse-keymap)))
      map)
    "The keymap for quick command/function execution.")
  (defvar cur/toggle-map
    (let ((map (make-sparse-keymap)))
      map)
    "Keymap for commands that change settings from the leader key.")
  (defvar cur/register-map
    (let ((map (make-sparse-keymap)))
      map)
    "Keymap for commands that use registers.")
  :init
  (defun cur/shell-filter-active-region ()
    (interactive)
    (if (region-active-p)
        (shell-command-on-region (region-beginning) (region-end)
                                 (read-shell-command "Filter Region with: ")
                                 1 1)
      (user-error "The region is not currently active")))
  (defun cur/reverse-other-window ()
    (interactive)
    (other-window -1))
  
  :custom
  (meow-mode-state-list
   '((authinfo-mode . normal)
     (beancount-mode . normal)
     (bibtex-mode . normal)
     (cider-repl-mode . normal)
     (cider-test-report-mode . normal)
     (cider-browse-spec-view-mode . motion)
     (cargo-process-mode . normal)
     (conf-mode . normal)
     (deadgrep-edit-mode . normal)
     (deft-mode . normal)
     (diff-mode . normal)
     (dired-mode . motion)
     (eat-mode . insert)
     (ediff-mode . motion)
     (eshell-mode . insert)
     (gud-mode . normal)
     (help-mode . motion)
     (helpful-mode . normal)
     (json-mode . normal)
     (jupyter-repl-mode . normal)
     (mix-mode . normal)
     (occur-edit-mode . normal)
     (pass-view-mode . normal)
     (prog-mode . normal)
     (py-shell-mode . normal)
     (restclient-mode . normal)
     (telega-chat-mode . normal)
     (term-mode . normal)
     (text-mode . normal)
     (vterm-mode . insert)
     (Custom-mode . normal))
   "Default meow states for modes")
  (meow-selection-command-fallback
   '((meow-change . meow-change-char)
     (meow-kill . meow-C-d)
     (meow-cancel-selection . keyboard-quit)
     (meow-pop-selection . meow-pop-grab)
     (meow-beacon-change . meow-beacon-change-char))
   "Meow fallback commands")
  (meow-keypad-describe-delay 0.0 "No delay in keypad help popup")
  (meow-cheats-layout meow-cheatsheet-layout-qwerty "Meow qwerty layout for the cheatsheet")
  (meow-keypad-leader-dispatch nil)
  (meow-visit-sanitize-completion t)
  :bind ( :map cur/sub-leader-keymap
          ("C-l" . ibuffer)
          ("C-b" . bookmark-set-no-overwrite)
          ("C-c" . compile)
          :map cur/register-map
          ("C-j" . jump-to-register)
          ("C-." . point-to-register)
          ("C-y" . copy-to-register)
          ("C-p" . insert-register)
          ("C-w" . window-configuration-to-register)
          ("C-n" . number-to-register)
          ("C-+" . increment-register)
          ("C-q" . kmacro-to-register)
          :map goto-map
          ("e a" . first-error)
          ("e n" . next-error)
          ("e p" . previous-error)
          (","   . xref-go-back)
          ("d"   . xref-find-definitions)
          ("r"   . xref-find-references)
          ("A"   . xref-find-apropos)
          ("h"   . move-beginning-of-line)
          ("j"   . end-of-buffer)
          ("k"   . beginning-of-buffer)
          ("l"   . move-end-of-line)
          ("J"   . next-logical-line)
          ("K"   . previous-logical-line)
          ("s"   . back-to-indentation)
          :map next-error-repeat-map
          ("a" . first-error))
  :config
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; ===Top Row===
   '("1" . digit-argument)
   '("2" . digit-argument)
   '("3" . digit-argument)
   '("4" . digit-argument)
   '("5" . digit-argument)
   '("6" . digit-argument)
   '("7" . digit-argument)
   '("8" . digit-argument)
   '("9" . digit-argument)
   '("0" . digit-argument)

   ;; ===2nd Row===
   ;; '("TAB"  . )
   '("q" . meow-comment)  ; comment dwim
   ;; '("w" . ) ; bound in ace-window section
   ;; '("W" . )
   ;; '("e" . ) ; bound in popper section
   (cons "r" cur/register-map)
   (cons "t" cur/toggle-map)
   ;; '("y" . )
   '("u" . "C-u")		; universal argument
   ;; '("i" . ) ; bound in popper section
   '("o" . other-window)		; other window
   (cons "p" project-prefix-map)
   ;; '("-" . )

   ;; ===3rd Row===
   ;; '("<escape>" . )
   ;; '("a" . )
   '("s" . "C-x C-s")			; save buffer
   '("d" . kill-current-buffer)
   '("f" . "C-x C-f")			; find file
   ;; '("g" . ) ; C-M- map
   ;; '("h" . ) ; C-h map
   ;; '("j" . )
   ;; '("k" . )
   ;; '("l" . )
   (cons ";" cur/sub-leader-keymap)
   '("RET"   . "M-x")

   ;; ===4th Row===
   ;; '("z" . )
   ;; '("x" . ) ; C-x map
   ;; '("c" . ) ; C-c map
   '("C" . capitalize-dwim)
   (cons "v" search-map)
   '("b" . "C-x b")
   ;; '("n" . )
   ;; '("m" . ) ; M- map
   '("," . meow-beginning-of-thing)
   '("." . meow-end-of-thing)
   '("/" . meow-visit)
   ;; '("'"  . )
   )
  (meow-normal-define-key
   ;; ===Special===
   '("|" . cur/shell-filter-active-region)

   ;; ===Top Row===
   '("1" . meow-expand-1)
   ;; '("!" . )
   '("2" . meow-expand-2)
   ;; '("@" . )
   '("3" . meow-expand-3)
   ;; '("#" . )
   '("4" . meow-expand-4)
   ;; '("$" . )
   '("5" . meow-expand-5)
   '("%" . meow-query-replace)
   '("6" . meow-expand-6)
   ;; '("^" . )
   '("7" . meow-expand-7)
   ;; '("&" . )
   '("8" . meow-expand-8)
   ;; '("*" . )
   '("9" . meow-expand-9)
   '("(" . meow-start-kmacro)
   '("0" . meow-expand-0)
   '(")" . meow-end-kmacro)

   ;; ===2nd Row===
   ;; '("TAB"  . )
   ;; '("BTAB" . )
   ;; '("q" . )
   '("Q" . kmacro-end-and-call-macro)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("t" . meow-till)
   '("T" . meow-till-expand)
   '("y" . meow-save)
   '("Y" . meow-clipboard-save)
   '("u" . meow-undo)
   ;; '("U" . )
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("P" . meow-yank-pop)
   '("-" . negative-argument)
   ;; '("_" . )

   ;; ===3rd Row===
   '("<escape>" . keyboard-quit)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("s" . repeat)
   ;; '("S" . )
   '("d" . meow-kill)
   '("D" . meow-c-k)
   '("f" . meow-find)
   '("F" . meow-find-expand)
   '("g" . meow-cancel-selection)
   ;; '("G" . )
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '(";" . meow-reverse)
   ;; '(":" . )
   ;; '("RET" . )
   ;; '("S-RET" . )

   ;; ===4th Row===
   '("z" . meow-pop-selection)
   ;; '("Z" . )
   '("x" . meow-line)
   '("X" . meow-line-expand)
   '("c" . meow-change)
   '("C" . meow-sync-grab)
   (cons "v" goto-map)
   ;; '("V" . )
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("n" . meow-search)
   '("N" . meow-visit)
   '("m" . meow-join)
   ;; '("M" . )
   '("," . meow-bounds-of-thing)
   '("<" . beginning-of-buffer)
   '("." . meow-inner-of-thing)
   '(">" . end-of-buffer)
   ;; '("/" . isearch-forward-regexp)
   ;; '("?" . isearch-backward-regexp)
   '("'" . meow-grab)
   '("\"" . meow-pop-grab))
  (meow-global-mode +1))

;; ===Meow Extensions===
(use-package cur-meow
  :after (meow)
  :commands (cur-meow-mini-search)
  :bind ( :map cur/sub-leader-keymap
          ("C-n" . cur-meow-toggle-temp-normal-motion))
  :init
  (meow-normal-define-key
   '("/" . cur-meow-mini-search)))

(use-package meow-tree-sitter
  :after meow
  :demand t
  :config
  (meow-tree-sitter-register-defaults))

(provide 'cur-config-bindings)
