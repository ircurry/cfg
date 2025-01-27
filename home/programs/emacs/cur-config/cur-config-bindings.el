;; ===Repeat Mode===
(use-package emacs
  :demand t
  :config
  (repeat-mode 1))

;; ===Hydra===
(use-package hydra
  :demand t)

(use-package avy
  :bind ( :map goto-map
	  ("c" . avy-goto-char-2)))

;; ===Meow Setup===
(use-package meow
  :after (hydra)
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
  (defvar cur/alignment-map
    (let ((map (make-sparse-keymap)))
      map)
    "Keymap for commands that change alignment and cursor display.")
  (defvar cur/register-map
    (let ((map (make-sparse-keymap)))
      map)
    "Keymap for commands that use registers.")
  (defun cur/shell-filter-active-region ()
    (interactive)
    (if (region-active-p)
        (shell-command-on-region (region-beginning) (region-end)
                                 (read-shell-command "Filter Region with: ")
                                 1 1)
      (user-error "The region is not currently active")))
  (defhydra cur/window (:hint nil)
    "
^Movement^            ^Splitting and Balancing^   ^Manipulation^       ^Resizing^
^--------^------------^-----------------------^---^------------^-------^--------^---------------------
_h_: left             _._: only window            _H_: swap left       _i_: enlarge window
_j_: down             _v_: split vertical         _J_: swap down       _r_: shrink window
_k_: up               _s_: split horizontal       _K_: swap up         _f_: enlarge window horizonally
_l_: right            _=_: balance windows        _L_: swap right      _b_: shrink window horizonally
_o_: other window     _F_: fit to buffer          _d_: close window
"
    ("RET" ignore "finished" :exit t)

    ("o" other-window)
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)

    ("." delete-other-windows)
    ("v" split-window-right)
    ("s" split-window-below)
    ("=" balance-windows)
    ("F" fit-window-to-buffer)

    ("H" windmove-swap-states-left)
    ("J" windmove-swap-states-down)
    ("K" windmove-swap-states-up)
    ("L" windmove-swap-states-right)
    ("d" delete-window)

    ("i" enlarge-window)
    ("r" shrink-window)
    ("f" enlarge-window-horizontally)
    ("b" shrink-window-horizontally))
  (defun cur/reverse-other-window ()
    (interactive)
    (other-window -1))
  (defun meow-setup ()
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; ===Top Row===
     ;; '("1" . )
     ;; '("2" . )
     ;; '("3" . )
     ;; '("4" . )
     ;; '("5" . )
     ;; '("6" . )
     ;; '("7" . )
     ;; '("8" . )
     ;; '("9" . )
     ;; '("0" . )

     ;; ===2nd Row===
     ;; '("TAB"  . )
     '("q" . meow-comment) ; comment dwim
     ;; '("w" . ) ; bound in ace-window section
     '("W" . cur/window/body)
     ;; '("e" . ) ; bound in popper section
     (cons "r" cur/register-map)
     (cons "t" cur/toggle-map)
     ;; '("y" . )
     '("u" . "C-u")   ; universal argument
     ;; '("i" . )
     '("o" . other-window) ; other window
     (cons "p" project-prefix-map)
     ;; '("-" . )

     ;; ===3rd Row===
     ;; '("<escape>" . )
     ;; '("a" . )
     '("s" . "C-x C-s") ; save buffer
     '("d" . kill-current-buffer)
     '("f" . "C-x C-f") ; find file
     ;; '("g" . ) ; C-M- map
     ;; '("h" . ) ; C-h map
     ;; '("j" . )
     ;; '("k" . )
     ;; '("l" . )
     (cons ";" cur/sub-leader-keymap)
     '("RET"   . "M-x")

     ;; ===4th Row===
     (cons "z" cur/alignment-map)
     ;; '("x" . ) ; C-x map
     ;; '("c" . ) ; C-c map
     '("C" . capitalize-dwim)
     ;; '("v" . )
     '("b" . "C-x b")
     ;; '("n" . )
     ;; '("m" . ) ; M- map
     '("," . meow-beginning-of-thing)
     '("." . meow-end-of-thing)
     '("/" . rg)
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
     '(":" . meow-page-up)
     '("RET" . "C-c")
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
     ;; '("/" . ) ; I bind this later to `consult-line'
     '("?" . meow-page-down)
     '("'" . meow-grab)
     '("\"" . meow-pop-grab)))
  :init
  (defhydra cur/hydra-buffer-cycle (:timeout 4)
    "tab through buffers"
    ("n" next-buffer "next buffer")
    ("p" previous-buffer "previous buffer")
    ("f" nil "finished" :exit t)
    ("RET" nil "finished" :exit t))
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
     (haskell-interactive-mode . normal)
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
  (meow-visit-sanitize-completion nil)
  :bind ( :map cur/sub-leader-keymap
          ("C-l" . ibuffer)
          ("C-b" . bookmark-set-no-overwrite)
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
          :map search-map
          ("n" . meow-visit)
          ("s" . isearch-forward)
          ("r" . isearch-backward)
	  :map next-error-repeat-map
	  ("a" . first-error))
  :config
  (meow-setup)
  (meow-global-mode 1))

(use-package meow
  :if (locate-library "ace-window.el")
  :config
  (meow-leader-define-key
   '("w" . ace-window)))

(use-package meow
  :if (locate-library "popper.el")
  :config
  (meow-leader-define-key
   '("e" . popper-toggle)))

(use-package cur-meow
  :after (meow)
  :bind ( :map cur/sub-leader-keymap
          ("C-n" . cur-meow-toggle-temp-normal-motion)))

(use-package meow-tree-sitter
  :after meow
  :config
  (meow-tree-sitter-register-defaults))

(provide 'cur-config-bindings)
