;;; cur-gruvbox-theme.el --- My custom gruvbox theme

;;; Commentary:

;;; Code:

(require 'cur-theme)

;; (add-to-list 'custom-theme-load-path (locate-user-emacs-file "/home/recur/cfg/home/programs/emacs/themes"))
;; (progn (mapc #'disable-theme custom-enabled-themes) (load-theme 'cur-gruvbox t))

(deftheme cur-gruvbox
  "Asdf.")

(let* (
       ;; Normal colours
       (red        "#cc241d")
       (green      "#98971a")
       (yellow     "#d79921")
       (blue       "#458588")
       (purple     "#b16286")
       (aqua       "#689d6a")
       (orange     "#d65d0e")

       ;; Bright colours
       (light-red        "#fb4934")
       (light-green      "#b8bb26")
       (light-yellow     "#fabd2f")
       (light-blue       "#83a598")
       (light-purple     "#d3869b")
       (light-aqua       "#8ec07c")
       (light-orange     "#fe8019")

       ;; Colored Background
       (bg-red    (cur-theme-blend "#cc241d" "#282828" 0.1)) ; red
       (bg-green  (cur-theme-blend "#b8bb26" "#282828" 0.1)) ; light-green
       (bg-yellow (cur-theme-blend "#fabd2f" "#282828" 0.1)) ; light-yellow
       (bg-blue   (cur-theme-blend "#83a598" "#282828" 0.1)) ; light-blue
       (bg-purple (cur-theme-blend "#d3869b" "#282828" 0.1)) ; light-purple
       (bg-aqua   (cur-theme-blend "#8ec07c" "#282828" 0.1)) ; light-aqua
       (bg-orange (cur-theme-blend "#fe8019" "#282828" 0.1)) ; light-orange

       ;; Pale Colors
       (pale-red    (cur-theme-blend "#cc241d" "#ebdbb2" 0.6))
       (pale-green  (cur-theme-blend "#b8bb26" "#ebdbb2" 0.6))
       (pale-yellow (cur-theme-blend "#fabd2f" "#ebdbb2" 0.6))
       (pale-blue   (cur-theme-blend "#83a598" "#ebdbb2" 0.6))
       (pale-purple (cur-theme-blend "#d3869b" "#ebdbb2" 0.6))
       (pale-aqua   (cur-theme-blend "#8ec07c" "#ebdbb2" 0.6))
       (pale-orange (cur-theme-blend "#fe8019" "#ebdbb2" 0.6))

       ;; Hightlight Fg
       (highlight-fg-red    (cur-theme-darken "#fb4934" 0.15))
       (highlight-fg-green  (cur-theme-darken "#b8bb26" 0.15))
       (highlight-fg-yellow (cur-theme-darken "#fabd2f" 0.15))
       (highlight-fg-blue   (cur-theme-darken "#83a598" 0.15))
       (highlight-fg-purple (cur-theme-darken "#d3869b" 0.15))
       (highlight-fg-aqua   (cur-theme-darken "#8ec07c" 0.15))
       (highlight-fg-orange (cur-theme-darken "#fe8019" 0.15))

       ;; Highlight Bg
       (highlight-bg-red    (cur-theme-blend highlight-fg-red    "#282828" 0.2))
       (highlight-bg-green  (cur-theme-blend highlight-fg-green  "#282828" 0.2))
       (highlight-bg-yellow (cur-theme-blend highlight-fg-yellow "#282828" 0.2))
       (highlight-bg-blue   (cur-theme-blend highlight-fg-blue   "#282828" 0.2))
       (highlight-bg-purple (cur-theme-blend highlight-fg-purple "#282828" 0.2))
       (highlight-bg-aqua   (cur-theme-blend highlight-fg-aqua   "#282828" 0.2))
       (highlight-bg-orange (cur-theme-blend highlight-fg-orange "#282828" 0.2))

       ;; Background colours
       (bg-sp1   "#0d1011")
       (bg0h     "#1d2021") ;; Darkest
       (bg0      "#282828") ;; Default bg
       (bg0s     "#32302f")
       (bg1      "#3c3836")
       (bg2      "#504945")
       (bg3      "#665c54")
       (bg4      "#7c6f64")
       (grey     "#928374") ;; Lightest

       ;; Foreground colours
       (fg4     "#a89984") ;; Darkest
       (fg3     "#bdae93")
       (fg2     "#d5c4a1")
       (fg1     "#ebdbb2") ;; Default fg
       (fg0     "#fbf1c7") ;; Lightest
       )
  (custom-theme-set-faces
   'cur-gruvbox
   `(default             ((t (:foreground ,fg1 :background ,bg0))))
   `(success             ((t (:foreground ,light-green))))
   `(warning             ((t (:foreground ,light-yellow))))
   `(error               ((t (:foreground ,light-red))))
   `(link                ((t (:foreground ,light-aqua :underline t :weight bold))))
   `(link-visited        ((t (:foreground ,aqua :underline t :weight bold))))
   `(cursor              ((t (:background ,fg0))))
   `(fringe              ((t (:distant-foreground ,fg1 :background ,bg0))))
   `(region              ((t (:background ,bg2))))
   `(highlight           ((t (:foreground ,bg-sp1 :distant-foreground ,fg0 :background ,light-yellow))))
   `(hl-line             ((t (:background ,bg1))))
   `(header-line         ((t (:inherit (mode-line)))))
   `(vertical-border     ((t (:foreground ,bg2 :background ,bg2))))
   `(secondary-selection ((t (:background ,bg1))))
   `(query-replace       ((t (:inherit isearch))))
   `(minibuffer-prompt   ((t (:foreground ,light-aqua))))
   `(tooltip             ((t (:foreground ,fg1 :background ,bg0h))))
   `(shadow              ((t (:foreground ,bg4))))
   `(trailing-whitespace ((t (:background ,light-red))))
   `(help-key-binding    ((t (:background ,bg0 :foreground ,light-blue :box (:line-width 1 :color ,bg2) :inherit fixed-pitch))))

   ;; Font Lock Faces

   `(font-lock-builtin-face           ((t (:foreground ,light-orange))))
   `(font-lock-comment-face           ((t (:foreground ,grey :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-doc-face               ((t (:foreground ,fg2))))
   `(font-lock-function-name-face     ((t (:foreground ,light-green))))
   `(font-lock-function-call-face     ((t (:inherit font-lock-function-name-face))))
   `(font-lock-keyword-face           ((t (:foreground ,light-red))))
   `(font-lock-preprocessor-face      ((t (:foreground ,fg1 :weight bold))))
   `(font-lock-string-face            ((t (:foreground ,light-green))))
   `(font-lock-type-face              ((t (:foreground ,light-yellow))))
   `(font-lock-constant-face          ((t (:foreground ,light-purple))))
   `(font-lock-variable-name-face     ((t (:foreground ,light-blue))))
   `(font-lock-warning-face           ((t (:inherit warning))))
   `(font-lock-negation-char-face     ((t (:foreground ,fg1 :weight bold))))
   `(font-lock-number-face            ((t (:foreground ,light-purple))))

   ;; Mode Line

   `(mode-line           ((t (:background ,bg1))))
   `(mode-line-active    ((t (:inherit mode-line))))
   `(mode-line-emphasis  ((t (:foreground ,light-blue :distant-foreground ,bg4))))
   `(mode-line-inactive  ((t (:foreground ,bg4 :background ,bg1))))
   `(mode-line-buffer-id ((t (:weight bold))))

   ;; Line Number

   `(line-number              ((t (:foreground ,bg4 :background ,bg0 :inherit default :slant italic :weight normal))))
   `(line-number-current-line ((t (:foreground ,light-yellow :background ,bg1 :inherit (hl-line default) :slant italic :weight normal))))

   ;; Isearch

   `(isearch         ((t (:foreground ,bg0h :background ,light-orange))))
   `(isearch-fail    ((t (:foreground ,fg0 :background ,light-red))))
   `(isearch-group-1 ((t (:foreground ,bg0h :background ,light-orange))))
   `(isearch-group-2 ((t (:foreground ,bg0h :background ,orange))))
   `(lazy-highlight  ((t (:foreground ,bg0h :background ,light-yellow))))

   ;; Dired

   `(dired-broken-symlink ((t (:foreground ,light-yellow :background ,light-red))))
   `(dired-directory ((t (:foreground ,light-blue))))
   `(dired-flagged ((t (:foreground ,light-red :bold t))))
   `(dired-header ((t (:foreground ,light-green :bold t))))
   `(dired-special ((t (:foreground ,light-orange))))
   `(dired-symlink ((t (:foreground ,light-aqua))))
   `(dired-mark ((t (:foreground ,light-orange :background ,bg0 :inverse t :bold t))))
   `(dired-marked ((t (:foreground ,light-yellow :inherit dired-mark))))

;;; Minibuffer and Completions

   ;; Marginalia

   `(marginalia-file-priv-dir ((t (:foreground ,light-blue))))
   `(marginalia-file-priv-exec ((t (:foreground ,light-green))))
   `(marginalia-file-priv-link ((t (:foreground ,light-aqua))))
   `(marginalia-file-priv-other ((t (:foreground ,light-orange))))
   `(marginalia-file-priv-rare ((t (:foreground ,light-orange))))
   `(marginalia-file-priv-read ((t (:foreground ,light-yellow))))
   `(marginalia-file-priv-write ((t (:foreground ,light-red))))

   ;; Match and Completions

   `(match                   ((t (:foreground ,pale-orange :background ,bg-orange))))
   `(completions-common-part ((t (:foreground ,pale-aqua :background ,bg-aqua))))

   ;; Orderless

   `(orderless-match-face-0 ((t (:foreground ,pale-blue :background ,bg-blue))))
   `(orderless-match-face-1 ((t (:foreground ,pale-red :background ,bg-red))))
   `(orderless-match-face-2 ((t (:foreground ,pale-green :background ,bg-green))))
   `(orderless-match-face-3 ((t (:foreground ,pale-yellow :background ,bg-yellow))))

   ;; Vertico

   `(vertico-current         ((t (:background ,bg2))))
   `(vertico-group-separator ((t (:strike-through t :inherit (shadow)))))
   `(vertico-group-title     ((t (:slant italic :inherit (shadow)))))
   `(vertico-multiline       ((t (:inherit (shadow)))))

   ;; Vertico Posframe

   `(vertico-posframe                 ((t (:inherit (default)))))
   `(vertico-posframe-border          ((t (:background ,grey))))
   `(vertico-posframe-border-2        ((t (:background ,light-red))))
   `(vertico-posframe-border-3        ((t (:background ,light-green))))
   `(vertico-posframe-border-4        ((t (:background ,light-blue))))
   `(vertico-posframe-border-fallback ((t (:background ,light-yellow))))

   ;; Which Key
   `(which-key-key-face ((t (:foreground ,light-green))))
   `(which-key-separator-face ((t (:slant normal :inherit (font-lock-comment-face)))))
   `(which-key-command-description-face ((t (:foreground ,light-blue))))

;;; Terminal and Shell Colors

   ;; Ansi Colors

   `(ansi-color-black          ((t (:foreground ,bg0h :background ,bg0h))))
   `(ansi-color-red            ((t (:foreground ,light-red :background ,light-red))))
   `(ansi-color-green          ((t (:foreground ,light-green :background ,light-green))))
   `(ansi-color-yellow         ((t (:foreground ,light-yellow :background ,light-yellow))))
   `(ansi-color-blue           ((t (:foreground ,light-blue :background ,light-blue))))
   `(ansi-color-magenta        ((t (:foreground ,light-purple :background ,light-purple))))
   `(ansi-color-cyan           ((t (:foreground ,light-aqua :background ,light-aqua))))
   `(ansi-color-white          ((t (:foreground ,fg4 :background ,fg4))))
   `(ansi-color-bright-black   ((t (:foreground ,grey :background ,grey))))
   `(ansi-color-bright-red     ((t (:foreground ,light-red :background ,light-red))))
   `(ansi-color-bright-green   ((t (:foreground ,light-green :background ,light-green))))
   `(ansi-color-bright-yellow  ((t (:foreground ,light-yellow :background ,light-yellow))))
   `(ansi-color-bright-blue    ((t (:foreground ,light-blue :background ,light-blue))))
   `(ansi-color-bright-magenta ((t (:foreground ,light-purple :background ,light-purple))))
   `(ansi-color-bright-cyan    ((t (:foreground ,light-aqua :background ,light-aqua))))
   `(ansi-color-bright-white   ((t (:foreground ,fg1 :background ,fg1))))

;;; Programming Auxiliary

   ;; Git Commit
   ;; `(git-commit-comment-action        ((t ())))
   `(git-commit-comment-branch-local  ((t (:foreground ,light-purple))))
   `(git-commit-comment-branch-remote ((t (:foreground ,light-green))))
   `(git-commit-comment-detached      ((t (:foreground ,light-orange))))
   `(git-commit-comment-file          ((t (:foreground ,light-purple))))
   `(git-commit-comment-heading       ((t (:foreground ,light-red))))
   `(git-commit-keyword               ((t (:foreground ,light-aqua :slant italic))))
   `(git-commit-nonempty-second-line  ((t (:inherit git-commit-overlong-summary))))
   `(git-commit-overlong-summary      ((t (:background ,bg-sp1 :slant italic :weight bold :inherit error))))
   `(git-commit-summary               ((t (:foreground ,light-green))))
   ;; `(git-commit-trailer-token         ((t ())))
   ;; `(git-commit-trailer-value         ((t ())))

   ;; Paren Mode

   `(show-paren-match            ((t (:background ,bg4))))
   `(show-paren-match-expression ((t (:inherit show-paren-match))))
   `(show-paren-mismatch         ((t (:foreground ,fg0 :background ,light-red :weight bold))))

   ;; Rainbow Delimiters

   `(rainbow-delimiters-base-face       ((t (:inherit default))))
   `(rainbow-delimiters-base-error-face ((t (:foreground ,light-red :background ,bg0h))))
   `(rainbow-delimiters-unmatched-face  ((t (:foreground ,bg0h :background ,light-red :weight bold))))
   `(rainbow-delimiters-mismatched-face ((t (:inherit rainbow-delimiters-base-unmatched-face))))
   `(rainbow-delimiters-depth-1-face    ((t (:foreground ,light-orange))))
   `(rainbow-delimiters-depth-2-face    ((t (:foreground ,light-red))))
   `(rainbow-delimiters-depth-3-face    ((t (:foreground ,light-green))))
   `(rainbow-delimiters-depth-4-face    ((t (:foreground ,light-blue))))
   `(rainbow-delimiters-depth-5-face    ((t (:foreground ,light-aqua))))
   `(rainbow-delimiters-depth-6-face    ((t (:foreground ,light-purple))))
   `(rainbow-delimiters-depth-7-face    ((t (:foreground ,light-orange))))
   `(rainbow-delimiters-depth-8-face    ((t (:foreground ,light-red))))
   `(rainbow-delimiters-depth-9-face    ((t (:foreground ,light-green))))

   ;; Magit

   `(magit-bisect-bad                      ((t (:foreground ,light-red))))
   `(magit-bisect-good                     ((t (:foreground ,light-green))))
   `(magit-bisect-skip                     ((t (:foreground ,light-orange))))
   `(magit-blame-date                      ((t (:foreground ,red))))
   ;; `(magit-blame-dimmed                    ((t ())))
   `(magit-blame-hash                      ((t (:foreground ,light-aqua))))
   `(magit-blame-heading                   ((t (:foreground ,light-orange :background ,bg1))))
   `(magit-blame-highlight                 ((t (:inherit hightlight))))
   ;; `(magit-blame-margin                    ((t ())))
   ;; `(magit-blame-name                      ((t ())))
   ;; `(magit-blame-summary                   ((t ())))
   `(magit-branch-current                  ((t (:foreground ,light-aqua :underline ,light-green))))
   `(magit-branch-local                    ((t (:foreground ,light-aqua))))
   `(magit-branch-remote                   ((t (:foreground ,light-green))))
   ;; `(magit-branch-remote-head              ((t ())))
   ;; `(magit-branch-upstream                 ((t ())))
   ;; `(magit-branch-warning                  ((t ())))
   `(magit-cherry-equivalent               ((t (:foreground ,light-purple))))
   `(magit-cherry-unmatched                ((t (:foreground ,light-aqua))))
   `(magit-diff-added                      ((t (:foreground ,(cur-theme-darken highlight-fg-green 0.2) :background ,(cur-theme-blend highlight-fg-green bg0 0.1) :extend t))))
   `(magit-diff-added-highlight            ((t (:foreground ,highlight-fg-green :background ,highlight-bg-green :weight bold :extend t))))
   `(magit-diff-base                       ((t (:foreground ,(cur-theme-darken highlight-fg-orange 0.2) :background ,(cur-theme-blend highlight-fg-orange bg0 0.1) :extend t))))
   `(magit-diff-base-highlight             ((t (:foreground ,highlight-fg-orange :background ,highlight-bg-orange :weight bold :extend t))))
   ;; `(magit-diff-conflict-heading           ((t ())))
   `(magit-diff-context                    ((t (:foreground ,(cur-theme-darken fg1 0.4) :background ,bg0))))
   `(magit-diff-context-highlight          ((t (:foreground ,fg1 :background ,bg0h))))
   `(magit-diff-file-heading               ((t (:foreground ,fg0 :weight bold :extend t))))
   ;; `(magit-diff-file-heading-highlight     ((t ())))
   ;; `(magit-diff-file-heading-selection     ((t ())))
   `(magit-diff-hunk-heading               ((t (:foreground ,fg1 :background ,bg1))))
   `(magit-diff-hunk-heading-highlight     ((t (:foreground ,fg0 :background ,bg2))))
   ;; `(magit-diff-hunk-heading-selection     ((t ())))
   ;; `(magit-diff-hunk-region                ((t ())))
   ;; `(magit-diff-lines-boundary             ((t ())))
   ;; `(magit-diff-lines-heading              ((t ())))
   ;; `(magit-diff-our                        ((t ())))
   ;; `(magit-diff-our-highlight              ((t ())))
   `(magit-diff-removed                    ((t (:foreground ,(cur-theme-darken highlight-fg-red 0.2) :background ,(cur-theme-blend highlight-fg-red bg0 0.1) :extend t))))
   `(magit-diff-removed-highlight          ((t (:foreground ,highlight-fg-red :background ,highlight-bg-red :weight bold :extend t))))
   ;; `(magit-diff-revision-summary           ((t ())))
   ;; `(magit-diff-revision-summary-highlight ((t ())))
   ;; `(magit-diff-their                      ((t ())))
   ;; `(magit-diff-their-highlight            ((t ())))
   ;; `(magit-diff-whitespace-warning         ((t ())))
   `(magit-diffstat-added                  ((t (:foreground ,highlight-fg-green))))
   `(magit-diffstat-removed                ((t (:foreground ,highlight-fg-red))))
   `(magit-dimmed                          ((t (:foreground ,fg2))))
   `(magit-filename                        ((t (:foreground ,light-purple))))
   `(magit-hash                            ((t (:slant normal :inherit font-lock-comment-face))))
   ;; `(magit-head                            ((t ())))
   `(magit-header-line                     ((t (:background ,blue :foreground ,fg0 :weight bold :box (:line-width 3 :color ,blue)))))
   ;; `(magit-header-line-key                 ((t ())))
   ;; `(magit-header-line-log-select          ((t ())))
   ;; `(magit-keyword                         ((t ())))
   ;; `(magit-keyword-squash                  ((t ())))
   `(magit-log-author                      ((t (:foreground ,light-orange :slant normal))))
   `(magit-log-date                        ((t (:foreground ,light-blue :slant normal))))
   ;; `(magit-log-graph                       ((t ())))
   ;; `(magit-mode-line-process               ((t ())))
   ;; `(magit-mode-line-process-error         ((t ())))
   `(magit-process-ng                      ((t (:inherit error))))
   `(magit-process-ok                      ((t (:inherit success))))
   ;; `(magit-reflog-amend                    ((t ())))
   ;; `(magit-reflog-checkout                 ((t ())))
   ;; `(magit-reflog-cherry-pick              ((t ())))
   ;; `(magit-reflog-commit                   ((t ())))
   ;; `(magit-reflog-merge                    ((t ())))
   ;; `(magit-reflog-other                    ((t ())))
   ;; `(magit-reflog-rebase                   ((t ())))
   ;; `(magit-reflog-remote                   ((t ())))
   ;; `(magit-reflog-reset                    ((t ())))
   ;; `(magit-refname                         ((t ())))
   ;; `(magit-refname-pullreq                 ((t ())))
   ;; `(magit-refname-stash                   ((t ())))
   ;; `(magit-refname-wip                     ((t ())))
   ;; `(magit-section-child-count             ((t ())))
   `(magit-section-heading                 ((t (:foreground ,light-aqua :weight bold :extend t))))
   `(magit-section-heading-selection       ((t (:foreground ,light-orange :weight bold :extend t))))
   `(magit-section-highlight               ((t (:inherit hl-line))))
   `(magit-section-secondary-heading       ((t (:foreground ,light-purple :weight bold :extend t))))
   ;; `(magit-sequence-done                   ((t ())))
   ;; `(magit-sequence-drop                   ((t ())))
   ;; `(magit-sequence-exec                   ((t ())))
   ;; `(magit-sequence-head                   ((t ())))
   ;; `(magit-sequence-onto                   ((t ())))
   ;; `(magit-sequence-part                   ((t ())))
   ;; `(magit-sequence-pick                   ((t ())))
   ;; `(magit-sequence-stop                   ((t ())))
   ;; `(magit-signature-bad                   ((t ())))
   ;; `(magit-signature-error                 ((t ())))
   ;; `(magit-signature-expired               ((t ())))
   ;; `(magit-signature-expired-key           ((t ())))
   ;; `(magit-signature-good                  ((t ())))
   ;; `(magit-signature-revoked               ((t ())))
   ;; `(magit-signature-untrusted             ((t ())))
   ;; `(magit-tag                             ((t ())))

   ;; `(magit-branch-local ((t (:foreground ,light-aqua))))
   ;; `(magit-section-heading ((t (:foreground ,light-aqua :bold t))))

;;; Languages and IDE Packages

   ;; Company
   `(company-tooltip ((t (:inherit 'tooltip))))
   `(company-tooltip-common ((t (:foreground ,light-aqua))))
   `(company-tooltip-common-selection ((t (:foreground ,light-aqua))))
   `(company-preview ((t (:foreground ,grey))))
   `(company-preview-common ((t (:foreground ,light-aqua :background ,bg1))))

   ;; Flycheck
   `(flycheck-error          ((t (:background ,bg1 :underline (:style wave :color ,light-red)))))
   `(flycheck-info           ((t (:background ,bg1 :underline (:style wave :color ,light-blue)))))
   `(flycheck-warning        ((t (:background ,bg1 :underline (:style wave :color ,light-yellow)))))
   `(flycheck-fringe-error   ((t (:inherit fringe :foreground ,light-red))))
   `(flycheck-fringe-info    ((t (:inherit fringe :foreground ,light-yellow))))
   `(flycheck-fringe-warning ((t (:inherit fringe :foreground ,light-green))))

   ;; Flyspell
   `(flyspell-duplicate ((t (:underline (:style wave :color ,light-yellow)))))
   `(flyspell-incorrect ((t (:underline (:style wave :color ,light-red)))))

   ;; Lsp-mode
   `(lsp-face-highlight-read ((t (:inherit lsp-face-highlight-textual))))
   `(lsp-face-highlight-textual ((t (:foreground ,fg1 :background ,bg1 :underline t))))

;;; Internet Modes

   ;; Elpher

   `(elpher-binary                     ((t (:inherit (elpher-unknown)))))
   `(elpher-gemini                     ((t (:foreground ,light-aqua :weight normal :inherit (link)))))
   `(elpher-gemini-heading1            ((t (:foreground ,light-red :height 1.8 :inherit (default)))))
   `(elpher-gemini-heading2            ((t (:foreground ,light-yellow :height 1.4 :inherit (default)))))
   `(elpher-gemini-heading3            ((t (:foreground ,light-orange :height 1.2 :inherit (default)))))
   `(elpher-gemini-preformatted        ((t (:foreground ,grey :inherit (fixed-pitch)))))
   `(elpher-gemini-preformatted-toggle ((t (:inherit (button)))))
   `(elpher-gemini-quoted              ((t (:inherit (italic)))))
   `(elpher-html                       ((t (:inherit (elpher-other-url)))))
   `(elpher-image                      ((t (:inherit (elpher-other-url)))))
   `(elpher-index                      ((t (:foreground ,light-red))))
   `(elpher-info                       ((t (:inherit (elpher-text)))))
   `(elpher-margin-brackets            ((t (:inherit (bold)))))
   `(elpher-margin-key                 ((t (:inherit (shadow)))))
   `(elpher-other-url                  ((t (:foreground ,bg4 :slant italic))))
   `(elpher-search                     ((t (:foreground ,light-yellow))))
   `(elpher-telnet                     ((t (:inherit (elpher-unknown)))))
   `(elpher-text                       ((t (:inherit (bold)))))
   `(elpher-unknown                    ((t (:foreground ,bg4))))

   ;; Gemini

   `(gemini-heading-face-1    ((t (:foreground ,light-red :height 1.8 :inherit (default)))))
   `(gemini-heading-face-2    ((t (:foreground ,light-yellow :height 1.4 :inherit (default)))))
   `(gemini-heading-face-3    ((t (:foreground ,light-orange :height 1.2 :inherit (default)))))
   `(gemini-heading-face-rest ((t (:inherit (bold)))))
   `(gemini-preformatted-face ((t (:foreground ,grey :inherit (fixed-pitch)))))
   `(gemini-quote-face        ((t (:inherit (italic)))))
   `(gemini-ulist-face        ((t (:inherit (default)))))

;;; Org

   ;; Org General
   `(org-archived                 ((t (:foreground ,bg4))))
   `(org-block                    ((t (:inherit default))))
   `(org-block-begin-line         ((t (:foreground ,grey :background ,bg1 :extend t))))
   `(org-block-end-line           ((t (:inherit (org-block-begin-line)))))
   `(org-code                     ((t (:foreground ,light-orange :inherit org-block))))
   `(org-checkbox-statistics-done ((t (:inherit org-done))))
   `(org-checkbox-statistics-todo ((t (:inherit org-todo))))
   `(org-checkbox                 ((t (:inherit org-todo))))
   `(org-date                     ((t (:foreground ,light-green))))
   `(org-drawer                   ((t (:foreground ,(cur-theme-lighten light-aqua 0.4)))))
   `(org-document-info            ((t (:foreground ,light-red :bold t))))
   `(org-document-title           ((t (:foreground ,light-red))))
   `(org-done                     ((t (:inherit org-headline-done :weight bold))))
   `(org-ellipsis                 ((t (:foreground ,light-orange))))
   `(org-footnote                 ((t (:foreground ,light-orange))))
   `(org-formula                  ((t (:foreground ,light-green))))
   `(org-headline-done            ((t (:foreground ,bg4))))
   `(org-headline-todo            ((t (:foreground ,light-green))))
   `(org-link                     ((t (:foreground ,light-yellow :inherit link))))
   `(org-list-dt                  ((t (:foreground ,light-aqua))))
   `(org-meta-line                ((t (:foreground ,grey))))
   `(org-priority                 ((t (:foreground ,light-red))))
   `(org-property-value           ((t (:inherit default))))
   `(org-quote                    ((t (:foreground ,fg2  :inherit (italic org-block)))))
   `(org-special-keyword          ((t (:foreground ,(cur-theme-lighten light-aqua 0.15)))))
   `(org-table                    ((t (:foreground ,light-aqua))))
   `(org-tag                      ((t (:foreground ,bg4 :weight normal))))
   `(org-todo                     ((t (:inherit org-headline-todo :weight bold))))
   `(org-verbatim                 ((t (:foreground ,light-yellow))))

   ;; Outline N and Org Heading Levels
   `(outline-1 ((t (:foreground ,light-purple :weight bold :extend t))))
   `(outline-2 ((t (:foreground ,light-aqua :weight bold :extend t))))
   `(outline-3 ((t (:foreground ,light-green :weight bold :extend t))))
   `(outline-4 ((t (:foreground ,(cur-theme-lighten light-purple 0.2) :weight bold :extend t))))
   `(outline-5 ((t (:foreground ,(cur-theme-lighten aqua 0.25) :weight bold :extend t))))
   `(outline-6 ((t (:foreground ,(cur-theme-lighten light-purple 0.4) :weight bold :extend t))))
   `(outline-7 ((t (:foreground ,(cur-theme-lighten aqua 0.5) :weight bold :extend t))))
   `(outline-8 ((t (:foreground ,(cur-theme-lighten light-aqua 0.6) :weight bold :extend t))))

   `(nerd-icons-blue       ((t (:foreground ,light-blue))))
   `(nerd-icons-blue-alt   ((t (:foreground ,light-aqua))))
   `(nerd-icons-cyan       ((t (:foreground ,light-aqua))))
   `(nerd-icons-cyan-alt   ((t (:foreground ,light-aqua))))
   `(nerd-icons-dblue      ((t (:foreground ,blue))))
   `(nerd-icons-dcyan      ((t (:foreground ,aqua))))
   `(nerd-icons-dgreen     ((t (:foreground ,(cur-theme-darken light-green 0.3)))))
   `(nerd-icons-dmaroon    ((t (:foreground ,(cur-theme-darken red 0.3)))))
   `(nerd-icons-dorange    ((t (:foreground ,(cur-theme-darken light-orange 0.3)))))
   `(nerd-icons-dpink      ((t (:foreground ,(cur-theme-lighten light-red 0.15)))))
   `(nerd-icons-dpurple    ((t (:foreground ,(cur-theme-darken light-purple 0.3)))))
   `(nerd-icons-dred       ((t (:foreground ,(cur-theme-darken light-red 0.3)))))
   `(nerd-icons-dsilver    ((t (:foreground ,(cur-theme-lighten grey 0.1)))))
   `(nerd-icons-dyellow    ((t (:foreground ,(cur-theme-darken yellow 0.3)))))
   `(nerd-icons-green      ((t (:foreground ,light-green))))
   `(nerd-icons-lblue      ((t (:foreground ,(cur-theme-lighten light-blue 0.3)))))
   `(nerd-icons-lcyan      ((t (:foreground ,(cur-theme-lighten light-aqua 0.3)))))
   `(nerd-icons-lgreen     ((t (:foreground ,(cur-theme-lighten light-green 0.3)))))
   `(nerd-icons-lmaroon    ((t (:foreground ,(cur-theme-lighten red 0.3)))))
   `(nerd-icons-lorange    ((t (:foreground ,(cur-theme-lighten light-orange 0.3)))))
   `(nerd-icons-lpink      ((t (:foreground ,(cur-theme-lighten light-red 0.55)))))
   `(nerd-icons-lpurple    ((t (:foreground ,(cur-theme-lighten light-purple 0.3)))))
   `(nerd-icons-lred       ((t (:foreground ,(cur-theme-lighten light-red 0.3)))))
   `(nerd-icons-lsilver    ((t (:foreground ,(cur-theme-lighten grey 0.7)))))
   `(nerd-icons-lyellow    ((t (:foreground ,(cur-theme-lighten yellow 0.3)))))
   `(nerd-icons-maroon     ((t (:foreground ,red))))
   `(nerd-icons-orange     ((t (:foreground ,light-orange))))
   `(nerd-icons-pink       ((t (:foreground ,(cur-theme-lighten light-red 0.35)))))
   `(nerd-icons-purple     ((t (:foreground ,light-purple))))
   `(nerd-icons-purple-alt ((t (:foreground ,(cur-theme-blend light-purple grey 0.15)))))
   `(nerd-icons-red        ((t (:foreground ,light-red))))
   `(nerd-icons-red-alt    ((t (:foreground ,(cur-theme-blend light-red grey 0.15)))))
   `(nerd-icons-silver     ((t (:foreground ,(cur-theme-lighten grey 0.45)))))
   `(nerd-icons-yellow     ((t (:foreground ,yellow))))
   )
  )

(provide-theme 'cur-gruvbox)
;;; cur-gruvbox-theme.el ends here
