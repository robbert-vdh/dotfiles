;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(load! "+bindings")

;; TODO: Use the new `:defer input/buffer' directives

(def-package! atomic-chrome
  :config
  (setq atomic-chrome-buffer-open-style 'frame
        atomic-chrome-buffer-frame-height 20
        atomic-chrome-buffer-frame-width 90
        atomic-chrome-default-major-mode 'markdown-mode)
  (ignore-errors (atomic-chrome-start-server)))

(def-package! evil-lion
  :after evil
  :config (evil-lion-mode))

(def-package! fish-mode
  :config
  (set! :electric '(fish-mode) :words '("else" "end")))

(def-package! ggtags
  :commands (ggtags-find-tag-dwim ggtags-find-reference)
  :commands (ggtags-mode)
  :config
  ;; Sort global results by nearness. This helps when editing Sass, as the
  ;; default variables will have a lower priority.
  (setq ggtags-sort-by-nearness t)
  ;; Fix gtags for Sass. Pygments has got a parser that works great, but it
  ;; doesn't use the dollar sign prefix. We'll have to manually add the jump
  ;; handler to scss-mode as there are not any yet.
  (add-hook! 'scss-mode-hook (modify-syntax-entry ?$ "'") (modify-syntax-entry ?% "."))
  (set! :lookup 'scss-mode :definition #'ggtags-find-tag-dwim :references #'ggtags-find-reference)
  (set! :company-backend '(css-mode scss-mode) 'company-gtags 'company-css))

;; Transforms ^L characters into horizontal lines
(def-package! page-break-lines
  :config
  (add-hook! (emacs-lisp-mode view-mode) 'turn-on-page-break-lines-mode))

;; The standard should be set either through a configuration file or globally
;; using `phpcs --config-set default_standard psr2' to ensure that flymake and
;; phpcbf use the same standard
(def-package! phpcbf)

(def-package! pkgbuild-mode)

(def-package! yapfify
  :hook (python-mode . yapf-mode))

;;; Overrides

;; `counsel-projectile-rg' doesn't get autoloaded in the default config
(autoload 'counsel-projectile-rg "counsel-projectile" nil t)

(after! company-box
  ;; Fix icons (all-the-icons is not loaded in time)
  (add-hook! 'company-box-mode-hook
    ;; FIXME: The window does not scale correctly when the icons are larger than
    ;;        the text
    (let ((all-the-icons-scale-factor 0.95))
      (setq company-box-backends-colors nil
            company-box-icons-elisp
            (list (all-the-icons-material "functions" :face 'all-the-icons-purple)
                  (all-the-icons-material "check_circle" :face 'all-the-icons-blue)
                  (all-the-icons-material "stars" :face 'all-the-icons-yellow)
                  (all-the-icons-material "format_paint" :face 'all-the-icons-pink))
            company-box-icons-unknown (all-the-icons-material "find_in_page" :face 'all-the-icons-silver)
            company-box-icons-yasnippet nil)))

  (advice-add #'company-box--next-line :override #'+robbert/company-box-next-line)
  (advice-add #'company-box--prev-line :override #'+robbert/company-box-prev-line)
  (advice-add #'company-box--change-line :after #'+robbert--company-box-fix-tng)
  (advice-add #'company-box--render-buffer :after #'+robbert--company-box-fix-tng))

(after! csharp-mode
  (set! :electric '(csharp-mode) :chars '(?\n ?\{)))

(after! ein
  (setq ein:jupyter-default-notebook-directory nil
        ein:slice-image '(10 nil)))

(after! evil
  (setq-default evil-symbol-word-search t)

  ;; Automatically indent when pasting
  (dolist (func '(evil-paste-before evil-paste-after))
    (advice-add func :around '+robbert--indent-paste-advise))

  ;; Doom Emacs overrides the `;' and `,' keys to also repeat things like
  ;; searches. Because it uses evil-snipe by default this hasn't been done for
  ;; the default f/F/t/T keybindings.
  (do-repeat! evil-find-char evil-repeat-find-char evil-repeat-find-char-reverse)
  (do-repeat! evil-find-char-backward evil-repeat-find-char evil-repeat-find-char-reverse)
  (do-repeat! evil-find-char-to evil-repeat-find-char evil-repeat-find-char-reverse)
  (do-repeat! evil-find-char-to-backward evil-repeat-find-char evil-repeat-find-char-reverse))

(after! evil-collection
  ;; FIXME: This setup breaks commiting in magit
  (after! diff-mode
    (remove-hook 'diff-mode-hook 'evil-collection-diff-toggle-setup))

  ;; Make sure neotree keybindings are loaded, sometimes they are not
  (after! neotree
    (evil-collection-neotree-setup)))

(after! evil-ediff
  ;; Ancestor is already shown in buffer C
  (setq ediff-show-ancestor nil)
  ;; Allow easy navigation between windows
  (push '("\C-h" . evil-window-left)  evil-ediff-bindings)
  (push '("\C-j" . evil-window-down)  evil-ediff-bindings)
  (push '("\C-k" . evil-window-up)    evil-ediff-bindings)
  (push '("\C-l" . evil-window-right) evil-ediff-bindings))

(after! evil-snipe
  ;; Disable evil-snipe overriding the S/s keys. This is a bit of a hack but the
  ;; clean way to disable it doesn't work right now.
  (evil-snipe-override-mode -1)
  (evil-snipe-mode -1))

(after! evil-org
  (setq evil-org-use-additional-insert t)
  (add-to-list 'additional 'evil-org-key-theme)
  (evil-org-set-key-theme))

(after! evil-magit
  (remove-hook 'git-commit-mode-hook #'evil-insert-state)
  ;; git-commit is always verbose as specified in ~/.gitconfig
  (setq magit-commit-show-diff nil
        magit-diff-refine-hunk 'all)
  ;; Use the traditional window splitting behaviour (I think it's cleaner)
  (setq magit-display-buffer-function 'magit-display-buffer-traditional)
  ;; Don't interfere with the leader key
  (dolist (mode (list magit-mode-map magit-revision-mode-map))
    (define-key mode (kbd doom-leader-key) nil)))

(after! evil-surround
  ;; Add evil-surround support for common markup symbols
  (dolist (pair '((?$ . ("$" . "$")) (?= . ("=" . "=")) (?~ . ("~" . "~"))
                  (?/ . ("/" . "/")) (?* . ("*" . "*")) (?* . (":" . ":"))))
    (push pair evil-surround-pairs-alist)))

(after! flx
  ;; Prefer more freeform regexes when fuzzy search isn't active
  (setq ivy-re-builders-alist '((counsel-ag . ivy--regex-ignore-order)
                                (counsel-rg . ivy--regex-ignore-order)
                                (counsel-pt . ivy--regex-ignore-order)
                                (counsel-grep . ivy--regex-ignore-order)
                                (counsel-grep-or-swiper . ivy--regex-ignore-order)
                                (swiper . ivy--regex-ignore-order)
                                (t . ivy--regex-fuzzy))))

(after! flycheck
  (set! :evil-state 'flycheck-error-list-mode 'normal))

(after! flyspell
  ;; Don't automatically spellcheck when enabling flycheck
  (add-hook 'text-mode-hook 'flyspell-mode)
  (setq-default +spellcheck-immediately nil))

(after! helpful
  ;; Increase the size of help popups to match Ivy's height
  (set! :popup "^\\*Help" '((size . 0.3)) '((select . t))))

(after! hl-todo
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("FIXME" . ,(face-foreground 'error))
          ("XXX"   . ,(face-foreground 'error))
          ("HACK"  . ,(face-foreground 'error))
          ("NOTE"  . ,(face-foreground 'success)))))

(after! langtool
  (setq langtool-disabled-rules '("WHITESPACE_RULE")
        langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*"))

(after! python
  ;; Set this to `django' to force docstring to always be on multiple lines
  (setq python-fill-docstring-style 'pep-257))

(after! markdown-mode
  ;; Doom adds extra line spacing in markdown documents
  (add-hook! :append markdown-mode (setq line-spacing nil)))

(after! multi-term
  ;; Term-mode only allows binding new keys using an alist
  (dolist
      (bind '(("C-c C-l" . +robbert/term-toggle-line-mode)
              ("C-c C-z" . term-send-raw)
              ("C-c b"   . +robbert/switch-terminal-buffer)
              ("C-c j"   . multi-term-next)
              ("C-c k"   . multi-term-prev)
              ("C-c n"   . multi-term)))
    (add-to-list 'term-bind-key-alist bind)))

(after! omnisharp
  ;; Use a more modern omnisharp server than the package specifies
  (when (equal omnisharp-expected-server-version "1.26.3")
    (setq omnisharp-expected-server-version "1.30.1"))

  ;; Killing the omnisharp server doesn't work as well when constantly switching
  ;; branches and previewing files
  (add-hook! :append csharp-mode
    (remove-hook 'kill-buffer-hook #'omnisharp-stop-server t)))

(after! org
  ;; Restore default indentation behavior
  (remove-hook! 'org-mode-hook '(org-indent-mode visual-line-mode))
  (setq org-adapt-indentation t
        org-startup-indented nil)

  (setq org-export-with-smart-quotes t
        org-imenu-depth 3
        org-highlight-latex-and-related '(latex script entities))
  (set-face-attribute
   'org-todo nil :foreground (doom-darken (face-foreground 'org-todo) 0.2))

  ;; Org mode should use komascript for LaTeX exports and code fragments should be colored
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
                 '("koma-article"
                   "\\documentclass[parskip=half]{scrartcl}
                    [DEFAULT-PACKAGES] [PACKAGES]
                    \\setminted{frame=leftline,framesep=1em,linenos,numbersep=1em,style=pastie}
                    \\setminted[python]{python3}
                    [EXTRA]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-packages-alist '("dutch" "babel"))
    (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))
    (setq org-latex-default-class "koma-article"
          org-format-latex-options
          (plist-put org-format-latex-options
                     :scale (if (equal system-name "laptop") 1.5 1.25))
          org-latex-caption-above nil
          org-latex-listings 'minted
          ;; latexmk tends to play along nicer than pdflatex
          org-latex-pdf-process '("latexmk -f -pdf %f"))))

(after! ox-pandoc
  ;; Doom explicitely adds the deprecated `parse-raw' option
  (setq org-pandoc-options '((standalone . t) (mathjax . t))))

(after! racer
  ;; Tell racer where to find library sources
  (setq racer-rust-src-path "~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"))

(after! rust-mode
  ;; Add missing confugration
  (setq rust-format-on-save t)
  (set! :electric '(rust-mode) :chars '(?\n ?\})))

(after! tide
  ;; Use the built in tsserver as formatting breaks otherwise
  (setq tide-tsserver-locator-function 'ignore)

  ;; Format TypeScript on save
  (add-hook! 'typescript-mode-hook
    (add-hook! :local 'before-save-hook 'tide-format-before-save)))

(after! web-mode
  ;; Fix web-mode and smartparens conflicts
  ;; (sp-pair "%" "%" :wrap "C-%")
  ;; (sp-pair "<" ">" :wrap "C->")

  (setq web-mode-enable-auto-pairing nil)

  ;; Editorconfig tells web-mode to indent attributes instead of aligning
  (add-hook! :append 'web-mode-hook
    (setq web-mode-attr-indent-offset nil
          web-mode-attr-value-indent-offset nil
          web-mode-block-padding 0)))

(after! wordnut
  (set! :popup "^\\*WordNut\\*$" '((size . 0.3)) '((select . t))))

(after! yasnippet
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand))

;;; Settings

(setq company-minimum-prefix-length 2
      completion-styles '(partial-completion initials)
      confirm-nonexistent-file-or-buffer nil
      evil-ex-substitute-global nil
      evil-want-C-u-scroll t
      evil-want-Y-yank-to-eol nil
      executable-prefix-env t
      ;; Order should not matter when searching
      ;; TODO: See whether flx works well, then either delete or reenable
      ;; ivy-re-builders-alist '(;; (swiper . ivy--regex-plus)
      ;;                         (t      . ivy--regex-ignore-order))
      flyspell-default-dictionary "english"
      nav-flash-delay 0.25
      show-trailing-whitespace t
      which-key-idle-delay 0.4

      +org-dir (expand-file-name "~/Documenten/notes/")
      doom-line-numbers-style 'relative
      doom-font (font-spec :family "Input Mono"
                           :width 'semi-condensed
                           :size (if (equal system-name "laptop") 18 16))
      doom-variable-pitch-font (font-spec :family "Roboto"
                                          :size (if (equal system-name "laptop") 18 16))
      doom-big-font (font-spec :family "Input Mono" :size 29))

;; Remove less useful functions from hippie expand
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-line
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Fix $PATH now that Doom doesn't include `exec-path-from-shell' anymore
(add-to-list 'exec-path "~/.local/bin")
(add-to-list 'exec-path "~/.cargo/bin")

;; Fix xdg-open and similar progrems not openening. Not sure why this is needed,
;; and might break others things.
(setq process-connection-type nil)
(setq-hook! 'term-mode-hook process-connection-type t)

;; Disable blinking
(add-hook! :append 'doom-init-ui-hook
  (blink-cursor-mode -1))

;; Trailing whitespace is not important when working with the minibuffer
(setq-hook! minibuffer-setup show-trailing-whitespace nil)

;; The smerge hydra is not always needed
(remove-hook 'find-file-hook '+vcs|enable-smerge-mode-maybe)

;; Automatically delete trailing whitespace
(add-hook! (prog-mode text-mode)
  (add-hook! :local 'before-save-hook #'delete-trailing-whitespace))

;; Make `w' and `b' handle more like in vim
(add-hook 'after-change-major-mode-hook #'+robbert/fix-evil-words-underscore)

;; Always highlight numbers as it looks pretty
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

;; Set default indentation levels and coding styles
(setq css-indent-offset 2
      js-indent-level 2
      sh-basic-offset 2
      sh-indentation 2
      typescript-indent-level 2
      web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-comment-style 2
      php-mode-coding-style 'psr2)

;; Different languages use different line lengths (there's probably a better
;; way to keep the variable value in the lambda)
(cl-loop for (mode . value) in '((php-mode-hook . 120)
                                 (python-mode-hook . 79)
                                 (rust-mode-hook . 100))
         do (add-hook mode `(lambda () (setq fill-column ,value))))

;; Add missing syntax highlighting
(add-to-list 'auto-mode-alist '("\\.csproj$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.ruleset$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.service$" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.socket$" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.target$" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.timer$" . conf-unix-mode))

;; Use ggtags in certain modes
(add-hook 'scss-mode-hook #'ggtags-mode)

;; Fix jumping to Sass files when the leading underscore is ommitted
;; TODO: Refactor this to use the new `+lookup/file' function
(require 'ffap)
(add-to-list 'ffap-alist '(scss-mode . +robbert/scss-find-file))

;; Flycheck popup tweaks
(setq flycheck-pos-tip-timeout 15)

;; Improve scrolling behaviour by recentering the screen when jumping to
;; something off screen
(setq mouse-wheel-scroll-amount '(1 ((shift) . 3) ((control)))
      scroll-conservatively 3
      scroll-margin 3
      maximum-scroll-margin 0.2)
(setq-hook! 'term-mode-hook scroll-margin 0)

;; Also highlight todos in text modes
(add-hook 'text-mode-hook #'hl-todo-mode)

;; Auto reload PDFs
(add-hook 'doc-view-mode-hook #'auto-revert-mode)

;; Enable vim-style auto completion
(require 'company-tng)
(company-tng-configure-default)
(setq company-frontends
      '(company-preview-if-just-one-frontend
        company-tng-frontend
        company-pseudo-tooltip-frontend))

;; Explicitely load evil-surround so that extra pairs can be loaded in time
(require 'evil-surround)

;; Doom Emacs doesn't play along nicely with the noninteractive Emacs daemon,
;; but server functinoaly is still very useful
(require 'server)
(unless (server-running-p)
  (server-start))
