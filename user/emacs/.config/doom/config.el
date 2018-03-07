;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(require 'cl)

(load! +bindings)

;; TODO: SPC / * to search for the word under the cursor

(def-package! atomic-chrome
  :config
  (setq atomic-chrome-buffer-open-style 'frame
        atomic-chrome-buffer-frame-height 25
        atomic-chrome-buffer-frame-width 85
        atomic-chrome-default-major-mode 'markdown-mode))

(def-package! evil-ediff
  :after ediff
  :config
  ;; Ancestor is already shown in buffer C
  (setq ediff-show-ancestor nil)
  ;; Allow easy navigation between windows
  (push '("\C-h" . evil-window-left)  evil-ediff-bindings)
  (push '("\C-j" . evil-window-down)  evil-ediff-bindings)
  (push '("\C-k" . evil-window-up)    evil-ediff-bindings)
  (push '("\C-l" . evil-window-right) evil-ediff-bindings))

(def-package! evil-lion
  :after evil
  :config (evil-lion-mode))

(def-package! evil-magit
  :after magit
  :config
  ;; git-commit is always verbose as specified in ~/.gitconfig
  (setq magit-commit-show-diff nil
        magit-diff-refine-hunk 'all)
  (remove-hook 'git-commit-mode-hook #'evil-insert-state)
  ;; Doom Emacs disables evil in `magit-blame' by default
  (add-hook 'magit-blame-mode-hook #'evil-local-mode t)
  ;; Don't interfere with the leader key
  (define-key magit-mode-map (kbd doom-leader-key) nil)
  ;; evil-vimish-fold overrides evil-magit's `z' keys and it's not useful anywya
  ;; in here, so we'll disable it
  (add-hook! 'magit-mode-hook (evil-vimish-fold-mode -1)))

(after! evil-org
  (setq evil-org-use-additional-insert t)
  (evil-org-set-key-theme))

(def-package! ggtags
  :commands (ggtags-find-tag-dwim ggtags-find-reference)
  :config
  (add-hook 'ggtags-mode-hook #'eldoc-mode)
  ;; Sort global results by nearness. This helps when editing Sass, as the
  ;; default variables will have a lower priority.
  (setq ggtags-sort-by-nearness t)
  ;; Fix gtags for Sass. Pygments has got a parser that works great, but it
  ;; doesn't use the dollar sign prefix. We'll have to manually add the jump
  ;; handler to scss-mode as there are not any yet.
  (add-hook! 'scss-mode-hook (modify-syntax-entry ?$ "'") (modify-syntax-entry ?% "."))
  (add-hook 'scss-mode-hook #'ggtags-mode)
  (set! :lookup 'scss-mode :definition #'ggtags-find-tag-dwim :references #'ggtags-find-reference)
  (set! :company-backend '(css-mode scss-mode) 'company-gtags 'company-css))

;;; Overrides

;; `counsel-projectile-rg' doesn't get autoloaded in the default config
(autoload 'counsel-projectile-rg "counsel-projectile" nil t)

(after! evil
  (setq-default evil-symbol-word-search t)
  ;; Automatically indent when pasting
  (dolist (func '(evil-paste-before evil-paste-after))
    (advice-add func :around '+robbert/indent-paste-advise))
  ;; Doom Emacs overrides the `;' and `,' keys to also repeat things like
  ;; searches. Because it uses evil-snipe by default this hasn't been done for
  ;; the default f/F/t/T keybindings.
  (do-repeat! evil-find-char evil-repeat-find-char evil-repeat-find-char-reverse)
  (do-repeat! evil-find-char-backward evil-repeat-find-char evil-repeat-find-char-reverse)
  (do-repeat! evil-find-char-to evil-repeat-find-char evil-repeat-find-char-reverse)
  (do-repeat! evil-find-char-to-backward evil-repeat-find-char evil-repeat-find-char-reverse))

(after! evil-snipe
  ;; Disable evil-snipe overriding the S/s keys. This is a bit of a hack but the
  ;; clean way to disable it doesn't work right now.
  (defun +robbert/disable-evil-snipe-once ()
    (remove-hook 'evil-snipe-mode-hook '+robbert/disable-evil-snipe-once)
    (evil-snipe-mode -1))
  (evil-snipe-override-mode -1)
  (add-hook 'evil-snipe-mode-hook '+robbert/disable-evil-snipe-once))

(after! evil-surround
  ;; Add evil-surround support for common markup symbols
  (dolist (pair '((?$ . ("$" . "$")) (?= . ("=" . "=")) (?~ . ("~" . "~"))
                  (?/ . ("/" . "/")) (?* . ("*" . "*")) (?* . (":" . ":"))))
    (push pair evil-surround-pairs-alist)))

(after! exec-path-from-shell
  ;; Make sure racer can find Rust's source files and disable gtags as Rust's
  ;; Language Server does a better job already
  (exec-path-from-shell-copy-envs '("LD_LIBRARY_PATH" "RUST_SRC_PATH")))

(after! flycheck
  (set! :evil-state 'flycheck-error-list-mode 'normal))

(after! flyspell
  ;; Don't automatically spellcheck when enabling flycheck
  (remove-hook 'flyspell-mode-hook #'+spellcheck|automatically)
  (add-hook! (git-commit-mode org-mode text-mode)
    'flyspell-mode))

(after! helpful
  (set! :evil-state 'helpful-mode 'motion))

(after! hl-todo
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("FIXME" . ,(face-foreground 'error))
          ("XXX"   . ,(face-foreground 'error))
          ("HACK"  . ,(face-foreground 'error))
          ("NOTE"  . ,(face-foreground 'success)))))

(after! ivy
  ;; Use ivy for minibuffer completion, Doom disables this by default
  (setq ivy-do-completion-in-region t
        completion-in-region-function 'ivy-completion-in-region))

(after! langtool
  (setq langtool-disabled-rules '("WHITESPACE_RULE")
        langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*"))

(after! python
  ;; Python docstrings should always be on multiple lines
  (setq python-fill-docstring-style 'django))

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
  ;; FIXME: This should not be necesary as aldoc and smartparens are already
  ;;        enabled for `csharp-mode'
  (add-hook 'omnisharp-mode-hook 'eldoc-mode)

  ;; Use a more modern omnisharp server than the package specifies
  (when (equal omnisharp-expected-server-version "1.26.3")
    (setq omnisharp-expected-server-version "1.29.1"))

  ;; Killing the omnisharp server doesn't work as well when constantly switching
  ;; branches and previewing files
  (advice-add #'+csharp|cleanup-omnisharp-server :override #'ignore))

(after! org
  ;; Restore default indentation behavior
  (remove-hook! 'org-mode-hook '(org-indent-mode visual-line-mode))
  (setq org-adapt-indentation t
        org-startup-indented nil)

  (setq org-imenu-depth 3
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

(after! rust-mode
  ;; Add missing confugration
  (setq rust-format-on-save t)
  (set! :electric '(rust-mode) :chars '(?\n ?\})))

(after! smartparens
  ;; FIXME: Brace expansion sometimes doesn't happen with `RET', but it works
  ;;        with `<return>'
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil :post-handlers '(:add ("||\n[i]" "<return>")))))

(after! tide
  (add-hook! 'typescript-mode-hook
    (add-hook! :local 'before-save-hook 'tide-format-before-save)))

(after! yasnippet
  ;; `~/.emacs/snippets' should come first as it's used as the default snippet
  ;; save location
  (push (expand-file-name "snippets" +private-config-path) yas-snippet-dirs)
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand))

;;; Settings

(setq company-minimum-prefix-length 2
      completion-styles '(partial-completion initials)
      confirm-nonexistent-file-or-buffer nil
      evil-goggles-duration 0.25
      evil-want-C-u-scroll t
      evil-want-Y-yank-to-eol nil
      ;; Order should not matter when searching
      ivy-re-builders-alist '(;; (swiper . ivy--regex-plus)
                              (t      . ivy--regex-ignore-order))
      nav-flash-delay 0.25
      show-trailing-whitespace t
      which-key-idle-delay 0.4

      +org-dir (expand-file-name "~/Documenten/notes/")
      doom-line-numbers-style 'relative
      doom-font (font-spec :family "Input Mono"
                           :width 'semi-condensed
                           :size (if (equal system-name "laptop") 18 16))
      doom-variable-pitch-font (font-spec :family "Roboto")
      doom-big-font (font-spec :family "Input Mono" :size 29))

;; Fix xdg-open and similar progrems not openening. Not sure why this is needed,
;; and might break others things.
(setq process-connection-type nil)
(add-hook! 'term-mode-hook (setq-local process-connection-type t))

;; Disable blinking
(add-hook! :append 'doom-init-ui-hook
  (blink-cursor-mode -1))

;; Trailing whitespace is not important when working with the minibuffer
(add-hook! minibuffer-setup (setq-local show-trailing-whitespace nil))

;; The smerge hydra is not always needed
(remove-hook 'find-file-hook '+vcs|enable-smerge-mode-maybe)

;; Automatically delete trailing whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)

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
(loop for (mode . value) in '((php-mode-hook . 120)
                              (rust-mode-hook . 100))
      do (add-hook mode `(lambda () (setq fill-column ,value))))

;; Add missing syntax highlighting
(add-to-list 'auto-mode-alist '("\\.twig$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.csproj$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.ruleset$" . nxml-mode))

;; Fix jumping to Sass files when the leading underscore is ommitted
(add-to-list 'ffap-alist
             '(scss-mode . (λ! (filename) (replace-regexp-in-string
                                           "^\\(.*?\\)\\([^/]+\\)$"
                                           "\\1_\\2.scss"
                                           filename))))

;; Flycheck popup tweaks
(setq flycheck-pos-tip-timeout 15)

;; Improve scrolling behaviour by recentering the screen when jumping to
;; something off screen
(setq mouse-wheel-scroll-amount '(1 ((shift) . 3) ((control)))
      scroll-conservatively 3
      scroll-margin 3)
(add-hook! 'term-mode-hook (setq-local scroll-margin 0))

;; Also highlight todos in text modes
(add-hook 'text-mode-hook #'hl-todo-mode)

;; Auto reload PDFs
(add-hook 'doc-view-mode-hook #'auto-revert-mode)

;; Enable automatic auto completion.
(+company/toggle-auto-completion)
(require 'company-tng)
(company-tng-configure-default)

;; Explicitely load evil-surround so that extra pairs can be loaded in time
(require 'evil-surround)

;; Doom Emacs doesn't play along nicely with the noninteractive Emacs daemon,
;; but server functinoaly is still very useful
(require 'server)
(unless (server-running-p)
  (server-start)
  (atomic-chrome-start-server))

;; TODO: Remove
(autoload 'evil-vimish-fold-mode "evil-vimish-fold")
