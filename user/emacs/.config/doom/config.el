;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(load! "+bindings")

;; My lalptop has a HiDPI scree nand I prefer everything to be scaled up
;; slightly
(setq +scaling-ratio (pcase (system-name)
                       ("thinkpad" 1.75)
                       (_ 1.2))
      +font-size (pcase (system-name)
                   ("thinkpad" 13.5)
                   (_ 13.0)))

(use-package! academic-phrases)

(use-package! evil-lion
  :after evil
  :config (evil-lion-mode))

(use-package! ggtags
  :commands (ggtags-find-tag-dwim ggtags-find-reference ggtags-mode)
  :hook (scss-mode . ggtags-mode)
  :config
  ;; Sort global results by nearness. This helps when editing Sass, as the
  ;; default variables will have a lower priority.
  (setq ggtags-sort-by-nearness t)

  ;; Fix gtags for Sass. Pygments has got a parser that works great, but it
  ;; doesn't use the dollar sign prefix. We'll have to manually add the jump
  ;; handler to scss-mode as there are not any yet.
  (add-hook! 'scss-mode-hook (modify-syntax-entry ?$ "'") (modify-syntax-entry ?% "."))

  ;; Completion is handled through `company-capf', though for scss in particular
  ;; we just want to use tags together with the lsp server as the built in
  ;; support misses a lot of variables
  (set-lookup-handlers! 'ggtags-mode :definition #'ggtags-find-tag-dwim :references #'ggtags-find-reference))

(use-package! kotlin-mode)

;; Transforms ^L characters into horizontal lines
(use-package! page-break-lines
  :config
  (add-hook! (emacs-lisp-mode view-mode) 'turn-on-page-break-lines-mode))

;; The standard should be set either through a configuration file or globally
;; using `phpcs --config-set default_standard psr2' to ensure that flymake and
;; phpcbf use the same standard
(use-package! phpcbf
  :config
  (set-formatter! 'php-mode #'phpcbf))

;;; Overrides

(after! agda2-mode
  (set-lookup-handlers! 'agda2-mode :definition #'agda2-goto-definition-keyboard))

(after! company-tng
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.1
        ;; ;; The previews are nice, but it doesn't work that well when combined
        ;; ;; with tng
        ;; company-frontends
        ;; '(company-preview-if-just-one-frontend
        ;;   company-tng-frontend
        ;;   company-pseudo-tooltip-unless-just-one-frontend
        ;;   company-echo-metadata-frontend)
        ))

(after! csharp-mode
  (set-electric! 'csharp-mode :chars '(?\n ?\{)))

(after! (yasnippet web-mode)
  ;; By default web-mode's snippets inherit from html-mode which in turn
  ;; inherits from nxml-mode, which contains a lot of snippets that are not
  ;; necesary when using emmet
  (remhash 'web-mode yas--parents))

(after! emmet-mode
  ;; Don't put an XML style slash at the end of self closing tags
  (setq emmet-self-closing-tag-style ""))

(after! ediff
  ;; Ancestor is already shown in buffer C
  (setq ediff-show-ancestor nil))

(after! ein
  (setq ein:jupyter-default-notebook-directory nil
        ein:slice-image '(10 nil)))

(after! ess
  (set-evil-initial-state! 'ess-help-mode 'motion)

  ;; The company backends are either not set or overwritten as `ess-mode'
  ;; inherrits from `prog-mode'
  (require 'ess-r-mode)
  (set-company-backend! 'ess-mode (car ess-r-company-backends))
  (set-company-backend! 'inferior-ess-mode (car ess-r-company-backends))

  ;; ESS buffers should not be cleaned up automatically
  (add-hook 'inferior-ess-mode-hook #'doom-mark-buffer-as-real-h)
  ;; Smartparens broke this a few months ago
  (add-hook 'inferior-ess-mode-hook #'smartparens-mode))

(after! evil
  (setq-default evil-symbol-word-search t)

  ;; Doom Emacs overrides the `;' and `,' keys to also repeat things like
  ;; searches. Because it uses evil-snipe by default this hasn't been done for
  ;; the default f/F/t/T keybindings.
  (set-repeater! evil-find-char evil-repeat-find-char evil-repeat-find-char-reverse)
  (set-repeater! evil-find-char-backward evil-repeat-find-char evil-repeat-find-char-reverse)
  (set-repeater! evil-find-char-to evil-repeat-find-char evil-repeat-find-char-reverse)
  (set-repeater! evil-find-char-to-backward evil-repeat-find-char evil-repeat-find-char-reverse)

  ;; These are not necessary because of `scroll-conservatively'
  (dolist (fn '(evil-ex-search-forward evil-ex-search-backward))
    (advice-remove fn #'doom-recenter-a)))

(after! evil-org
  (setq evil-org-use-additional-insert t)
  (add-to-list 'evil-org-key-theme 'additional)
  (evil-org--populate-additional-bindings))

(after! evil-surround
  ;; Add evil-surround support for common markup symbols
  (dolist (pair '((?$ . ("$" . "$")) (?= . ("=" . "=")) (?~ . ("~" . "~"))
                  (?/ . ("/" . "/")) (?* . ("*" . "*")) (?* . (":" . ":"))))
    (push pair evil-surround-pairs-alist)))

(after! helm
  (setq helm-default-external-file-browser "mimeopen")

  ;; Some of the default Helm windows are a bit too small for my liking
  (add-hook! 'helm-after-initialize-hook
    (set-popup-rule! "^\\*helm" :vslot -100 :size 10 :ttl nil)
    (set-popup-rule! "^\\*swiper\\*" :vslot -100 :size 10 :ttl nil)))

(after! helm-ag
  (setq helm-ag-base-command "rg --no-heading"))

(after! fish-mode
  (set-electric! 'fish-mode :words '("else" "end")))

(after! flycheck
  (set-evil-initial-state! 'flycheck-error-list-mode 'normal))

(after! flyspell
  ;; Don't automatically spellcheck when enabling flycheck
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'yaml-mode-hook 'flyspell-mode)
  (setq-default +flyspell-immediately nil))

(after! format-all
  ;; Override shfmt to use two spaces instead of tabs
  (set-formatter! 'shfmt
    '("shfmt"
      "-i" "2"
      ;; Mode selection copied from the default config
      ("-ln" "%s" (cl-case (and (boundp 'sh-shell) (symbol-value 'sh-shell))
                    (bash "bash") (mksh "mksh") (t "posix"))))
    :modes 'sh-mode)

  (add-to-list '+format-on-save-enabled-modes 'yaml-mode t))

(after! haskell-mode
  (set-formatter! 'hindent '("hindent") :modes '(haskell-mode  literate-haskell-mode))
  ;; Hindent usually works fine, but it doesn't play nice with preformatted
  ;; files or with certain constructs
  (add-to-list '+format-on-save-enabled-modes 'haskell-mode t)

  ;; Improve code navigation in Haskell buffers
  (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode)
  (add-hook 'haskell-mode-hook #'haskell-indentation-mode)
  (setq-hook! 'haskell-mode-hook
    outline-regexp "-- \\*+"
    ;; `haskell-mode' sets the default tab width to eight spaces for some reason
    tab-width 2))

(after! helpful
  ;; Increase the size of help popups to match Ivy's height
  (set-popup-rule! "^\\*Help" :size 0.3 :select t))

(after! hl-todo
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("FIXME" . ,(face-foreground 'error))
          ("XXX"   . ,(face-foreground 'error))
          ("HACK"  . ,(face-foreground 'error))
          ("NOTE"  . ,(face-foreground 'success)))))

(after! intero
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

(after! latex-mode
  (set-electric! 'latex-mode :chars '(?\n ?\{)))

(after! langtool
  (setq langtool-disabled-rules '("WHITESPACE_RULE")
        langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*"))

(after! lsp-mode
  ;; Don't highlight symbols automatically, I'll use `gh' to do this manually
  (setq lsp-enable-symbol-highlighting nil)
  ;; Increase the height of documentation (since these will contain long
  ;; docstrings in them)
  (set-popup-rule! "^\\*lsp-help\\*$" :size 0.3 :select t)

  ;; Mode-specific configuration

  ;; Enable clippy support
  (add-hook! 'rustic-mode-hook :append
    (let ((preferences (make-hash-table)))
      (puthash "clippy_preference" "on" preferences)
      (lsp--set-configuration `(:rust ,preferences))))

  ;; We can't apply our configuration in a simple hook as lsp-mode gets loaded
  ;; asynchronously
  (add-hook! 'lsp-managed-mode-hook :append
    (cond ((derived-mode-p 'scss-mode)
           ;; `lsp-mode' overrides our tags here, but we need those for variable name
           ;; completions as `lsp-css' isn't that smart yet
           (setq company-backends '((:separate company-capf
                                               company-lsp
                                               company-yasnippet))
                 ;; lsp-css's auto completion returns so many results that
                 ;; company struggles to keep up
                 company-idle-delay 0.3
                 completion-at-point-functions '(ggtags-completion-at-point))))))

(after! lsp-ui
  ;; Use regular flycheck popups instead of the sideline
  (add-hook 'lsp--managed-mode-hook #'flycheck-posframe-mode))

(after! magit
  (remove-hook 'git-commit-setup-hook #'+vc-start-in-insert-state-maybe)
  (setq magit-diff-refine-hunk 'all))

(after! magit-todos
  ;; Ignore concatenated/minified files when searching for todos
  (setq magit-todos-rg-extra-args '("-M 512")))

(after! python
  ;; Set this to `django' to force docstring to always be on multiple lines
  (setq python-fill-docstring-style 'onetwo)

  (setq lsp-python-ms-nupkg-channel "beta")
  (after! lsp-ui
    ;; Also show flake8 warnings since mspyls misses a lot of things
    (flycheck-add-next-checker 'lsp-ui '(warning . python-flake8)))

  ;; Electric indent on `:' only really works for `else' clauses and makes
  ;; defining functions a lot harder than it should be
  (set-electric! 'python-mode :words '("else:"))
  ;; Disable the default template, as we don't need a hashbang in every Python
  ;; file
  (set-file-template! 'python-mode :ignore t))

(after! markdown-mode
  ;; Disable trailing whitespace stripping for Markdown mode
  (add-hook 'markdown-mode-hook #'doom-disable-delete-trailing-whitespace-h)
  ;; Doom adds extra line spacing in markdown documents
  (add-hook! 'markdown-mode-hook :append (setq line-spacing nil)))

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
  ;; Killing the omnisharp server doesn't work as well when constantly switching
  ;; branches and previewing files
  (add-hook! 'csharp-mode-hook :append
    (remove-hook 'kill-buffer-hook #'omnisharp-stop-server t)))

(after! org
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
                    \\setminted{frame=leftline,framesep=1em,linenos,numbersep=1em,style=friendly}
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
                     :scale (* 1.25 +scaling-ratio))
          org-latex-caption-above nil
          org-latex-listings 'minted
          ;; latexmk tends to play along nicer than pdflatex
          org-latex-pdf-process '("latexmk -f -pdf %f"))))

(after! ox-pandoc
  ;; Doom explicitely adds the deprecated `parse-raw' option
  (setq org-pandoc-options '((standalone . t) (mathjax . t))))

(after! prodigy
  (set-evil-initial-state! 'prodigy-mode 'normal))

(after! rustic-mode
  ;; Add missing confugration
  ;; XXX: Is this still needed?
  (set-electric! 'rustic-mode :chars '(?\n ?\}))

  ;; FIXME: Without this, function opening braces don't expand
  ;; XXX: Is this still needed?
  (dolist (brace '("(" "{" "["))
    (sp-local-pair 'rustic-mode brace nil
                   :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))))

  ;; RLS, for some reason, always wants to use the stable compiler's source code
  ;; even when specifically running the nightly RLS
  (setenv "RUST_SRC_PATH"
          (expand-file-name "~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"))
  ;; XXX: Is this still needed?
  (setq racer-rust-src-path (getenv "RUST_SRC_PATH")))

(after! css-mode
  (set-electric! 'css-mode :chars '(?})))

(after! web-mode
  ;; Make sure that attributes are indented when breaking lines (e.g. long lists
  ;; of classes)
  (set-electric! 'web-mode :chars '(?\<) :words '("endfor" "endif" "endblock"))

  ;; Editorconfig tells web-mode to indent attributes instead of aligning
  (add-hook! 'web-mode-hook :append
    (setq web-mode-attr-indent-offset nil
          web-mode-attr-value-indent-offset nil
          web-mode-block-padding 0)))

(autoload 'web-mode-set-engine "web-mode" nil t)
(def-project-mode! +web-django-mode
  :modes '(web-mode js-mode coffee-mode css-mode haml-mode pug-mode)
  :files ("manage.py")
  :on-enter (web-mode-set-engine "django"))

(after! wordnut
  (set-popup-rule! "^\\*WordNut\\*$" :size 0.3 :select t))

(after! yasnippet
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand))

(after! vterm
  ;; Disable cursor blinking, this is not needed and it persists after the
  ;; terminal clsoes
  (add-hook! 'vterm-mode-hook (blink-cursor-mode -1)))

;;; Settings

(setq completion-styles '(partial-completion initials)
      confirm-nonexistent-file-or-buffer nil
      ;; `jk' tends to cause a lot of issues when writing in Dutch
      evil-escape-key-sequence nil
      evil-ex-substitute-global nil
      executable-prefix-env t
      ;; Order should not matter when searching
      ;; ivy-re-builders-alist '(;; (swiper . ivy--regex-plus)
      ;;                         (t      . ivy--regex-ignore-order))
      flyspell-default-dictionary "english"
      nav-flash-delay 0.25
      which-key-idle-delay 0.4

      +evil-want-o/O-to-continue-comments nil
      doom-modeline-height 30
      org-directory (expand-file-name "~/Documenten/notes/")
      ;; The gray comments are hard to read in my terminal, although I rarely
      ;; use Emacs in a terminal
      doom-one-brighter-comments (not (or (display-graphic-p) (daemonp)))
      display-line-numbers-type 'relative
      doom-font (font-spec :family "InputMono Nerd Font"
                           :width 'semi-condensed
                           :size +font-size)
      doom-variable-pitch-font (font-spec :family "Roboto"
                                          :size +font-size))

;; Increase the default frame size
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 120))

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

;; Make `w' and `b' handle more like in vim
(add-hook 'after-change-major-mode-hook #'+robbert/fix-evil-words-underscore)

;; Show trailing whitespace, this used to be a default
(setq-default show-trailing-whitespace nil)
(add-hook! (prog-mode text-mode conf-mode)
  (defun doom-enable-show-trailing-whitespace-h ()
    (setq show-trailing-whitespace t)))

;; Set default indentation levels and coding styles
(add-hook! '(css-mode-hook
             haskell-mode-hook
             js2-mode-hook
             scss-mode-hook
             shell-mode-hook
             typescript-mode-hook
             web-mode-hook)
  (doom/set-indent-width 2))
(setq css-indent-offset 2
      js-indent-level 2
      sh-basic-offset 2
      typescript-indent-level 2
      web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-comment-style 2)

;; Different languages use different line lengths (there's probably a better
;; way to keep the variable value in the lambda)
(cl-loop for (mode . value) in '((python-mode-hook . 79)
                                 (rustic-mode-hook . 100))
         do (prin1 `(setq-hook! ',mode fill-column ,value)))

;; Add missing syntax highlighting
(add-to-list 'auto-mode-alist '("\\.service$" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.socket$" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.target$" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("index\\.theme$" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.timer$" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.csproj$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.ruleset$" . nxml-mode))

(add-to-list 'auto-mode-alist '("Pipfile$" . conf-toml-mode))
(add-to-list 'auto-mode-alist '("Pipfile\\.lock$" . json-mode))

;; Haskell-like files (Happy, Alex, uuagc)
(add-to-list 'auto-mode-alist '("\\.ag$" . +robbert/basic-haskell-mode))

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
(setq-hook! 'intero-repl-mode-hook scroll-margin 0)
(setq-hook! 'term-mode-hook scroll-margin 0)

;; Also highlight todos in text modes
(add-hook 'text-mode-hook #'hl-todo-mode)

;; Auto reload PDFs
(add-hook 'doc-view-mode-hook #'auto-revert-mode)

;; Explicitely load evil-surround so that extra pairs can be loaded in time
(require 'evil-surround)

;; Doom Emacs doesn't play along nicely with the noninteractive Emacs daemon,
;; but server functinoaly is still very useful
(require 'server)
(unless (server-running-p)
  (server-start))
