;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(load! "+bindings")
(load! "+lsp")

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
  (set-electric! 'fish-mode :words '("else" "end")))

(def-package! ggtags
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
  (set-lookup-handlers! 'scss-mode :definition #'ggtags-find-tag-dwim :references #'ggtags-find-reference))

;; Transforms ^L characters into horizontal lines
(def-package! page-break-lines
  :config
  (add-hook! (emacs-lisp-mode view-mode) 'turn-on-page-break-lines-mode))

;; The standard should be set either through a configuration file or globally
;; using `phpcs --config-set default_standard psr2' to ensure that flymake and
;; phpcbf use the same standard
(def-package! phpcbf)

(def-package! pipenv
  :init
  ;; Conditionally enable pipenv if the project comes with a `Pipfile'
  ;; FIXME: This should ideally be done after switching buffers to support
  ;;        multiple projects
  (add-hook 'python-mode-hook #'+robbert/python-maybe-enable-pipenv))

(def-package! yapfify
  :hook (python-mode . yapf-mode))

;;; Overrides

;; `counsel-projectile-rg' doesn't get autoloaded in the default config
(autoload 'counsel-projectile-rg "counsel-projectile" nil t)

(after! company-box
  ;; Fix icons (all-the-icons is not loaded in time)
  (add-hook! :after 'company-box-mode-hook
    ;; FIXME: The window does not scale correctly when the icons are larger than
    ;;        the text
    (let ((all-the-icons-scale-factor 0.9))
      (setq company-box-backends-colors nil
            company-box-icons-elisp
            (list (all-the-icons-material "functions" :face 'all-the-icons-purple)
                  (all-the-icons-material "check_circle" :face 'all-the-icons-blue)
                  (all-the-icons-material "stars" :face 'all-the-icons-yellow)
                  (all-the-icons-material "format_paint" :face 'all-the-icons-pink))
            ;; These are all copied form
            ;; https://github.com/sebastiencs/company-box/wiki/icons and translated to use `all-the-icons'
            company-box-icons-lsp
            `((1 . ,(all-the-icons-faicon "tags")) ;; Text
              (2 . ,(all-the-icons-faicon "tags" :face 'all-the-icons-purple)) ;; Method
              (3 . ,(all-the-icons-faicon "tag" :face 'all-the-icons-purple)) ;; Function
              (4 . ,(all-the-icons-faicon "tag" :face 'all-the-icons-purple)) ;; Constructor
              (5 . ,(all-the-icons-faicon "cog" :face 'all-the-icons-blue)) ;; Field
              (6 . ,(all-the-icons-faicon "cog" :face 'all-the-icons-blue)) ;; Variable
              (7 . ,(all-the-icons-faicon "cube" :face 'all-the-icons-yellow)) ;; Class
              (8 . ,(all-the-icons-faicon "cube" :face 'all-the-icons-yellow)) ;; Interface
              (9 . ,(all-the-icons-faicon "cube" :face 'all-the-icons-yellow)) ;; Module
              (10 . ,(all-the-icons-faicon "cog" :face 'all-the-icons-blue)) ;; Property
              (11 . ,(all-the-icons-material "settings_system_daydream")) ;; Unit
              (12 . ,(all-the-icons-faicon "cog" :face 'all-the-icons-blue)) ;; Value
              (13 . ,(all-the-icons-material "storage" :face 'all-the-icons-yellow)) ;; Enum
              (14 . ,(all-the-icons-material "closed_caption" :face 'all-the-icons-dyellow)) ;; Keyword
              (15 . ,(all-the-icons-material "closed_caption")) ;; Snippet
              (16 . ,(all-the-icons-material "color_lens" :face 'all-the-icons-cyan)) ;; Color
              (17 . ,(all-the-icons-faicon "file-text-o")) ;; File
              (18 . ,(all-the-icons-material "refresh")) ;; Reference
              (19 . ,(all-the-icons-faicon "folder-open")) ;; Folder
              (20 . ,(all-the-icons-material "closed_caption" :face 'all-the-icons-dorange)) ;; EnumMember
              (21 . ,(all-the-icons-faicon "square" :face 'all-the-icons-lblue)) ;; Constant
              (22 . ,(all-the-icons-faicon "cube" :face 'all-the-icons-yellow)) ;; Struct
              (23 . ,(all-the-icons-faicon "calendar")) ;; Event
              (24 . ,(all-the-icons-faicon "square-o")) ;; Operator
              (25 . ,(all-the-icons-faicon "arrows"))) ;; TypeParameter
            company-box-icons-unknown nil
            company-box-icons-yasnippet nil)))

  (advice-add #'company-box--next-line :override #'+robbert/company-box-next-line)
  (advice-add #'company-box--prev-line :override #'+robbert/company-box-prev-line)
  (advice-add #'company-box--change-line :after #'+robbert--company-box-fix-tng)
  (advice-add #'company-box--render-buffer :after #'+robbert--company-box-fix-tng))

(after! csharp-mode
  (set-electric! 'csharp-mode :chars '(?\n ?\{)))

(after! emmet-mode
  ;; Don't put an XML style slash at the end of self closing tags
  (setq emmet-self-closing-tag-style ""))

(after! ediff
  ;; Ancestor is already shown in buffer C
  (setq ediff-show-ancestor nil))

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

(after! evil-snipe
  ;; Disable evil-snipe overriding the S/s keys. This is a bit of a hack but the
  ;; clean way to disable it doesn't work right now.
  (evil-snipe-override-mode -1)
  (evil-snipe-mode -1))

(after! evil-org
  (setq evil-org-use-additional-insert t)
  (add-to-list 'evil-org-key-theme 'additional)
  (evil-org--populate-additional-bindings))

(after! evil-magit
  (remove-hook 'git-commit-mode-hook #'evil-insert-state)
  ;; git-commit is always verbose as specified in ~/.gitconfig
  (setq ;; magit-commit-show-diff nil
        magit-diff-refine-hunk 'all)

  ;; TODO: Decide which behaviour is better
  ;; Use the traditional window splitting behaviour (I think it's cleaner)
  ;; (setq magit-display-buffer-function 'magit-display-buffer-traditional)

  ;; Don't interfere with the leader key
  (dolist (mode (list magit-mode-map magit-revision-mode-map))
    (define-key mode (kbd doom-leader-key) nil)))

(after! evil-surround
  ;; Add evil-surround support for common markup symbols
  (dolist (pair '((?$ . ("$" . "$")) (?= . ("=" . "=")) (?~ . ("~" . "~"))
                  (?/ . ("/" . "/")) (?* . ("*" . "*")) (?* . (":" . ":"))))
    (push pair evil-surround-pairs-alist)))

(after! flycheck
  (set-evil-initial-state! 'flycheck-error-list-mode 'normal))

(after! flyspell
  ;; Don't automatically spellcheck when enabling flycheck
  (add-hook 'text-mode-hook 'flyspell-mode)
  (setq-default +spellcheck-immediately nil))

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

(after! latex-mode
  (set-electric! 'latex-mode :chars '(?\n ?\{)))

(after! langtool
  (setq langtool-disabled-rules '("WHITESPACE_RULE")
        langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*"))

(after! python
  ;; Set this to `django' to force docstring to always be on multiple lines
  (setq python-fill-docstring-style 'pep-257)

  ;; Disable the default template, as we don't need a hashbang in every Python
  ;; file
  (set-file-template! 'python-mode :ignore t))

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
  (remove-hook 'org-mode-hook #'org-indent-mode)
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

(after! prodigy
  (set-evil-initial-state! 'prodigy-mode 'normal))

(after! rust-mode
  ;; Add missing confugration
  (set-electric! 'rust-mode :chars '(?\n ?\}))

  ;; Don't show snippets in the completion, as these tend to cause a lot of
  ;; clutter
  (set-company-backend! 'rust-mode 'company-capf)

  ;; FIXME: Without this, function opening braces don't expand
  (dolist (brace '("(" "{" "["))
    (sp-local-pair 'rust-mode brace nil
                   :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))))

  ;; RLS, for some reason, always wants to use the stable compiler's source code
  ;; even when specifically running the nightly RLS
  (setenv "RUST_SRC_PATH"
          (expand-file-name "~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"))
  (setq racer-rust-src-path (getenv "RUST_SRC_PATH")))

(after! tide
  ;; Use the built in tsserver as formatting breaks otherwise
  (setq tide-tsserver-locator-function 'ignore)

  ;; Format TypeScript on save
  (add-hook! 'typescript-mode-hook
    (add-hook! :local 'before-save-hook 'tide-format-before-save)))

(after! web-mode
  ;; Fix double curly braces caused by smartparens
  (sp-local-pair 'web-mode "{" nil :actions nil)

  ;; Make sure that attributes are indented when breaking lines (e.g. long lists
  ;; of classes)
  (set-electric! 'web-mode :chars '(?\<) :words '("endfor" "endif" "endblock"))

  ;; Editorconfig tells web-mode to indent attributes instead of aligning
  (add-hook! :append 'web-mode-hook
    (setq web-mode-attr-indent-offset nil
          web-mode-attr-value-indent-offset nil
          web-mode-block-padding 0)))

(def-project-mode! +web-django-mode
  :modes (web-mode js-mode coffee-mode css-mode haml-mode pug-mode)
  :files ("manage.py")
  :on-enter
  (when (eq major-mode 'web-mode)
    (web-mode-set-engine "django")))

(after! wordnut
  (set-popup-rule! "^\\*WordNut\\*$" :size 0.3 :select t))

(after! yasnippet
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand))

;;; Settings

(setq company-minimum-prefix-length 2
      completion-styles '(partial-completion initials)
      confirm-nonexistent-file-or-buffer nil
      ;; `jk' tends to cause a lot of issues when writing in Dutch
      evil-escape-key-sequence nil
      evil-ex-substitute-global nil
      evil-want-C-u-scroll t
      evil-want-Y-yank-to-eol nil
      executable-prefix-env t
      ;; Order should not matter when searching
      ivy-re-builders-alist '(;; (swiper . ivy--regex-plus)
                              (t      . ivy--regex-ignore-order))
      flyspell-default-dictionary "english"
      nav-flash-delay 0.25
      show-trailing-whitespace t
      which-key-idle-delay 0.4

      +org-dir (expand-file-name "~/Documenten/notes/")
      ;; The gray comments are hard to read in my terminal, although I rarely
      ;; use Emacs in a terminal
      doom-one-brighter-comments (not (or (display-graphic-p) (daemonp)))
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
;; Doom advices the company-box frontend to always show even when there's only
;; one candidate, but this is not needed as I've also enabled the preview
(after! company-box
  (advice-remove #'company-box-frontend #'+company*box-frontend-even-if-single))

;; FIXME Doom should be doing this for us
(flycheck-posframe-mode +1)

;; Explicitely load evil-surround so that extra pairs can be loaded in time
(require 'evil-surround)

;; Doom Emacs doesn't play along nicely with the noninteractive Emacs daemon,
;; but server functinoaly is still very useful
(require 'server)
(unless (server-running-p)
  (server-start))
