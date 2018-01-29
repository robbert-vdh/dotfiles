;;; private/robbert/config.el -*- lexical-binding: t; -*-

(require 'cl)

(load! +bindings)

;; TODO: SPC / * to search for the word under the cursor
;; TODO: Copy snippets from Spacemacs config
;; TODO: Strip trailing whitespace on save

(def-package! evil-collection
  :after company-tng ;; Should be `evil', but this makes it a little easier
  :config
  (setq evil-collection-setup-minibuffer t)

  ;; evil-collection breaks backspace in term, but emacs mode plus C-z for
  ;; copying works fine
  (setq evil-collection-mode-list
        (seq-remove (lambda (elem) (and (listp elem) (eq (car elem) 'term)))
                    evil-collection-mode-list))
  (evil-collection-init)

  ;; `SPC' should still be a leader key in dired
  (after! dired
    (evil-define-key 'normal dired-mode-map
      " " nil))

  ;; HACK: evil-collection tries to make diff buffers read only, which is nice,
  ;;       but it somehow breaks magit
  (define-advice git-commit-propertize-diff
      (:around (original-function &rest args))
    (remove-hook 'diff-mode-hook #'evil-collection-diff-toggle-setup)
    (apply original-function args)
    (add-hook 'diff-mode-hook #'evil-collection-diff-toggle-setup)))

(def-package! evil-ediff
  :after ediff)

(def-package! evil-lion
  :after evil
  :config (evil-lion-mode))

(def-package! evil-magit
  :after magit
  :config
  ;; git-commit is always verbose as specified in ~/.gitconfig
  (setq magit-commit-show-diff nil)
  (remove-hook 'git-commit-mode-hook #'evil-insert-state)
  ;; Doom Emacs disables evil in `magit-blame' by default
  (add-hook 'magit-blame-mode-hook #'evil-local-mode t)
  ;; Don't interfere with the leader key
  (define-key magit-mode-map (kbd doom-leader-key) nil)
  ;; evil-vimish-fold overrides evil-magit's `z' keys and it's not useful anywya
  ;; in here, so we'll disable it
  (add-hook! 'magit-mode-hook (evil-vimish-fold-mode -1)))

(def-package! evil-org
  :after org
  :config
  (setq evil-org-use-additional-insert t)
  (add-hook 'org-mode-hook #'evil-org-mode)
  (add-hook! 'evil-org-mode-hook (evil-org-set-key-theme)))

;;; Overrides

;; `counsel-projectile-rg' doesn't get autoloaded in the default config
(autoload 'counsel-projectile-rg "counsel-projectile" nil t)

(after! evil
  (setq-default evil-symbol-word-search t)
  ;; Doom Emacs overrides the `;' and `,' keys to also repeat things like
  ;; searches. Because it uses evil-snipe by default this hasn't been done for
  ;; the default f/F/t/T keybindings.
  (do-repeat! evil-find-char evil-repeat-find-char evil-repeat-find-char-reverse)
  (do-repeat! evil-find-char-backward evil-repeat-find-char evil-repeat-find-char-reverse)
  (do-repeat! evil-find-char-to evil-repeat-find-char evil-repeat-find-char-reverse)
  (do-repeat! evil-find-char-to-backward evil-repeat-find-char evil-repeat-find-char-reverse))

(after! exec-path-from-shell
  ;; Make sure racer can find Rust's source files and disable gtags as Rust's
  ;; Language Server does a better job already
  (exec-path-from-shell-copy-envs '("LD_LIBRARY_PATH" "RUST_SRC_PATH")))

(after! flyspell
  ;; Don't automatically spellcheck when enabling flycheck
  (remove-hook 'flyspell-mode-hook #'+spellcheck|automatically)
  (add-hook! '(git-commit-mode-hook org-mode-hook)
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

(after! python
  ;; Python docstrings should always be on multiple lines
  (setq python-fill-docstring-style 'django))

(after! org
  ;; Restore default indentation behavior
  (remove-hook! 'org-mode-hook '(org-indent-mode visual-line-mode))
  (setq org-adapt-indentation t
        org-startup-indented nil)

  (setq org-highlight-latex-and-related '(latex script entities))
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

;;; Settings

(setq completion-styles '(partial-completion initials)
      evil-goggles-duration 0.25
      ivy-re-builders-alist '((swiper . ivy--regex-plus)
                              (t      . ivy--regex-ignore-order))
      +org-dir (expand-file-name "~/Documenten/notes/")
      nav-flash-delay 0.25
      which-key-idle-delay 0.4
      ;; doom-line-numbers-style 'relative ;; FIXME: Broken right now
      doom-font (font-spec :family "Input Mono"
                           :width 'semi-condensed
                           :size (if (eq system-name "laptop") 18 16)))

;; Disable blinking
(add-hook! :append 'doom-init-ui-hook
  (blink-cursor-mode -1))

;; Automatically delete trailing whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Make `w' and `b' handle more like in vim
(add-hook 'after-change-major-mode-hook #'+robbert/fix-evil-words-underscore)

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

;; Flycheck popup tweaks
(setq flycheck-pos-tip-timeout 15)

;; Improve scrolling behaviour by recentering the screen when jumping to
;; something off screen
(setq mouse-wheel-scroll-amount '(1 ((shift) . 3) ((control)))
      scroll-conservatively 3
      scroll-margin 3)
(add-hook! 'term-mode-hook (setq-local scroll-margin 0))

;; Auto reload PDFs
(add-hook 'doc-view-mode-hook #'auto-revert-mode)

;; Enable automatic auto completion.
(require 'company)
(require 'company-tng)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 2)
