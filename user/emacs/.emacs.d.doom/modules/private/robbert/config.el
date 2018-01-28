;;; private/robbert/config.el -*- lexical-binding: t; -*-

(require 'cl)

(load! +bindings)

;; TODO: Make counsel-rg M-RET act the same way as in Spacemacs
;; TODO: SPC / *
;; TODO: Fix f/F/t/T and ;
;; TODO: Missing XXX and Hack highlighting

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

  ;; HACK: evil-collection tries to make diff buffers read only, which is nice,
  ;;       but it somehow breaks magit
  (define-advice git-commit-propertize-diff
      (:around (original-function &rest args))
    (remove-hook 'diff-mode-hook 'evil-collection-diff-toggle-setup)
    (apply original-function args)
    (add-hook 'diff-mode-hook 'evil-collection-diff-toggle-setup)))

(def-package! evil-ediff
  :after ediff)

(def-package! evil-lion
  :after evil
  :config (evil-lion-mode))

(def-package! evil-magit
  :after magit
  :config
  (remove-hook 'git-commit-mode-hook #'evil-insert-state))

(def-package! evil-org
  :after org
  :config
  (setq evil-org-use-additional-insert t)

  ;; Add an easier 'insert item after this line' keybinding. evil-org only
  ;; inserts a new item when the bullet is on the current line.
  (defun +robbert/evil-org-always-open-below ()
    (interactive)
    (end-of-visible-line)
    (org-meta-return)
    (evil-insert nil)))

;;; Overrides

;; `counsel-projectile-rg' doesn't get autoloaded in the default config
(autoload 'counsel-projectile-rg "counsel-projectile" nil t)

(after! exec-path-from-shell
  ;; Make sure racer can find Rust's source files and disable gtags as Rust's
  ;; Language Server does a better job already
  (exec-path-from-shell-copy-envs '("LD_LIBRARY_PATH" "RUST_SRC_PATH")))

(after! helpful
  (set! :evil-state 'helpful-mode 'motion))

(after! python
  ;; Python docstrings should always be on multiple lines
  (setq python-fill-docstring-style 'django))

(after! org
  ;; Restore default indentation behavior
  (remove-hook 'org-mode-hook 'org-indent-mode)
  (setq org-adapt-indentation t
        org-startup-indented nil)

  (setq org-highlight-latex-and-related '(latex script entities))

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
      evil-symbol-word-search t
      ivy-re-builders-alist '((swiper . ivy--regex-plus)
                              (t      . ivy--regex-ignore-order))
      nav-flash-delay 0.25
      which-key-idle-delay 0.4
      ;; doom-line-numbers-style 'relative ;; FIXME: Broken right now
      doom-font (font-spec :family "Input Mono"
                           :width 'semi-condensed
                           :size (if (eq system-name "laptop") 18 16)))

;; Disable blinking
(add-hook 'doom-init-ui-hook (lambda () (blink-cursor-mode -1)) t)

;; Make `w' and `b' handle more like in vim
(defun +robbert/fix-evil-words-underscore ()
  (modify-syntax-entry ?_ "w"))
(defun +robbert/fix-evil-words-dash ()
  (modify-syntax-entry ?- "w"))
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

;; Improve scrolling behaviour by recentering the screen when jumping to
;; something off screen
(setq mouse-wheel-scroll-amount '(1 ((shift) . 3) ((control)))
      scroll-conservatively 3
      scroll-margin 3)
(add-hook 'term-mode-hook (lambda () (setq-local scroll-margin 0)))

;; Auto reload PDFs
(add-hook 'doc-view-mode-hook #'auto-revert-mode)

;; Enable automatic auto completion.
(require 'company)
(require 'company-tng)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 2)

;;; Functions

(defun +robbert/company-select-next-or-complete (&optional arg)
  "Select the next candidate if more than one, else complete.
With ARG, move by that many elements. This removes the default
'match prefix' funcitonality."
  (interactive "p")
  (if (> company-candidates-length 1)
      (company-select-next arg)
    (company-complete-selection)))

(defun +robbert/delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting.
Take from
http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/."
  (interactive)
  (when buffer-file-name
    (if (vc-backend buffer-file-name)
        (vc-delete-file buffer-file-name)
      (progn
        (delete-file buffer-file-name)
        (message "Deleted file %s" buffer-file-name)
        (kill-buffer)))))

(defun +robbert/find-file-in-dir ()
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'counsel-file-jump))

(defun +robbert/find-file-in-project ()
  (interactive)
  (counsel-file-jump nil (projectile-project-root)))
(defun +robbert/newline-and-indent ()
  "Inserts a newline and possibly indents it. This is the same as
`+doom/newline-and-indent' but without the comment handling."
  (interactive)
  (if (sp-point-in-string)
      (newline)
    (progn
      (newline nil t)
      (indent-according-to-mode))))

(defun +robbert/indent-pasted-text ()
  "Indents whatever was just pasted."
  (interactive)
  (save-excursion
    (+evil/reselect-paste)
    (call-interactively 'indent-region)))

;; Add keybindings for locating a file in a directory or in the current
;; project, even when it's ignored.
(defun +robbert/magit-blame-follow-copy ()
  "Blame with the `-wCCC' options, telling Git to track copied
text"
  (interactive)
  (magit-blame magit-buffer-refname buffer-file-name '("-wCCC")))

(define-minor-mode pleb-mode
  "Emacs for normal people"
  :lighter " PLEB"
  :global t
  ;; Enable more standard keybindings
  (evil-mode 'toggle)
  (cua-mode 'toggle)
  (if pleb-mode
      (progn
        ;; File trees are nice
        (+neotree/toggle)
        (other-window -1)
        ;; Always use the bar cursor
        (setq cursor-type 'bar)
        ;; What is this weird numbering?
        (nlinum-relative-off))
    (nlinum-relative-on)))
