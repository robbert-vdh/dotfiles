;;; ../../.dotfiles/user/emacs/.emacs.d/modules/private/robbert/autoload.el -*- lexical-binding: t; -*-

;; These two are missing from the default config

;;;###autoload
(defmacro +robbert--def-browse-in! (prefix name dir)
  `(defun ,(intern (format "+%s/browse-%s" prefix name)) ()
     (interactive)
     (doom-project-browse ,dir)))

;;;###autoload
(defmacro +robbert--def-find-in! (prefix name dir)
  `(defun ,(intern (format "+%s/find-in-%s" prefix name)) ()
     (interactive)
     (doom-project-find-file ,dir)))

(setq +dotfiles-dir (expand-file-name "~/.dotfiles/"))
;;;###autoload (autoload '+default/find-in-dotfiles "private/robbert/autoload" nil t)
(+robbert--def-find-in!   default dotfiles +dotfiles-dir)
;;;###autoload (autoload '+default/browse-dotfiles "private/robbert/autoload" nil t)
(+robbert--def-browse-in! default dotfiles +dotfiles-dir)

;;; Custom functions used in config and keybindigns

;;;###autoload
(defun +robbert/company-select-next-or-complete (&optional arg)
  "Select the next candidate if more than one, else complete.
With ARG, move by that many elements. This removes the default
'match prefix' funcitonality."
  (interactive "p")
  (if (> company-candidates-length 1)
      (company-select-next arg)
    (company-complete-selection)))

;;;###autoload
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

;; Add an easier 'insert item after this line' keybinding. evil-org only
;; inserts a new item when the bullet is on the current line.
;;;###autoload
(defun +robbert/evil-org-always-open-below ()
  (interactive)
  (end-of-visible-line)
  (org-meta-return)
  (evil-insert nil))

;;;###autoload
(defun +robbert/find-file-in-dir ()
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'counsel-file-jump))

;;;###autoload
(defun +robbert/find-file-in-project ()
  (interactive)
  (counsel-file-jump nil (projectile-project-root)))

;;;###autoload
(defun +robbert/fix-evil-words-underscore ()
  (modify-syntax-entry ?_ "w"))
;;;###autoload
(defun +robbert/fix-evil-words-dash ()
  (modify-syntax-entry ?- "w"))

;;;###autoload
(defun +robbert/newline-and-indent ()
  "Inserts a newline and possibly indents it. This is the same as
`+doom/newline-and-indent' but without the comment handling."
  (interactive)
  (if (sp-point-in-string)
      (newline)
    (progn
      (newline nil t)
      (indent-according-to-mode))))

;;;###autoload
(defun +robbert/indent-pasted-text ()
  "Indents whatever was just pasted."
  (interactive)
  (save-excursion
    (+evil/reselect-paste)
    (call-interactively 'indent-region)))

;; Add keybindings for locating a file in a directory or in the current
;; project, even when it's ignored.
;;;###autoload
(defun +robbert/magit-blame-follow-copy ()
  "Blame with the `-wCCC' options, telling Git to track copied
text"
  (interactive)
  (magit-blame magit-buffer-refname buffer-file-name '("-wCCC")))

;;;###autoload
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
