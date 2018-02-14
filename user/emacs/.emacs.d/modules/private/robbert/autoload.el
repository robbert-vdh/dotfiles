;;; ../../.dotfiles/user/emacs/.emacs.d/modules/private/robbert/autoload.el -*- lexical-binding: t; -*-

(setq +dotfiles-dir (expand-file-name "~/.dotfiles/"))
;;;###autoload (autoload '+robbert/find-in-dotfiles "private/robbert/autoload" nil t)
(+default--def-find-in!   dotfiles +dotfiles-dir robbert)
;;;###autoload (autoload '+robbert/browse-dotfiles "private/robbert/autoload" nil t)
(+default--def-browse-in! dotfiles +dotfiles-dir robbert)

;;; Custom functions used in config and keybindigns

;;;###autoload
(defun +robbert/buffer-to-clipboard ()
  "Copy the buffer's contents to the system clipboard. Copied
from Spacemacs."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

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
(defun +robbert/clipboard-to-buffer ()
  "Replace the buffer's contents with the clipboard's contents.
Copied from Spacemacs."
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

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
(defun +robbert/generate-scss-tags ()
  "Regenerate SCSS tags for the current project. This will overwrite all
existing tags."
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (shell-command "find node_modules/bootstrap node_modules/foundation-sites assets public src -iname '*.scss' >gtags.files 2>/dev/null")
    (shell-command "gtags --gtagslabel pygments")))

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

(defvar +robbert/indentation-sensitive-modes '()
  "Modes that should not automatically indent when pasting")

(defvar +robbert/indentation-max-length 2000
  "The maximum length in characters for which to apply automatic
  indentation on paste.")

;;;###autoload
(defun +robbert/indent-paste-advise (original-function &rest args)
  "Automatically indent pasted code. See
`+robbert/indentation-sensitive-modes'."
  (if (or (member major-mode +robbert/indentation-sensitive-modes))
      (apply original-function (cdr args))
    (let ((inhibit-message t)
          (transient-mark-mode nil))
      (evil-start-undo-step)
      ;; This would otherwise cause double highlighting
      (let ((evil-goggles-mode nil))
        (apply original-function args))
      ;; Only indent whent the region is not too large
      (when (<= (- (region-end) (region-beginning)) +robbert/indentation-max-length)
        (indent-region (region-beginning) (region-end) nil))
      ;; HACK: For some reason the `(evil-end-undo-step)' moves the point one
      ;;       unit to the left when in insert mode
      (when (evil-insert-state-p) (forward-char))
      (evil-end-undo-step))))

;;;###autoload
(defun +robbert/magit-blame-follow-copy ()
  "Blame with the `-wCCC' options, telling Git to track copied
text"
  (interactive)
  (magit-blame magit-buffer-refname buffer-file-name '("-wCCC")))

;;;###autoload
(defun +robbert--is-terminal-buffer-p (buffer)
  (with-current-buffer (cdr buffer)
    (memq major-mode '(term-mode multi-term-mode shell-mode eshell-mode))))

;;;###autoload
(defun +robbert/switch-terminal-buffer ()
  "Switch to a terminal buffer. This is useful when multiple
multi-term buffers are open at once."
  (interactive)
  (ivy-read "Switch to terminal: " 'internal-complete-buffer
            :predicate #'+robbert--is-terminal-buffer-p
            :matcher #'ivy--switch-buffer-matcher
            :preselect (buffer-name (other-buffer (current-buffer)))
            :action #'ivy--switch-buffer-action
            :keymap ivy-switch-buffer-map
            :caller '+robbert/switch-terminal-buffer))

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

;;;###autoload
(defun +robbert/languagetool-next-error (count)
  (interactive "p")
  (dotimes (_ count) (langtool-goto-next-error))
  (langtool-show-message-at-point))

;;;###autoload
(defun +robbert/languagetool-previous-error (count)
  (interactive "p")
  (dotimes (_ count) (langtool-goto-previous-error))
  (langtool-show-message-at-point))

(autoload 'langtool-check-buffer "langtool" nil t)

;;;###autoload
(defun +robbert/languagetool-toggle ()
  "Perform grammar and spell checking on the current buffer using
LanguageTool. Flyspell errors will be cleared if the
'spell-checking' layer is active as they add a lot of clutter."
  (interactive)
  (let* ((has-ran (bound-and-true-p langtool-mode-line-message))
         (still-running (and has-ran
                             (equal ":run" (cadr langtool-mode-line-message)))))
    ;; Don't do anything while LanguageTool is still running
    (unless still-running
      (if has-ran (langtool-check-done)
        (progn
          (langtool-check-buffer (+robbert--languagetool-get-language))
          (flyspell-delete-all-overlays))))))

;;;###autoload
(defun +robbert--languagetool-get-language ()
  "Try to parse the current spell checking language for a usable
locale string, as they may be different from what languagetool is
expecting."
  (when-let ((language (or ispell-local-dictionary ispell-dictionary)))
    ;; We'll assume the language is either a locale or a named language (i.e.
    ;; "en_GB" or "english")
    (let* ((locale
            (or (cadr (assoc language ispell-dicts-name2locale-equivs-alist))
                language))
           ;; ispell uses underscores in its locales, but LanguageTool expects a
           ;; dash (e.g. "en_US" => "en-US")
           (langtool-code (replace-regexp-in-string "_" "-" locale)))
      langtool-code)))
