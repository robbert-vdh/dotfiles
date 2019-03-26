;;; ~/.config/doom/autoload/functions.el -*- lexical-binding: t; -*-

;;; Variables

(defvar +robbert/scss-tag-dirs
  '("node_modules/bootstrap"
    "node_modules/motion-ui"
    "node_modules/foundation-sites"
    "assets"
    "ClientApp/styles" ;; ASP.NET SPA
    "public"
    "src"
    "theme/src")
  "The directories, starting from a Node.JS project root, that
  should be searched for SCSS tags.")

;;; Interactive functions

(setq +dotfiles-dir (expand-file-name "~/.dotfiles/"))
;;;###autoload
(defun +robbert/browse-dotfiles ()
  (interactive) (doom-project-browse +dotfiles-dir))
;;;###autoload
(defun +default/find-in-dotfiles ()
  (interactive) (doom-project-find-file +dotfiles-dir))

;;; Functions

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
(defun +robbert/company-box-next-line ()
  "Override for `company-box--next-line' to make
`company-tng-frontend' work. Emulates the normal behaviour of
`company-select-next'."
  (interactive)
  (let ((company-frontends '(company-tng-frontend)))
    (+robbert/company-select-next-or-complete))

  (company-box--change-line)
  (company-box--update-width))

;;;###autoload
(defun +robbert/company-box-prev-line ()
  "Override for `company-box--prev-line' to make
`company-tng-frontend' work. Emulates the normal behaviour of
`company-select-previous'."
  (interactive)
  (let ((company-frontends '(company-tng-frontend)))
    (company-select-previous))

  (company-box--change-line)
  (company-box--update-width))

;;;###autoload
(defun +robbert/clipboard-to-buffer ()
  "Replace the buffer's contents with the clipboard's contents.
Copied from Spacemacs."
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

;;;###autoload
(defun +robbert/ein:worksheet-merge-cell-next ()
  (interactive)
  (ein:worksheet-merge-cell (ein:worksheet--get-ws-or-error) (ein:worksheet-get-current-cell) t t))

;;;###autoload
(defun +robbert/enable-wrapping ()
  "Enable `truncate-lines' and `visual-line-mode', and disable `auto-fill-mode'.
  This is useful when composing messages for things that respect
  line breaks."
  (interactive)
  (toggle-truncate-lines)
  (visual-line-mode +1)
  (auto-fill-mode -1))

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
  (call-interactively 'helm-find))

;;;###autoload
(defun +robbert/find-file-in-project ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (call-interactively 'helm-find)))

;;;###autoload
(defun +robbert/fix-evil-words-underscore ()
  (modify-syntax-entry ?_ "w"))
;;;###autoload
(defun +robbert/fix-evil-words-dash ()
  (modify-syntax-entry ?- "w"))

;;;###autoload
(defun +robbert/generate-scss-tags (&optional directory)
  "Regenerate SCSS tags for the current project, starting at the
nearest `package.json' or the project root. This will overwrite
all existing tags."
  (interactive)
  (let ((default-directory (or directory
                               ;; Prompt when there's a prefix argument
                               (and current-prefix-arg
                                    (read-directory-name "Project root: "))
                               (locate-dominating-file "." "package.json")
                               (projectile-project-root)))
        (scss-dirs (string-join +robbert/scss-tag-dirs " ")))
    (shell-command (concat  "find " scss-dirs " -iname '*.scss' >gtags.files 2>/dev/null | true"))
    (shell-command "gtags --gtagslabel pygments")))

;;;###autoload
(defun +robbert/init-emacs-anywhere (app-name window-title x y w h)
  (+robbert/clipboard-to-buffer)
  (pcase app-name
    ("Firefox"
     (cond ((string-prefix-p "JupyterLab" window-title) (python-mode))
           (t (markdown-mode))))))

;;;###autoload
(defun +robbert/lsp-format-before-save ()
  ;; Use the server's formatter instead of format-all
  (remove-hook 'before-save-hook #'+format|buffer t)
  (add-hook 'before-save-hook #'lsp-format-buffer nil t))

;;;###autoload
(defun +robbert/newline-and-indent ()
  "Inserts a newline and possibly indents it. This is the same as
`doom*newline-and-indent' but without the comment handling."
  (interactive)
  (if (sp-point-in-string)
      (newline)
    (progn
      (newline nil t)
      (indent-according-to-mode))))

;;;###autoload
(defun +robbert/newline-and-indent-always ()
  "Inserts a newline and always indent it. This is useful for
HTML, where string are really just argument lists and should be
treaded the same. This is needed because `newline-and-indent'
gets overridden."
  (interactive)
  (newline nil t)
  (indent-according-to-mode))

;;;###autoload
(defun +robbert/magit-blame-follow-copy ()
  "Blame with the `-wCCC' options, telling Git to track copied
text"
  (interactive)
  (require 'magit-blame)
  (let ((magit-blame-arguments (cons "-CCC" (magit-blame-arguments))))
    (magit-blame)))

(defvar +robbert--pipenv-project-root nil
  "The last project we've checked pipenv for. This prevents
  unnecesarily trying to activate pipenv while it's already
  active.")

;;;###autoload
(defun +robbert/python-maybe-enable-pipenv ()
  "Either enables or disables pipenv depending on the presence of
  a `Pipfile' in the current project."
  (interactive)
  (when (not (equal +robbert--pipenv-project-root (projectile-project-root)))
    (if (locate-dominating-file default-directory "Pipfile")
        (progn
          (pipenv-activate)
          (message "Pipenv activated for current project"))
      (pipenv-deactivate))

    (setq +robbert--pipenv-project-root (projectile-project-root))))

;;;###autoload
(defun +robbert/scss-find-file (filename)
  "`find-file-at-point' won't find find some file names by
   default, so we'll have to manually push it in the right
   direction. There are two special cases handled here:

   1) The file is a partial, and an underscore should be added.
   2) The file is located in `node_modules' and starts with a
      tilde."
  (let* ((project-root (locate-dominating-file "." "node_modules"))
         (node-modules (concat project-root "node_modules/"))
         (filename (replace-regexp-in-string "^~" node-modules filename))
         ;; The file extension is probably missing, so we'll add it unless the
         ;; file is clearly a directory
         (filename (or (and (string-match-p "\\(.scss\\|\\/\\)$" filename) filename)
                       (concat filename ".scss")))
         ;; Scss partials include an underscore before the filename
         (partial-filename (concat (file-name-directory filename)
                                   "_"
                                   (file-name-nondirectory filename))))
    ;; Try both with and without an underscore, as `ffap-file-at-point' only
    ;; expects a single result
    (or (and (file-exists-p partial-filename) partial-filename)
        filename)))

;;;###autoload
(defun +robbert/switch-terminal-buffer ()
  "Switch to a terminal buffer. This is useful when multiple
multi-term buffers are open at once."
  (interactive)
  ;; TODO: Rewrite this for Helm or scrap it as I don't really use this anyway
  (ivy-read "Switch to terminal: " 'internal-complete-buffer
            :predicate #'+robbert--is-terminal-buffer-p
            :matcher #'ivy--switch-buffer-matcher
            :preselect (buffer-name (other-buffer (current-buffer)))
            :action #'ivy--switch-buffer-action
            :keymap ivy-switch-buffer-map
            :caller '+robbert/switch-terminal-buffer))

;;;###autoload
(defun +robbert/term-toggle-line-mode ()
  "Toggles between line and char mode in term-mode."
  (interactive)
  (if (term-in-line-mode)
      (progn
        (evil-emacs-state)
        (term-char-mode))
    (evil-motion-state)
    (term-line-mode)))

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
(defun +robbert/unfill-paragraph (&optional region)
  "The inverse of `fill-paragraph', i.e. put an entire pagraph on
  a single line. Source:
  https://www.emacswiki.org/emacs/UnfillParagraph"
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

;;; Modes

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

;;; Helper functions

;;;###autoload
(defun +robbert--is-terminal-buffer-p (buffer)
  (with-current-buffer (cdr buffer)
    (memq major-mode '(term-mode multi-term-mode shell-mode eshell-mode))))

;;;###autoload
(defun +robbert--languagetool-get-language ()
  "Try to parse the current spell checking language for a usable
locale string, as they may be different from what languagetool is
expecting."
  (when-let* ((language (or ispell-local-dictionary ispell-dictionary)))
    ;; We'll assume the language is either a locale or a named language (i.e.
    ;; "en_GB" or "english")
    (let* ((locale
            (or (cadr (assoc language ispell-dicts-name2locale-equivs-alist))
                language))
           ;; ispell uses underscores in its locales, but LanguageTool expects a
           ;; dash (e.g. "en_US" => "en-US")
           (langtool-code (replace-regexp-in-string "_" "-" locale)))
      langtool-code)))
