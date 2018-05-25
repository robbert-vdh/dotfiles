;;; ~/.config/doom/autoload/browse.el -*- lexical-binding: t; -*-

;; These macros are copied from config/default/+autoload/default, as they
;; somehow can't be found with the new module format.

;; TODO: Use the new, included macros for this, see `+default/bindings.el'

;;;###autoload
(defmacro +robbert--def-browse-in! (name dir &optional prefix)
    (let ((prefix (or prefix (cdr (doom-module-from-path (or load-file-name byte-compile-current-file))))))
          `(defun ,(intern (format "+%s/browse-%s" prefix name)) ()
                    (interactive)
                           (doom-project-browse ,dir))))

;;;###autoload
(defmacro +robbert--def-find-in! (name dir &optional prefix)
    (let ((prefix (or prefix (cdr (doom-module-from-path (or load-file-name byte-compile-current-file))))))
          `(defun ,(intern (format "+%s/find-in-%s" prefix name)) ()
                    (interactive)
                           (doom-project-find-file ,dir))))

(setq +dotfiles-dir (expand-file-name "~/.dotfiles/"))
;;;###autoload (autoload '+robbert/find-in-dotfiles "~/.config/doom/autoload/browse.el" nil t)
(+robbert--def-find-in!   dotfiles +dotfiles-dir robbert)
;;;###autoload (autoload '+robbert/browse-dotfiles "~/.config/doom/autoload/browse.el" nil t)
(+robbert--def-browse-in! dotfiles +dotfiles-dir robbert)
