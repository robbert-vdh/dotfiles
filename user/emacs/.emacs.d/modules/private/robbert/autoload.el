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

