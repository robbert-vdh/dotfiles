;;; packages.el --- c-c++-irony layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Robbert van der Helm <mail@robbertvanderhelm.nl>
;; URL: https://github.com/robbert-vdh/dotfiles
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst c-c++-irony-packages
  '(company
    company-irony
    flycheck
    flycheck-irony
    irony
    irony-eldoc))

(defconst c-c++-irony-excluded-packages
  '(company-clang))

(defun c-c++-irony/post-init-company ()
  (spacemacs|add-company-hook irony-mode))

(defun c-c++-irony/init-company-irony ()
  (use-package company-irony
    :defer t
    :init
    (push 'company-irony company-backends-irony-mode)))

(defun c-c++-irony/init-irony ()
  (use-package irony
    :defer t
    :commands (irony-mode irony-install-server)
    :init
    (progn
      (add-hook 'c++-mode-hook 'irony-mode)
      (add-hook 'c-mode-hook 'irony-mode)
      (add-hook 'objc-mode-hook 'irony-mode)
      (add-hook 'irony-mode-hook
                (lambda ()
                  (define-key irony-mode-map [remap completion-at-point]
                    'irony-completion-at-point-async)
                  (define-key irony-mode-map [remap complete-symbol]
                    'irony-completion-at-point-async)
                  (irony-cdb-autosetup-compile-options)))
      (spacemacs|diminish irony-mode " Ⓘ" " I"))))

(defun c-c++-irony/init-irony-eldoc ()
  (use-package irony-eldoc
    :defer t
    :init
    (add-hook 'irony-mode-hook 'irony-eldoc)))

(defun c-c++-irony/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'irony-mode))

(defun c-c++-irony/init-flycheck-irony ()
  (use-package flycheck-irony
    :defer t
    :init
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))
