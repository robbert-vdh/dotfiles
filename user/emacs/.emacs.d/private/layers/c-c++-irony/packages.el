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
;;
;; This layer is made to work in conjunction with the PlatformIO layer
;; (https://github.com/LeartS/platformio-layer), as such it doesn't include the
;; majority of required initialization.

(defconst c-c++-irony-packages
  '(company
    company-irony
    flycheck-irony
    irony
    irony-eldoc))

(defconst c-c++-irony-excluded-packages
  '(company-clang))

(defun c-c++-irony/post-init-irony ()
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (spacemacs|diminish irony-mode " Ⓘ" " I")
  )

(defun c-c++-irony/init-flycheck-irony ()
  (use-package flycheck-irony
    :defer t
    :init
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))))
