;;; packages.el --- platformio layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Robbert van der Helm <mail@robbertvanderhelm.nl>
;; URL: https://github.com/robbert-vdh/dotfiles
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst platformio-packages
  '(platformio-mode))

(defun platformio/init-platformio-mode ()
  (use-package platformio-mode
    :defer t
    :commands platformio-conditionally-enable
    :init
    (progn
      (add-hook 'c++-mode-hook 'platformio-conditionally-enable)
      (spacemacs/set-leader-keys-for-minor-mode 'platformio-mode
        "pb" 'platformio-build
        "pu" 'platformio-upload
        "pp" 'platformio-programmer-upload
        "ps" 'platformio-spiffs-upload
        "pc" 'platformio-clean
        "pd" 'platformio-update
        "pi" 'platformio-init-update-workspace)
      (spacemacs|diminish platformio-mode " ⓘ" " i"))))
