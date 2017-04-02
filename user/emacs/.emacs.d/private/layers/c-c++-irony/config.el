;;; config.el --- c-c++-irony layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Robbert van der Helm <mail@robbertvanderhelm.nl>
;; URL: https://github.com/robbert-vdh/dotfiles
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs|defvar-company-backends irony-mode)

;; Use C++14 by default when using Irony for error checking
(setq irony-additional-clang-options '("-std=c++14")
      flycheck-clang-language-standard "c++14"
      flycheck-gcc-language-standard "c++14")
