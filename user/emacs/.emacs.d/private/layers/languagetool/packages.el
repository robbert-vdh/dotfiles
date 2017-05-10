;;; packages.el --- languagetool layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Robbert van der Helm <mail@robbertvanderhelm.nl>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst languagetool-packages
  '((langtool :toggle (spacemacs//languagetool-detect))))

(defun languagetool/init-langtool ()
    (use-package langtool
      :defer t
      :init
      (progn
        ;; Spell checking is already handled by flyspell, and the whitespace
        ;; rules give a lot of false positives when linting rich text.
        (setq-default langtool-disabled-rules '("WHITESPACE_RULE"))
        (spacemacs/set-leader-keys
          "Sl" 'languagetool/toggle))))
