;;; funcs.el --- languagetool layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Robbert van der Helm <mail@robbertvanderhelm.nl>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//languagetool-detect ()
  "Detects whether the LanguageTool binary exists"
  (if (boundp 'langtool-language-tool-jar)
      (if (file-readable-p langtool-language-tool-jar)
          t
        (spacemacs-buffer/warning "LanguageTool binary not found"))
    (spacemacs-buffer/warning "LanguageTool binary not set")))
