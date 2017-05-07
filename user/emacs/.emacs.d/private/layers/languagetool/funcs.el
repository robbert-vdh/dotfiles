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

(defun languagetool/toggle ()
  "Performs grammar and spell checking on the current buffer
  using LanguageTool for grammar and flyspell for spell
  checking."
  (interactive)
  (if (package-installed-p 'langtool)
      ;; Clear LanguageTool's errors if there is an active error overlay
      (if (and (fboundp 'langtool--overlays-region)
               (not (null (langtool--overlays-region (point-min) (point-max)))))
          (progn
            (langtool-check-done)
            (flyspell-delete-all-overlays))
        ;; Don't do anything while LanguageTool is still running
        (unless (and (boundp 'langtool-mode-line-message)
                     (equal ":run" (cadr langtool-mode-line-message)))
          (langtool-check-buffer (spacemacs//languagetool-get-language))
          (flyspell-buffer)))
    (error "LanguageTool has not been set up yet")))

(defun spacemacs//languagetool-detect ()
  "Detects whether the LanguageTool binary exists"
  (cond ((boundp 'langtool-language-tool-jar)
         (if (file-readable-p langtool-language-tool-jar)
             t
           (spacemacs-buffer/warning "LanguageTool binary not found")))
        ((boundp 'langtool-java-classpath) t)
        (t (spacemacs-buffer/warning "LanguageTool binary not set"))))

(defun spacemacs//languagetool-get-language ()
  "Tries to parse the current spell checking language for a
  usable locale string"
  (let ((language (or ispell-local-dictionary ispell-dictionary)))
    (when language
      (let* ((dict (assoc language ispell-dicts-name2locale-equivs-alist))
             ;; ispell uses underscores in its locales, but LanguageTool expects a
             ;; dash (e.g. "en_US" => "en-US")
             (language-code (replace-regexp-in-string "_" "-" (cadr dict))))
        language-code))))
