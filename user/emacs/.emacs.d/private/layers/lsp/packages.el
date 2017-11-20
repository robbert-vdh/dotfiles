;;; packages.el --- lsp layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author:  <robbert@desktop>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `lsp-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lsp/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lsp/pre-init-PACKAGE' and/or
;;   `lsp/post-init-PACKAGE' to customize the package as it is loaded.

(defconst lsp-packages
  '(company-lsp
    lsp-mode
    (lsp-rust :requires rust-mode)))

(defun lsp/init-company-lsp ()
  (use-package company-lsp
    :defer t
    :init
    (progn
      (setq company-lsp-async t)
      (spacemacs|add-company-backends
        :backends company-lsp
        :modes lsp-mode))))

(defun lsp/init-lsp-mode ()
  (use-package lsp-mode
    :defer t
    :init
    (progn
      (setq lsp-highlight-symbol-at-point nil)
      (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
        "gh" 'lsp-symbol-highlight
        "ra" 'lsp-apply-commands
        "rr" 'lsp-rename))))

(defun lsp/init-lsp-rust ()
  (use-package lsp-rust
    :defer t
    :commands (lsp-rust-enable)
    :init (add-hook 'rust-mode-local-vars-hook #'lsp-rust-enable)))
