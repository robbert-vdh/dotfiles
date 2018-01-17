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

(setq lsp-packages
  '(company-lsp
    lsp-mode
    ;; lsp-ui
    (lsp-rust :requires rust-mode)
    (racer :excluded t)
    ))

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

;; (defun lsp/init-lsp-ui ()
;;   (use-package lsp-ui
;;     :defer t
;;     :commands (lsp-ui-mode)
;;     :init (add-hook 'lsp-mode-hook 'lsp-ui-mode)))

(defun lsp/init-lsp-rust ()
  (use-package lsp-rust
    :defer t
    :commands (lsp-rust-enable)
    :config (setq lsp-rust-rls-command '("rls"))
    :init (add-hook 'rust-mode-local-vars-hook 'lsp-rust-enable)))
