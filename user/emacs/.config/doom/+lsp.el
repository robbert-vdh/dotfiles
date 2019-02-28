;;; ~/.config/doom/+lsp.el -*- lexical-binding: t; -*-

;; TODO: See what of this is still needed now that LSP support is included in
;;       Doom. I've left the things I'm not sure about commented until I merge
;;       this back into config.el.

(after! lsp-mode
  ;; ;; Integrate lsp-mode into Doom's awesome UI
  ;; (set-lookup-handlers! 'lsp--managed-mode :documentation #'lsp-describe-thing-at-point)
  ;; ;; Use the LSP's own formatter instead formal-all
  ;; (add-hook 'lsp--managed-mode-hook #'+robbert/lsp-format-before-save)

  ;; Don't highlight symbols automatically, I'll use `gh' to do this manually
  (remove-hook 'lsp-eldoc-hook #'lsp-document-highlight)

  ;; Mode-specific configuration

  ;; Enable clippy support
  (add-hook! :append 'rust-mode-hook
    (let ((preferences (make-hash-table)))
      (puthash "clippy_preference" "on" preferences)
      (lsp--set-configuration `(:rust ,preferences))))

  ;; We can't apply our configuration in a simple hook as lsp-mode gets loaded
  ;; asynchronously
  (add-hook! :append 'lsp--managed-mode-hook
    (prin1 major-mode)
    (cond ((derived-mode-p 'scss-mode)
           ;; `lsp-mode' overrides our tags here, but we need those for variable name
           ;; completions as `lsp-css' isn't that smart yet
           (setq company-backends '(:separate company-capf company-lsp)
                 completion-at-point-functions '(ggtags-completion-at-point))))))

;; ;; Auto loaded by lsp-mode
;; (def-package! lsp-ui
;;   :defer t
;;   :config
;;   (setq lsp-prefer-flymake nil ;; Use regular old flycheck
;;         ;; Use Eldoc and the K-key instead since this tends to obscure a lot of information
;;         lsp-ui-doc-enable nil
;;         lsp-ui-sideline-show-diagnostics nil
;;         lsp-ui-sideline-show-hover nil))
