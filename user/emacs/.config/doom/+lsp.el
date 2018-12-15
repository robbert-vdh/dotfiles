;;; ~/.config/doom/+lsp.el -*- lexical-binding: t; -*-

;;; Core modes

(def-package! lsp-mode
  ;; lsp-mode supports a lot of modes out of the box, but for some languages,
  ;; like Python and Haskell, I prefer to use the non-LSP tooling
  :hook ((css-mode . lsp)
         (html-mode . lsp)
         (mhtml-mode . lsp)
         (php-mode . lsp)
         (rust-mode . lsp)
         (sgml-mode . lsp)
         (shell-mode . lsp)
         (web-mode . lsp))
  :init (require 'lsp-clients)
  :config
  ;; Integrate lsp-mode into Doom's awesome UI
  (set-lookup-handlers! 'lsp--managed-mode :documentation #'lsp-info-under-point)
  ;; Use the LSP's own formatter instead formal-all
  (add-hook 'lsp--managed-mode #'+robbert/lsp-format-before-save)

  ;; Don't highlight symbols automatically, use `gh' to do this manually
  (remove-hook 'lsp-eldoc-hook #'lsp-document-highlight)

  ;; FIXME: Hotfix until this gets added to lsp.el again
  (add-to-list 'lsp-language-id-configuration '(less-mode . "less"))
  (add-to-list 'lsp-language-id-configuration '(sass-mode . "sass"))
  (add-to-list 'lsp-language-id-configuration '(scss-mode . "scss"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("css-languageserver" "--stdio"))
                    :major-modes '(less-mode sass-mode scss-mode)
                    :action-handlers (lsp-ht ("_css.applyCodeAction" 'lsp-clients-css--apply-code-action))
                    :server-id 'scss-ls))

  ;; Mode-specific configuration

  ;; Enable clippy support
  (add-hook! :append 'rust-mode-hook
    (let ((preferences (make-hash-table)))
      (puthash "clippy_preference" "on" preferences)
      (lsp--set-configuration `(:rust ,preferences))))

  ;; `lsp-mode' overrides our tags here, but we need those for variable name
  ;; completions as `lsp-css' isn't that smart yet
  (add-hook! :append 'scss-mode-hook
    (setq company-backends '(:separate company-lsp company-capf)
          completion-at-point-functions '(ggtags-completion-at-point))))

;; Auto loaded by lsp-mode
(def-package! lsp-ui
  :defer t
  :config
  (setq lsp-prefer-flymake nil ;; Use regular old flycheck
        lsp-ui-doc-use-childframe nil ;; This tends to obscure whatever you're editing
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-hover nil))
