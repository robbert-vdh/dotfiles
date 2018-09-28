;;; ~/.config/doom/+lsp.el -*- lexical-binding: t; -*-

;;; Core modes

(def-package! lsp-mode
  :config
  ;; Don't highlight symbols automatically, use `gh' to do this manually
  (setq lsp-highlight-symbol-at-point nil)
  (set-lookup-handlers! 'lsp-mode :documentation #'lsp-info-under-point))

(def-package! lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (set-formatter! 'lsp-mode #'lsp-format-buffer)
  (setq lsp-ui-doc-position 'bottom
        lsp-ui-sideline-show-flycheck nil
        lsp-ui-sideline-show-symbol nil))

(def-package! company-lsp
  :after (company lsp-mode)
  :config
  (set-company-backend! 'lsp-mode #'company-lsp)
  ;; lsp slows down company by a lot
  (setq-hook! 'lsp-mode-hook company-idle-delay 0.2))

;;; Language support

(def-package! lsp-css
  :after lsp-mode
  :hook ((css-mode . +robbert//lsp-css-enable)
         (less-mode . lsp-less-enable)
         (sass-mode . lsp-sass-enable)
         (scss-mode . lsp-scss-enable))
  :config
  (defun +robbert//lsp-css-enable ()
    "Don't enable lsp-css in derived modes."
    (when (eq major-mode 'css-mode) (lsp-css-enable)))

  ;; `lsp-mode' overrides our tags here, but we need those for variable name
  ;; completions as `lsp-css' isn't that smart yet
  (set-company-backend! 'scss-mode '(:separate company-lsp company-capf))
  (add-hook! 'lsp-after-open-hook
    (when (eq major-mode 'scss-mode)
      ;; `lsp-mode' overrides our tags here, but we need those for variable name
      ;; completions as `lsp-css' isn't that smart yet
      (setq completion-at-point-functions '(ggtags-completion-at-point)))))

(def-package! lsp-intellij
  :hook (kotlin-mode . lsp-intellij-enable))

(def-package! lsp-rust
  :after lsp-mode
  :hook (rust-mode . lsp-rust-enable)
  :config
  ;; Enable clippy support
  (lsp-rust-set-config "clippy_preference" "on"))
