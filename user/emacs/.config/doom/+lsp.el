;;; ~/.config/doom/+lsp.el -*- lexical-binding: t; -*-

;;; Core modes

(def-package! lsp-mode
  :config
  ;; Don't highlight symbols automatically, use `gh' to do this manually
  (setq lsp-highlight-symbol-at-point nil)
  ;; FIXME: Refactor this when `set-lookup-handlers!' supports minor modes
  (set-lookup-handlers! 'rust-mode :documentation 'lsp-info-under-point))

(def-package! lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-position 'bottom
        lsp-ui-sideline-show-flycheck nil))

(def-package! company-lsp
  :after (company lsp-mode)
  :config
  (add-hook! 'lsp-mode-hook (push 'company-lsp company-backends)))

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

  (add-hook! 'lsp-after-open-hook
    (when (eq major-mode 'scss-mode)
      (setq completion-at-point-functions '(ggtags-completion-at-point)
            company-backends '((:separate company-lsp company-capf))))))

(def-package! lsp-python
  :after lsp-mode
  ;; lsp-python only makes sense in larger projects, so this should be enabled
  ;; with through .dir-locals.el or by calling `+robbert/python-enable-lsp'
  ;; which does it for you.
  :config
  ;; Since lsp-python is not enabled globally we'll have to use hooks for the
  ;; rest of the setup
  (add-hook! 'python-mode-hook
    (add-hook! :local 'lsp-mode-hook
      (+robbert/lsp-format-before-save)
      ;; FIXME: Refactor this when `set-lookup-handlers!' supports minor modes
      (add-hook '+lookup-documentation-functions #'lsp-info-under-point nil t)

      ;; Duplicate functionality can be disabled now
      (anaconda-mode -1)
      (anaconda-eldoc-mode -1)
      (yapf-mode -1))))

(def-package! lsp-rust
  :after lsp-mode
  :hook (rust-mode . lsp-rust-enable)
  :config
  ;; Enable clippy support
  (lsp-rust-set-config "clippy_preference" "on")
  ;; Format before saving
  (add-hook 'rust-mode-hook #'+robbert/lsp-format-before-save))
