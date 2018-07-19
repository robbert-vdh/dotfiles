;; -*- no-byte-compile: t; -*-
;;; ~/.config/doom/packages.el

(package! atomic-chrome)
(package! company-racer :disable t :ignore t) ;; racer's capf backend is better
(package! ein)
(package! evil-lion)
(package! flycheck-rust)
(package! ggtags)
(package! kotlin-mode)
(package! page-break-lines)
(package! phpcbf)
(package! pipenv)
(package! pkgbuild-mode)
(package! racer :disable t :ignore t) ;; Handled through rls instead
(package! yapfify)

;; +lsp
(package! company-lsp)
(package! lsp-css :recipe (:fetcher github :repo "emacs-lsp/lsp-css"))
(package! lsp-intellij)
(package! lsp-mode)
(package! lsp-python)
(package! lsp-rust)
(package! lsp-ui)
