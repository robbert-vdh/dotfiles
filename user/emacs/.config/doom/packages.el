;; -*- no-byte-compile: t; -*-
;;; ~/.config/doom/packages.el

(package! atomic-chrome)
(package! ein)
(package! evil-lion)
(package! ggtags)
(package! kotlin-mode)
(package! page-break-lines)
(package! phpcbf)
(package! pkgbuild-mode)
(package! racer :disable t :ignore t) ;; Handled through rls instead

;; +lsp
(package! company-lsp)
(package! lsp-css :recipe (:fetcher github :repo "emacs-lsp/lsp-css"))
(package! lsp-intellij)
(package! lsp-mode)
(package! lsp-rust)
(package! lsp-ui)
