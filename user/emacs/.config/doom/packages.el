;; -*- no-byte-compile: t; -*-
;;; ~/.config/doom/packages.el

(package! atomic-chrome)
(package! company-lsp)
(package! company-racer :disable t :ignore t) ;; racer's capf backend is better
(package! ein)
(package! evil-lion)
(package! fish-mode)
(package! flycheck-rust)
(package! ggtags)
(package! lsp-css :recipe (:fetcher github :repo "emacs-lsp/lsp-css"))
(package! lsp-mode)
(package! lsp-rust)
(package! lsp-ui)
(package! page-break-lines)
(package! phpcbf)
(package! pkgbuild-mode)
(package! racer :disable t :ignore t) ;; Handled through rls instead
(package! racer)
(package! yapfify)
