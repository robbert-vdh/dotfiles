;; -*- no-byte-compile: t; -*-
;;; ~/.config/doom/packages.el

(package! academic-phrases)
(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
(package! ein)
(package! ggtags)
(package! highlight-numbers)
(package! kotlin-mode)
(package! meson-mode)
(package! ob-ipython)
(package! page-break-lines)
(package! pkgbuild-mode)
(package! rainbow-delimiters)
(package! strace-mode)
(package! vue-mode)

(package! magit-delta)
(package! magit-todos)

;; I prefer instant feedback over slight improvements in resposniveness
(package! flyspell-lazy :disable t)
