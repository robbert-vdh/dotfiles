;; -*- no-byte-compile: t; -*-
;;; ~/.config/doom/packages.el

(package! academic-phrases)
(package! ein)
(package! ggtags)
(package! kotlin-mode)
(package! meson-mode)
(package! ob-ipython)
(package! page-break-lines)
(package! pkgbuild-mode)
(package! strace-mode)
(package! vue-mode)

;; TODO: Remove when merged upstream
(package! magit-delta)

;; I prefer instant feedback over slight improvements in resposniveness
(package! flyspell-lazy :disable t)

(package! tidal)
(package! tidal-extras :recipe (:repo "https://codeberg.org/bunnylushington/tidal-extras"))
