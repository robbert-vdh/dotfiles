;;; ~/.config/doom/init.el -*- lexical-binding: t; -*-

;; This won't get set otherwise
(def-package-hook! evil :pre-init (setq evil-want-C-u-scroll t) t)
(def-package-hook! evil :post-init (setq evil-want-Y-yank-to-eol nil) t)

(def-package-hook! evil-collection :pre-config
  ;; These keybindings are set by Doom anyway, and it breaks quitting magit
  (delq 'simple evil-collection-mode-list))

;; The tng-frontend should be added before `company-quickhelp' gets loaded, or
;; else it will get overridden
(def-package-hook! company :post-config
  (setq company-frontends
        '(company-tng-frontend
          company-preview-if-just-one-frontend
          company-pseudo-tooltip-unless-just-one-frontend)) t)
