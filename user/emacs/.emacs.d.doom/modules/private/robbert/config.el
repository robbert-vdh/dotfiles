;;; private/robbert/config.el -*- lexical-binding: t; -*-

(load! +bindings)
(load! +settings)

(def-package! evil-collection
  :after evil
  :config (evil-collection-init))
