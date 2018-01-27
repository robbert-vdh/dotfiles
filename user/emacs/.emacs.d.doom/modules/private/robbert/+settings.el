;;; modules/private/robbert/+settings.el -*- lexical-binding: t; -*-
;; Configuration for Emacs and existing packages.

(after! company
  ;; Enable automatic auto completion
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2))
