;;; packages.el --- vue layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author:  <robbert@desktop>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `vue-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `vue/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `vue/pre-init-PACKAGE' and/or
;;   `vue/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst vue-packages
  '(edit-indirect
    (vue-mode :location (recipe
                         :fetcher github
                         :repo "codefalling/vue-mode"))))

(defun vue/init-edit-indirect ()
  ;; vue-mode is the only mode using edit-indirect. This should probably be
  ;; moved to the spacemacs-base layer if more layers start requiring
  ;; edit-indirect
  (use-package edit-indirect
    :defer t
    :init
    (spacemacs/set-leader-keys-for-minor-mode 'edit-indirect--overlay
      dotspacemacs-major-mode-leader-key 'edit-indirect-commit
      "c" 'edit-indirect-commit
      "a" 'edit-indirect-abort
      "k" 'edit-indirect-abort)))

(defun vue/init-vue-mode ()
  (use-package vue-mode
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'vue-mode
      "'" 'vue-mode-edit-indirect-at-point)))
