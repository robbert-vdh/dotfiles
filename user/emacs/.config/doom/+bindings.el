;;; ~/.config/doom/+bindings.el -*- lexical-binding: t; -*-

;; FIXME: Keybindings with shift + meta are broken, so they use this weird
;;        vector notation

(map!
 [remap dabbrev-expand] #'hippie-expand
 :gi [remap newline]    #'+robbert/newline-and-indent
 :gi [M-return]         #'newline-and-indent         ;; The default is adviced to continue comments

 ;; These keybindigns are no longer defaults, but they're still very useful
 :gi [M-backspace]      #'doom/backward-kill-to-bol-and-indent
 :g "M-f"               #'swiper-helm
 :g "M-F"               #'+default/search-project
 :gnvi "M-Q"            #'+robbert/unfill-paragraph

 :gni "C-S-SPC"         #'company-yasnippet

 ;; These keybindigns don't make a lot of sense, but they're easy to use and not
 ;; in use for anything else
 :m "[v"                #'+robbert/languagetool-previous-error
 :m "]v"                #'+robbert/languagetool-next-error

 (:leader
   (:prefix "b"
     :desc "New buffer"               "c"  #'+default/new-buffer
     :desc "Replace with clipboard"   "P"  #'+robbert/clipboard-to-buffer
     :desc "Revert"                   "R"  #'revert-buffer
     :desc "Copy to clipboard"        "Y"  #'+robbert/buffer-to-clipboard)

   (:prefix "f"
     :desc "Find file in dotfiles"    "d"  #'+robbert/find-in-dotfiles
     :desc "Browse dotfiles"          "D"  #'+robbert/browse-dotfiles
     :desc "Copy file"                "M"  #'doom/copy-this-file)

   (:prefix "g"
     :desc "Browse in revision"       "."  #'magit-find-file
     :desc "Git blame (follow copy)"  "b"  #'+robbert/magit-blame-follow-copy
     :desc "SMerge hydra"             "m"  #'+hydra-smerge/body)

   (:prefix "o"
     :desc "R (ESS)"                  "R" #'R
     ;; TODO: Use `:prefix-map' after this gets fixed
     (:prefix ("j" . "jupyter")
       :desc "Open in browser"        "b" #'ein:notebook-open-in-browser
       :desc "Open this file"         "f"  #'ein:notebooklist-open-notebook-by-file-name
       :desc "Login and open"         "o"  #'ein:jupyter-server-login-and-open
       :desc "Start server"           "s"  #'ein:jupyter-server-start))

   (:prefix "p"
     :desc "Open terminal in project" "t" #'+vterm/open-popup-in-project
     :desc "List project tasks"       "T" #'+ivy/tasks)

   (:prefix "/"
     (:prefix ("f" . "find")
       :desc "In directory"           "d" #'+robbert/find-file-in-dir
       :desc "In project"             "p" #'+robbert/find-file-in-project))

   (:prefix "t"
     :desc "Change dictionary"        "S"  #'ispell-change-dictionary
     :desc "LanguageTool"             "t"  #'+robbert/languagetool-toggle
     :desc "LanguageTool correct"     "T"  #'langtool-correct-buffer
     :desc "Line wrapping"            "w"  #'+robbert/enable-wrapping))

 (:after agda2-mode
   (:map agda2-mode-map
     "C-c w" #'+robbert/agda-insert-with
     (:localleader
      :desc "Insert 'with'" "w" #'+robbert/agda-insert-with)))

 (:after company
   (:map company-active-map
     "C-a"    #'company-abort
     "C-l"    #'company-complete
     [escape] nil))

 (:after diff-mode
   (:map diff-mode-map
     :nm "{" #'diff-hunk-prev
     :nm "}" #'diff-hunk-next))

 (:after ein-multilang
   (:map ein:notebook-multilang-mode-map
     :ni  [C-return] #'ein:worksheet-execute-cell
     :ni  [S-return] #'ein:worksheet-execute-cell-and-goto-next
     :nvi [backtab]  #'ein:pytools-request-tooltip-or-help
     :n   "gj"       #'ein:worksheet-goto-next-input
     :n   "gk"       #'ein:worksheet-goto-prev-input
     :nv  "M-j"      #'ein:worksheet-move-cell-down
     :nv  "M-k"      #'ein:worksheet-move-cell-up
     :nv  "C-s"      #'ein:notebook-save-notebook-command
     (:localleader
       "y" #'ein:worksheet-copy-cell
       "p" #'ein:worksheet-yank-cell
       "d" #'ein:worksheet-kill-cell)))

 (:after ein-traceback
   (:map ein:traceback-mode-map
     (:localleader
       "RET" #'ein:tb-jump-to-source-at-point-command
       "n"   #'ein:tb-next-item
       "p"   #'ein:tb-prev-item
       "q"   #'bury-buffer)))

 (:after emmet-mode
   (:map emmet-mode-keymap
     :i [backtab] #'emmet-expand-line))

 (:after ess
   (:map ess-mode-map
     ;; Don't do the roxygen continuation, as this collides with Doom's own
     ;; comment continuation.
     "RET" nil))

 (:after evil
   ;; This makes the up and down motions in visual line mode handle like the
   ;; regular motions
   (:map visual-line-mode-map
     :o [remap evil-next-line] #'evil-next-line
     :o [remap evil-previous-line] #'evil-previous-line
     :o [remap evil-next-visual-line] #'evil-next-visual-line
     :o [remap evil-previous-visual-line] #'evil-previous-visual-line))

 (:after evil-org
   (:map evil-org-mode-map
     ;; Doom changes c-return to always create new list items when inside of a
     ;; list, but M-return already does this so I prefer the old behaviour
     [C-return] (evil-org-define-eol-command org-insert-heading-respect-content)
     :ni [M-return] #'+robbert/evil-org-always-open-below))

 (:after flycheck
   (:map flycheck-error-list-mode-map
     :m [M-return] #'flycheck-error-list-explain-erro))

 (:after haskell-mode
   (:map haskell-mode-map
     [remap evil-open-above] #'+robbert/haskell-evil-open-above
     [remap evil-open-below] #'+robbert/haskell-evil-open-below))

 (:after helm
   (:map helm-map
     ;; Doom binds C-f and C-S-f, but C-d/C-u is just too comfortable
     "C-d"   #'helm-next-page
     "C-u"   #'helm-previous-page))
 ;; TODO: Fix this
 (:after helm-commands
   (:map helm-M-x-map
     "C-u"   #'helm-previous-page
     "C-S-u" #'helm-M-x-universal-argument))

 (:after helm-ag
   (:map helm-ag-map
     "C-S-k"   #'helm-ag--previous-file
     "C-S-j"   #'helm-ag--next-file))

 ((:after intero
    (:map intero-mode-map
      ;; We can't just set the documentation function here since `intero-info'
      ;; does its own buffer management
      [remap +lookup/documentation] #'intero-info)))

 (:after lsp-ui
   (:map lsp-ui-mode-map
     :nvi [M-return]                  #'lsp-execute-code-action
     :nv  "gh"                        #'lsp-document-highlight
     (:prefix "c"
       :desc "Format (LSP)" "F"       #'lsp-format-buffer)
     (:localleader
       :desc "Rename"       "r"       #'lsp-rename)))

 (:after omnisharp
   (:map omnisharp-mode-map
     :nv [M-return]                 #'omnisharp-run-code-action-refactoring
     (:localleader
       :desc "Refactor this"  "SPC" #'omnisharp-run-code-action-refactoring
       :desc "Restart server" "s"   #'omnisharp-start-omnisharp-server)))

 ;; Python offers some nicer ways to work with REPLs
 (:after python
   (:map python-mode-map
     (:localleader
       (:prefix ("r" . "REPL send")
         :desc "Buffer"   "b" #'python-shell-send-buffer
         :desc "Function" "f" #'python-shell-send-defun
         :desc "Region"   "r" #'python-shell-send-region))))

 (:map scss-mode-map
   (:localleader
     :desc "Generate tags" "t" #'+robbert/generate-scss-tags))

 (:after term
   (:map term-mode-map
     "C-c C-l"        #'+robbert/term-toggle-line-mode)
   (:map term-raw-map
     ;; Allow for window bindings in term-mode
     "C-w"            evil-window-map
     :i "<return>"    #'term-send-return))

 (:after tide
   (:map tide-mode-map
     :nv [M-return] #'tide-fix
     (:localleader
       :desc "JSDoc template"         "c"   #'tide-jsdoc-template
       :desc "Restart"                "s"   #'tide-restart-server
       :desc "Fix issue"              "RET" #'tide-fix
       :desc "Refactor..."            "SPC" #'tide-refactor)))

 ;; These keybindings tend to cause unwanted behaviour when combined with
 ;; company-tng
 (:after yasnippet
   (:map yas-minor-mode-map
     :ig "<tab>" nil
     :ig "TAB" nil))

 (:after web-mode
   (:map web-mode-map
     "M-/" nil

     ;; In HTML we DO want to automatically indent broken 'strings', as these
     ;; are likely long attributes like a list of classes
     [remap newline] #'+robbert/newline-and-indent-always))

 ;; Disable evil-collection overrides
 ;; TODO: Check if these are still needed after refactored default/+bindings.el
 (:after comint
   (:map comint-mode-map
     :n "[" nil
     :n "]" nil
     :n "{" #'comint-previous-input
     :n "}" #'comint-next-input))
 (:after compile
   (:map compilation-mode-map
     :n  "SPC"  nil))
 (:after dired
   (:map dired-mode-map
     "SPC"      nil ;; FIXME
     :nm "SPC"  nil ;; FIXME
     :n "["     nil
     :n "]"     nil
     :n "{"     #'dired-prev-dirline
     :n "}"     #'dired-next-dirline))
 (:after outline
   (:map outline-mode-map
     :n "["     nil
     :n "]"     nil
     :n "{"     #'outline-previous-visible-heading
     :n "}"     #'outline-next-visible-heading))
 (:after magit
   (:map magit-diff-mode-map
     :n  "SPC"  nil))
 (:after python
   (:map python-mode-map
     :n "gz"    nil))
 (:after view
   (:map view-mode-map
     :n "SPC"   nil
     :n "S-SPC" nil)))
