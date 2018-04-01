;;; ~/.config/doom/+bindings.el -*- lexical-binding: t; -*-

;; FIXME: Keybindings with shift + meta are broken, so they use this weird
;;        vector notation

(map!
 [remap dabbrev-expand] #'hippie-expand
 [remap newline]        #'+robbert/newline-and-indent
 :i [M-return]          #'newline-and-indent ;; The default is adviced to continue comments

 :ni "C-S-SPC"          #'company-yasnippet
 :ne [(shift meta f)]   #'counsel-rg         ;; As a complement to the `M-f' Swiper defined in +defualt
 :nvie "M-q"            #'fill-paragraph     ;; Doom Emacs overrides this to quit by default
 :v "C-u"               #'evil-scroll-up     ;; `evil-want-C-u-scroll' doesn't do anything in visual mode

 :m "[a"                #'+robbert/languagetool-previous-error
 :m "]a"                #'+robbert/languagetool-next-error

 ;; Override for the default config, this breaks magit
 :n  "z"                nil

 (:leader
   (:desc "buffer"                    :prefix "b"
     :desc "Replace with clipboard"   :n "P"  #'+robbert/clipboard-to-buffer
     :desc "Revert"                   :n "R"  #'revert-buffer
     :desc "Copy to clipboard"        :n "Y"  #'+robbert/buffer-to-clipboard)

   (:desc "file"                      :prefix "f"
     :desc "Find file in dotfiles"    :n "d"  #'+robbert/find-in-dotfiles
     :desc "Browse dotfiles"          :n "D"  #'+robbert/browse-dotfiles
     :desc "Delete current file"      :n "k"  #'doom/delete-this-file
     :desc "Move current file"        :n "m"  #'doom/move-this-file
     :desc "Copy current file"        :n "M"  #'doom/copy-this-file
     :desc "Open file externally"     :n "x"  #'counsel-find-file-extern)

   (:desc "git"                       :prefix "g"
     :desc "Browse in revision"       :n "."  #'magit-find-file
     :desc "Git blame (follow copy)"  :n "b"  #'+robbert/magit-blame-follow-copy
     :desc "SMerge hydra"             :n "m"  #'+hydra-smerge/body
     :desc "Git log current file"     :n "l"  #'magit-log-buffer-file
     :desc "Git status"               :n "s"  #'magit-status
     :desc "Git stage hunk"           :n "S"  #'git-gutter:stage-hunk)

   (:desc "open"                      :prefix "o"
     (:desc "+jupyter"                :prefix "j"
       :desc "Open in browser"        :nv "b" #'ein:notebook-open-in-browser
       :desc "Open this file"         :n "f"  #'ein:notebooklist-open-notebook-by-file-name
       :desc "Login and open"         :n "o"  #'ein:jupyter-server-login-and-open
       :desc "Start server"           :n "s"  #'ein:jupyter-server-start))

   (:desc "project"                   :prefix "p"
     :desc "Find file in proejct"     :nv "." #'counsel-projectile-find-file
     :desc "Ripgrep in project"       :nv "/" #'counsel-projectile-rg
     :desc "Open terminal in project" :n  "t" #'+term/open-popup-in-project
     :desc "List project tasks"       :n  "T" #'+ivy/tasks)

   (:desc "search"                    :prefix "/"
     :desc "Project"                  :nv "/" #'counsel-rg
     :desc "Find in directory"        :nv "f" #'+robbert/find-file-in-dir
     :desc "Find in project"          :nv "p" #'+robbert/find-file-in-project
     :desc "Buffer"                   :nv "s" #'swiper)

   (:desc "toggle"                    :prefix "t"
     :desc "Change dictionary"        :n "S"  #'ispell-change-dictionary
     :desc "LanguageTool"             :n "t"  #'+robbert/languagetool-toggle
     :desc "LanguageTool correct"     :n "T"  #'langtool-correct-buffer))

 (:after company
   (:map company-active-map
     "C-a"    #'company-abort
     "C-l"    #'company-complete
     [tab]    #'+robbert/company-select-next-or-complete
     "C-/"    #'counsel-company ;; Search the candidates using ivy
     "RET"    nil
     [escape] nil))

 (:after diff-mode
   (:map diff-mode-map
     :nm "{" #'diff-hunk-prev
     :nm "}" #'diff-hunk-next))

 (:after ein-multilang
   (:map ein:notebook-multilang-mode-map
     :ni [C-return] #'ein:worksheet-execute-cell
     :ni [S-return] #'ein:worksheet-execute-cell-and-goto-next
     :n  "gj"       #'ein:worksheet-goto-next-input
     :n  "gk"       #'ein:worksheet-goto-prev-input
     :nv "M-j"      #'ein:worksheet-move-cell-down
     :nv "M-k"      #'ein:worksheet-move-cell-up
     :nv "C-s"      #'ein:notebook-save-notebook-command
     (:localleader
       :nv "y"     #'ein:worksheet-copy-cell
       :nv "p"     #'ein:worksheet-yank-cell
       :nv "d"     #'ein:worksheet-kill-cell)))

 (:after ein-traceback
   (:map ein:traceback-mode-map
     (:localleader
       :nv "RET" #'ein:tb-jump-to-source-at-point-command
       :nv "n"   #'ein:tb-next-item
       :nv "p"   #'ein:tb-prev-item
       :nv "q"   #'bury-buffer)))

 (:after evil-org
   (:map evil-org-mode-map
     :ni [M-return] #'+robbert/evil-org-always-open-below))

 (:after flycheck
   (:map flycheck-error-list-mode-map
     :m [M-return] #'flycheck-error-list-explain-erro))

 (:after helpful
   (:map helpful-mode-map
     :m "q"  #'quit-window
     :m "ZZ" #'quit-window
     :m "ZQ" #'quit-window))

 (:after ivy
   (:map ivy-minibuffer-map
     "C-d"         #'ivy-scroll-up-command
     "C-u"         #'ivy-scroll-down-command)

   (:map ivy-occur-mode-map
     :n "<return>" #'ivy-occur-press-and-switch
     :n "C-SPC"    #'ivy-occur-press
     :n [M-return] #'ivy-occur-press
     :n "j"        #'ivy-occur-next-line
     :n "k"        #'ivy-occur-previous-line
     :n "ga"       #'ivy-occur-read-action
     :n "M-o"      #'ivy-occur-dispatch
     :n "gr"       #'ivy-occur-revert-buffer
     :n "q"        #'quit-window))

 (:after omnisharp
   (:map omnisharp-mode-map
     :nv [M-return]                     #'omnisharp-run-code-action-refactoring
     (:localleader
       :desc "Refactor this"  :nv "SPC" #'omnisharp-run-code-action-refactoring
       :desc "Restart server" :n  "s"   #'omnisharp-start-omnisharp-server)))

 ;; Python offers some nicer ways to work with REPLs
 (:after python
   (:map python-mode-map
     (:localleader
       (:desc "REPL send"  :prefix "r"
         :desc "Buffer"    :nv "b" #'python-shell-send-buffer
         :desc "Function"  :nv "f" #'python-shell-send-defun
         :desc "Region"    :nv "r" #'python-shell-send-region))))

 ;; scss-mode is built in so we can't use a use-package hook
 (:map* scss-mode-map
   (:localleader
     :desc "Generate tags" :nv "t" #'+robbert/generate-scss-tags))

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
       ;; TODO: Add Doom's refactor menu
       :desc "JSDoc template" :nv "c"   #'tide-jsdoc-template
       :desc "Documentation"  :nv "h"   #'tide-documentation-at-point
       :desc "Fix imports"    :nv "i"   #'tide-organize-imports
       :desc "Rename"         :nv "r"   #'tide-rename-symbol
       :desc "Refactor this"  :nv "R"   #'tide-refactor
       :desc "Fix issue"      :nv "SPC" #'tide-fix
       :desc "Restart"        :n "s"    #'tide-restart-server)))

 (:after web-mode
   (:map web-mode-map
     "M-/" nil))

 ;; Disable evil-collection overrides
 (:map compilation-mode-map
   :n  "SPC" nil
   :nv "C-h" nil
   :nv "C-j" nil
   :nv "C-k" nil
   :nv "C-l" nil)
 (:after dired
   (:map dired-mode-map
     :n "[" nil
     :n "]" nil
     :n "{" #'dired-prev-dirline
     :n "}" #'dired-next-dirline))
 (:after python
   (:map python-mode-map
     :n "gz" nil)))
