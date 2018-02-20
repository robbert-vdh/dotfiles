;;; ~/.config/doom/+bindings.el -*- lexical-binding: t; -*-

;; FIXME: Keybindings with shift + meta are broken, so they use this weird
;;        vector notation

(map!
 [remap dabbrev-expand] #'hippie-expand

 :ni "C-S-SPC" #'company-yasnippet
 :i "<return>" #'+robbert/newline-and-indent
 :i [M-return] #'newline-and-indent ;; The default is adviced to continue comments
 :ne [(shift meta f)] #'counsel-rg ;; As a complement to the `M-f' Swiper defined in +defualt
 :nvie "M-q"   #'fill-paragraph    ;; Doom Emacs overrides this to quit by default
 :v "C-u"      #'evil-scroll-up    ;; `evil-want-C-u-scroll' doesn't do anything in visual mode

 :m "[a" #'+robbert/languagetool-previous-error
 :m "]a" #'+robbert/languagetool-next-error

 (:leader
   (:desc "search"                   :prefix "/"
     :desc "Ripgrep"                 :nv "/" #'counsel-rg
     :desc "Find in directory"       :nv "f" #'+robbert/find-file-in-dir
     :desc "Find in project"         :nv "p" #'+robbert/find-file-in-project
     :desc "Swiper"                  :nv "s" #'swiper)

   (:desc "buffer"                   :prefix "b"
     :desc "Replace with clipboard"  :n "P"  #'+robbert/clipboard-to-buffer
     :desc "Revert"                  :n "R"  #'revert-buffer
     :desc "Copy to clipboard"       :n "Y"  #'+robbert/buffer-to-clipboard)

   (:desc "file"                     :prefix "f"
     :desc "Find file in dotfiles"   :n "d"  #'+robbert/find-in-dotfiles
     :desc "Browse dotfiles"         :n "D"  #'+robbert/browse-dotfiles
     :desc "Delete current file"     :n "k"  #'doom/delete-this-file
     :desc "Move current file"       :n "m"  #'doom/move-this-file
     :desc "Copy current file"       :n "M"  #'doom/copy-this-file
     :desc "Open file externally"    :n "x"  #'counsel-find-file-extern)

   (:desc "git"                      :prefix "g"
     :desc "Git blame (follow copy)" :n "b"  #'+robbert/magit-blame-follow-copy
     :desc "SMerge hydra"            :n "m"  #'+hydra-smerge/body
     :desc "Git log current file"    :n "l"  #'magit-log-buffer-file
     :desc "Git status"              :n "s"  #'magit-status
     :desc "Git stage hunk"          :n "S"  #'git-gutter:stage-hunk)

   (:desc "project"                  :prefix "p"
     :desc "Find file in proejct"    :nv "." #'counsel-projectile-find-file
     :desc "Ripgrep in project"      :nv "/" #'counsel-projectile-rg)

   (:desc "toggle"                   :prefix "t"
     :desc "Change dictionary"       :n "S"  #'ispell-change-dictionary
     :desc "LanguageTool"            :n "t"  #'+robbert/languagetool-toggle
     :desc "LanguageTool correct"    :n "T"  #'langtool-correct-buffer))

 (:after company
   (:map company-active-map
     "C-a"            #'company-abort
     "C-l"            #'company-complete
     [tab]            #'+robbert/company-select-next-or-complete
     "C-/"            #'counsel-company ;; Search the candidates using ivy
     "RET"            nil
     [escape]         nil))

 (:after diff-mode
   (:map diff-mode-map
     :nm "{"          #'diff-hunk-prev
     :nm "}"          #'diff-hunk-next))

 (:after evil-org
   (:map evil-org-mode-map
     :ni [M-return]   #'+robbert/evil-org-always-open-below))

 (:after flycheck
   (:map flycheck-error-list-mode-map
     :m [M-return] #'flycheck-error-list-explain-erro))

 (:after helpful
   (:map helpful-mode-map
     :m "q"           #'quit-window
     :m "ZZ"          #'quit-window
     :m "ZQ"          #'quit-window))

 (:after ivy
   (:map ivy-minibuffer-map
     "C-d"            #'ivy-scroll-up-command
     "C-u"            #'ivy-scroll-down-command)

   (:map ivy-occur-mode-map
     :n "<return>"    #'ivy-occur-press-and-switch
     :n "C-SPC"       #'ivy-occur-press
     :n [M-return]    #'ivy-occur-press
     :n "j"           #'ivy-occur-next-line
     :n "k"           #'ivy-occur-previous-line
     :n "ga"          #'ivy-occur-read-action
     :n "M-o"         #'ivy-occur-dispatch
     :n "gr"          #'ivy-occur-revert-buffer
     :n "q"           #'quit-window))

 ;; Python offers some nicer ways to work with REPLs
 (:after python
   (:map python-mode-map
     (:localleader
       (:desc "REPL send"  :prefix "r"
         :desc "Buffer"    :nv "b" #'python-shell-send-buffer
         :desc "Function"  :nv "f" #'python-shell-send-defun
         :desc "Region"    :nv "r" #'python-shell-send-region))))

 (:after term
   (:map term-mode-map
     "C-c C-l"        #'+robbert/term-toggle-line-mode)
   (:map term-raw-map
     ;; Allow for window bindings in term-mode
     "C-w"            evil-window-map
     :i "<return>"    #'term-send-return)))
