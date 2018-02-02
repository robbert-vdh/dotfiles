;;; private/robbert/+bindings.el -*- lexical-binding: t; -*-

;; FIXME: Keybindings with shift + meta are broken, so they use this weird
;;        vector notation

(map!
 [remap dabbrev-expand] #'hippie-expand

 :ni "C-S-SPC" #'company-yasnippet
 :i "RET"      #'+robbert/newline-and-indent
 :i [(meta return)]   #'doom/newline-and-indent ;;
 :ne [(shift meta f)] #'counsel-rg ;; As a complement to the `M-f' Swiper defined in +defualt
 :ni "M-p"     #'+robbert/indent-pasted-text
 :nvie "M-q"   #'fill-paragraph ;; Doom Emacs overrides this to quit by default
 :v "C-u"      #'evil-scroll-up ;; `evil-want-C-u-scroll' doesn't do anything in visual mode

 :m "[a" #'+robbert/languagetool-previous-error
 :m "]a" #'+robbert/langaugetool-next-error

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
     :desc "Delete current file"     :n "k"  #'+robbert/delete-file-and-buffer)

   (:desc "git"                      :prefix "g"
     :desc "Git blame (follow copy)" :n "b"  #'+robbert/magit-blame-follow-copy
     :desc "Git status"              :n "s"  #'magit-status
     :desc "Git log current file"    :n "l"  #'magit-log-buffer-file
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
     "C-a"        #'company-abort
     "C-l"        #'company-complete
     [tab]        #'+robbert/company-select-next-or-complete
     "RET"        nil
     [escape]     nil))

 (:after diff-mode
   (:map diff-mode-map
     :nm "{"      #'diff-hunk-prev
     :nm "}"      #'diff-hunk-next))

 (:after evil-org
   (:map evil-org-mode-map
     :ni "M-o"    #'+robbert/evil-org-always-open-below
     :ni "M-RET"  #'+robbert/evil-org-always-open-below)
   ;; Not sure why, but org somehow overrides the binding without this
   (:map org-mode-map
     :nmi [remap outline-insert-heading] #'+robbert/evil-org-always-open-below))

 (:after flycheck
   (:map flycheck-error-list-mode-map
     :m "M-RET"   #'flycheck-error-list-explain-erro))

 (:after helpful
   (:map helpful-mode-map
     :m "q"       #'quit-window
     :m "ZZ"      #'quit-window
     :m "ZQ"      #'quit-window))

 (:after ivy
   (:map ivy-minibuffer-map
     "C-d"        #'ivy-scroll-up-command
     "C-u"        #'ivy-scroll-down-command))

 (:after term
   (:map term-raw-map
     ;; Manage multiple terminals
     "M-j"        #'multi-term-next
     "M-k"        #'multi-term-prev
     "M-n"        #'multi-term
     ;; Allow for window bindings in term-mode
     "C-w"        evil-window-map)))
