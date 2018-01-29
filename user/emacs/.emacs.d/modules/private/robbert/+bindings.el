;;; private/robbert/+bindings.el -*- lexical-binding: t; -*-

(map!
 :ni "C-S-SPC" #'company-yasnippet
 :i "RET"      #'+robbert/newline-and-indent
 :i "M-RET"    #'doom/newline-and-indent
 :ne [(shift meta f)] #'counsel-rg ;; As a complement to the `M-f' Swiper defined in +defualt
 :ni "M-p"     #'+robbert/indent-pasted-text
 :nvie "M-q"   #'fill-paragraph ;; Doom Emacs overrides this to quit by default
 :v "C-u"      #'evil-scroll-up ;; `evil-want-C-u-scroll' doesn't do anything in visual mode

 (:leader
   (:desc "search"                   :prefix "/"
     :desc "Ripgrep"                 :nv "/" #'counsel-rg
     :desc "Find in directory"       :nv "f" #'+robbert/find-file-in-dir
     :desc "Find in project"         :nv "p" #'+robbert/find-file-in-project
     :desc "Swiper"                  :nv "s" #'swiper)

   (:desc "buffer"                   :prefix "b"
     :desc "Revert"                  :n "R"  #'revert-buffer)

   (:desc "file"                     :prefix "f"
     :desc "Delete current file"     :n "D"  #'+robbert/delete-file-and-buffer)

   (:desc "git"                      :prefix "g"
     :desc "Git blame (follow copy)" :n "b"  #'+robbert/magit-blame-follow-copy
     :desc "Git status"              :n "s"  #'magit-status
     :desc "Git stage hunk"          :n "S"  #'git-gutter:stage-hunk)

   (:desc "Project"                  :prefix "p"
     :desc "Find file in proejct"    :nv "." #'counsel-projectile-find-file
     :desc "Ripgrep in project"      :nv "/" #'counsel-projectile-rg))

 (:after company
   (:map company-active-map
     "C-a"        #'company-abort
     "C-j"        #'company-select-next
     "C-k"        #'company-select-previous
     "C-l"        #'company-complete
     [tab]        #'+robbert/company-select-next-or-complete
     [escape]     nil))

 (:after evil-org
   (:map org-mode-map
     :ni "M-o"    #'+robbert/evil-org-always-open-below
     :ni "M-RET"  #'+robbert/evil-org-always-open-below))

 (:after helpful
   (:map helpful-mode-map
     :m "q"       #'quit-window
     :m "ZZ"      #'quit-window
     :m "ZQ"      #'quit-window)))
