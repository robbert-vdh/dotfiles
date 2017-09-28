;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ivy
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup nil
                      auto-completion-tab-key-behavior 'complete)
     ;; (c-c++ :variables
     ;;        c-c++-default-mode-for-headers 'c++-mode
     ;;        c-c++-enable-clang-support t
     ;;        c-c++-enable-clang-format-on-save t)
     ;; c-c++-irony
     ;; java
     colors
     csv
     docker
     emacs-lisp
     git
     graphviz
     gtags
     html
     javascript
     latex
     (languagetool :variables
                   langtool-default-language "nl"
                   languagetool-show-error-on-jump t
                   langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")
     lsp
     major-modes
     markdown
     nginx
     nlinum
     (org :variables
          org-enable-bootstrap-support t)
     ;; platformio
     php
     (python :variables
             python-enable-yapf-format-on-save t)
     (rust :variables
           rust-format-on-save t)
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     shell-scripts
     ;; php
     (spell-checking :variables
                     flyspell-default-dictionary "nl"
                     spell-checking-enable-by-default nil)
     typescript
     syntax-checking
     version-control
     yaml
     vue)
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(scss-mode)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n` to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font `("Input Mono Narrow"
                               :size ,(if (equal system-name "laptop") 18 16)
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t
   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands.
   dotspacemacs-auto-generate-layout-names nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non-nil the paste micro-state is enabled. When enabled pressing `p'
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers '(:relative t :size-limit-kb 1000 :enabled-for-modes
                                         text-mode
                                         prog-mode
                                         typescript-parent-mode
                                         web-mode-prog-mode)
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   dotspacemacs-frame-title-format "%a"
   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed
   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil
   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  ;; Underscores should be part of a word
  (modify-syntax-entry ?_ "w"))

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
  (setq completion-styles '(partial-completion initials))

  ;; Fix the highlighted line color
  (defun fix-color-scheme (&rest frame)
    (set-face-attribute 'nlinum-relative-current-face nil
                        :background nil
                        :foreground "#5E6A76"))
  (add-to-list 'focus-in-hook 'fix-color-scheme)

  ;; Set default indentation levels
  (setq css-indent-offset 2
        js-indent-level 2
        sh-basic-offset 2
        sh-indentation 2
        typescript-indent-level 2
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2)

  ;; Improve scrolling behaviour by recentering the screen when jumping to
  ;; something off screen
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 3) ((control)))
        scroll-conservatively 3
        scroll-margin 3)

  ;; Use C++14 by default when using Irony for error checking
  (setq irony-additional-clang-options '("-std=c++14")
        flycheck-clang-language-standard "c++14"
        flycheck-gcc-language-standard "c++14")

  ;; Make sure racer can find Rust's source files and disable gtags as Rust's
  ;; Language Server does a better job already
  (setq racer-rust-src-path
        "~/.multirust/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
  (setq xref-prompt-for-identifier nil)
  (remove-hook 'rust-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)

  ;; The default 'fd' gives a lote of trouble when writing in Dutch
  (setq evil-escape-key-sequence "jk")

  ;; Spell checking should be enabled in git commits
  (add-hook 'text-mode-hook 'spacemacs/toggle-spelling-checking)
  ;; Disable for config and .vue files
  (add-hook 'conf-mode-hook 'spacemacs/toggle-spelling-checking)
  (add-hook 'vue-mode-hook 'spacemacs/toggle-spelling-checking)

  ;; Always highlight color codes in CSS
  (add-hook 'css-mode-hook 'rainbow-mode)
  ;; rainbow-mode incorectly identifies parts of variable names as color names,
  ;; and they aren't very useful anyway
  (add-hook 'css-mode-hook (lambda () (setq-local rainbow-html-colors-alist '())))
  (with-eval-after-load 'rainbow-mode
    (add-to-list 'rainbow-html-colors-major-mode-list 'scss-mode))

  ;; Use proper comment syntax
  (setq web-mode-comment-style 2)

  ;; Prefer the PSR-2 coding style in PHP
  (setq php-mode-coding-style 'psr2)

  ;; Different languages use different line lengths (there's probably a better
  ;; way to keep the variable value in the lambda)
  (loop for (mode . value) in '((php-mode-hook . 120)
                                (rust-mode-hook . 100))
        do (add-hook mode `(lambda () (setq fill-column ,value))))

  ;; Enable gtags for Sass. Pygments has got a parser that works great, but it
  ;; doesn't use the dollar sign prefix. We'll have to manually add the jump
  ;; handler to scss-mode as there are not any yet.
  (add-hook 'scss-mode-hook (lambda () (modify-syntax-entry ?$ "'")
                              (modify-syntax-entry ?% ".")))
  (spacemacs|define-jump-handlers scss-mode)
  (spacemacs|add-company-backends :backends company-capf :modes scss-mode)
  (add-hook 'scss-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)
  (spacemacs/counsel-gtags-define-keys-for-mode 'scss-mode)

  ;; Fix jumping to Sass files when the leading underscore is ommitted
  (add-to-list 'ffap-alist
               '(scss-mode . (lambda (filename) (replace-regexp-in-string
                                                 "^\\(.*?\\)\\([^/]+\\)$"
                                                 "\\1_\\2.scss"
                                                 filename))))

  ;; Sort global results by nearness. This helps when editing Sass, as the
  ;; default variables will have a lower priority.
  (setq ggtags-sort-by-nearness t)
  (setq counsel-gtags-path-style 'relative)
  (with-eval-after-load 'counsel-gtags
    (setq counsel-gtags--complete-options
          (loop for (type . options) in counsel-gtags--complete-options
                collect (cons type (concat options "N"))))
    (add-to-list 'counsel-gtags--complete-options '(definition . "-N"))
    (add-to-list 'counsel-gtags--complete-options '(from-here . "-N")))

  ;; Python docstrings should always be on multiple lines
  (setq python-fill-docstring-style 'django)

  ;; LaTeX previews should reload automatically
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)

  ;; Org mode should use komascript for LaTeX exports and code fragments should be colored
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
                 '("koma-article"
                   "\\documentclass[parskip=half]{scrartcl}
                    [DEFAULT-PACKAGES] [PACKAGES]
                    \\setminted{frame=leftline,framesep=1em,linenos,numbersep=1em,style=pastie}
                    \\setminted[python]{python3}
                    [EXTRA]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-packages-alist '("dutch" "babel"))
    (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))
    (setq org-latex-default-class "koma-article"
          org-format-latex-options
          (plist-put org-format-latex-options
                     :scale (if (equal system-name "laptop") 1.5 1.25))
          org-latex-caption-above nil
          org-latex-listings 'minted
          ;; latexmk tends to play along nicer than pdflatex
          org-latex-pdf-process '("latexmk -f -pdf %f")))
  (setq org-agenda-files '("~/Documenten/notes"))
  ;; Highlight math snippets
  (setq org-highlight-latex-and-related '(latex script entities))
  ;; Allow M-* keybindings in insert mode
  (setq evil-org-use-additional-insert t)

  ;; Workaround for C-k not working as expected when using company quickhelp
  ;; See: https://github.com/syl20bnr/spacemacs/issues/2974#issuecomment-316319212
  (add-hook
   'company-completion-started-hook
   (lambda (&rest ignore)
     (when evil-mode
       (when (evil-insert-state-p)
         (define-key evil-insert-state-map (kbd "C-k") nil)))))

  ;; Add a keybinding for opening the company completions (ignores the minimum
  ;; prefix length)
  (define-key evil-insert-state-map (kbd "C-SPC") 'company-complete)

  ;; Hide unimported minor modes
  (spacemacs|hide-lighter ggtags-mode)
  (spacemacs|hide-lighter lsp-mode)
  (spacemacs|hide-lighter magit-gitflow-mode)
  (spacemacs|hide-lighter reftex-mode)

  ;; Custom functions

  ;; Add keybindings for locating a file in a directory or in the current
  ;; project, even when it's ignored.
  (defun magit-blame-follow-copy ()
    "Blame with the `-wCCC' options, telling Git to track copied
text"
    (interactive)
    (magit-blame magit-buffer-refname buffer-file-name '("-wCCC")))

  (defun find-file-in-dir ()
    (interactive)
    (setq current-prefix-arg '(4))
    (call-interactively 'counsel-file-jump))

  (defun find-file-in-project ()
    (interactive)
    (counsel-file-jump nil (projectile-project-root)))

  (spacemacs/set-leader-keys
    "ob" 'magit-blame-follow-copy
    "of" 'find-file-in-dir
    "op" 'find-file-in-project)

  (defun org-edit-src-fill-column (column)
    "Sets the fill column in Org src blocks to prevent them being
too wide in exported PDFs"
    (interactive "P")
    (setq fill-column (or column 70))
    (spacemacs/toggle-fill-column-indicator-on))

  (spacemacs/set-leader-keys-for-minor-mode 'org-src-mode
    "of" 'org-edit-src-fill-column))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(evil-want-Y-yank-to-eol nil)
 '(package-selected-packages
   (quote
    (vue-mode edit-indirect ssass-mode vue-html-mode lsp-rust lsp-mode ivy-rich hl-line+ rainbow-mode rainbow-identifiers color-identifiers-mode counsel-css counsel-gtags helm-purpose org-category-capture tide typescript-mode wolfram-mode vala-snippets vala-mode thrift stan-mode scad-mode qml-mode pkgbuild-mode matlab-mode logcat kivy-mode julia-mode hoon-mode ebuild-mode arduino-mode linum-relative origami helm-gtags ggtags helm-pydoc helm-gitignore helm-css-scss helm-company helm-c-yasnippet meghanada groovy-mode groovy-imports pcache gradle-mode ensime sbt-mode scala-mode company-emacs-eclim eclim impatient-mode org-brain omnisharp shut-up csharp-mode evil-org add-node-modules-path flycheck-bashate company-php ac-php-core xcscope pdf-tools evil-lion zenburn-theme password-generator realgud test-simple loc-changes load-relative stickyfunc-enhance srefactor seq powerline spinner log4e gntp org-plus-contrib nlinum markdown-mode skewer-mode simple-httpd multiple-cursors window-purpose imenu-list hydra parent-mode haml-mode fringe-helper git-gutter+ git-gutter flyspell-correct flx git-commit with-editor smartparens iedit anzu evil goto-chg undo-tree highlight magit-popup json-snatcher json-reformat diminish projectile pkg-info epl swiper ivy web-completion-data dash-functional tern pos-tip company rust-mode bind-map bind-key yasnippet packed anaconda-mode pythonic f dash avy auto-complete popup auctex langtool async js2-mode counsel flycheck helm helm-core irony alert magit php-mode s nginx-mode dockerfile-mode docker tablist docker-tramp window-numbering jade-mode ido-vertical-mode quelpa package-build spacemacs-theme yapfify yaml-mode xterm-color ws-butler winum which-key wgrep web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tagedit symon string-inflection spaceline smex smeargle slim-mode shell-pop scss-mode sass-mode restart-emacs request rainbow-delimiters racer pyvenv pytest pyenv-mode py-isort pug-mode popwin platformio-mode pip-requirements phpunit phpcbf php-extras php-auto-yasnippets persp-mode pcre2el paradox ox-twbs orgit org-projectile org-present org-pomodoro org-download org-bullets open-junk-file nlinum-relative neotree multi-term move-text mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode link-hint less-css-mode json-mode js2-refactor js-doc ivy-purpose ivy-hydra irony-eldoc insert-shebang info+ indent-guide hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-make graphviz-dot-mode google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md fuzzy flyspell-correct-ivy flycheck-rust flycheck-pos-tip flycheck-irony flx-ido fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav editorconfig dumb-jump drupal-mode disaster diff-hl define-word cython-mode csv-mode counsel-projectile company-web company-tern company-statistics company-shell company-quickhelp company-irony-c-headers company-irony company-c-headers company-auctex company-anaconda column-enforce-mode coffee-mode cmake-mode clean-aindent-mode clang-format cargo browse-at-remote auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk aggressive-indent adaptive-wrap ace-window ace-link ac-ispell)))
 '(safe-local-variable-values (quote ((yaml-indent-offset . 4)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
)
