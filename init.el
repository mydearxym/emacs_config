;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
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
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     go
     windows-scripts
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; evil-visualstar
     spacemacs-helm
     spacemacs-ivy
     better-defaults
     react
     github
     (version-control :variables version-control-diff-tool 'git-gutter+
                      version-control-global-margin t)
     osx
     semantic                           ; too slow
     markdown
     (vinegar :variables vinegar-reuse-dired-buffer t)
     org
     prodigy
     search-engine
     (syntax-checking :variables syntax-checking-enable-by-default nil)
     (spell-checking :variables spell-checking-enable-by-default nil)
     yaml
     ;; (ruby :variables ruby-version-manager 'rvm)
     python
     ;; lua
     themes-megapack
     html
     command-log
     javascript
     ;; restclient
     emacs-lisp
     (clojure :variables clojure-enable-fancify-symbols t)
     ;; emoji
     ;; ycmd
     ;; fasd
     ;; deft
     ;; elfeed
     ranger
     ;; racket
     ;; gtags
     (spacemacs-layouts :variables layouts-enable-autosave t
                        layouts-autosave-delay 300)
     ;; eyebrowse
     (colors :variables
             colors-enable-nyan-cat-progress-bar t)
     (git :variables
          git-magit-status-fullscreen t
          magit-push-always-verify nil
          magit-save-repository-buffers 'dontask
          magit-revert-buffers 'silent
          magit-refs-show-commit-count 'all
          ;; This is really creepy magit
          magit-revision-show-gravatars nil)
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode)
     (auto-completion :variables auto-completion-enable-sort-by-usage t)
     (shell :variables
            shell-default-position 'full
            shell-default-shell 'ansi-term
            shell-default-term-shell "/bin/bash")
     (chinese :variables chinese-default-input-method 'wubi
              chinese-enable-fcitx t
              chinese-enable-youdao-dict t)
     zilongshanren
     guanghui)
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(magit-gh-pulls
                                    magit-gitflow
                                    evil-mc
                                    skewer-mode
                                    vi-tilde-fringe
                                    coffee-mode
                                    ace-jump-helm-line
                                    evil-tutor
                                    eyebrowse
                                    ;; emmet-mode
                                    stickyfunc-enhance
                                    flx-ido
                                    smooth-scrolling
                                    org-repo-todo
                                    chinese-wbim
                                    chinese-pyim
                                    srefactor
                                    org-download
                                    org-timer
                                    org-plus-contrib
                                    org-tree-slide
                                    git-gutter
                                    git-gutter-fringe
                                    ;; i prefer iedit
                                    ;; multiple-cursors
                                    ;; disable it for lispy-mode
                                    ;;https://github.com/abo-abo/lispy/issues/137
                                    ;; evil-escape
                                    ;;At first, I should disable hydra in zilongshanren layer and install clj-refactor, after it is installed.
                                    ;; I could re-enable it again in zilongshanren layer.
                                    ;; clj-refactor
                                    ;;remove from spacemacs distribution
                                    ;; neotree
                                    leuven-theme
                                    gh-md
                                    evil-lisp-state
                                    spray
                                    doc-view
                                    lorem-ipsum
                                    solarized-theme
                                    beacon
                                    ;; spaceline
                                    )
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https nil
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects bookmarks)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 10
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(brin
                         solarized-dark
                         solarized-light
                         leuven
                         ;; sanityinc-tomorrow-day
                         ;; sanityinc-tomorrow-eighties
                         ;; spacemacs-dark
                         ;; spacemacs-light
                         ;; solarized-dark
                         ;; zenburn
                         )
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key ":"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
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
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
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
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters nil
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put almost
any user code here.  The exception is org related code, which should be placed
in `dotspacemacs/user-config'."
  ;; https://github.com/syl20bnr/spacemacs/issues/2705
  ;; (setq tramp-mode nil)
  (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  ;; ss proxy. But it will cause anacond-mode failed.
  (setq socks-server '("Default server" "127.0.0.1" 1080 5))
  (setq evil-shift-round nil)
  )

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."

  ;; mydearxym
  ;; (require 'sublimity)
  ;; (sublimity-mode 1)
  ;; (setq mouse-wheel-scroll-amount '(10 ((shift) . 10) ((control) . nil)))
  ;; (setq mouse-wheel-progressive-speed 1)
  ;; (setq scroll-step 10)
  ;; (setq scroll-conservatively 1000)
  ;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  ;; (setq mouse-wheel-progressive-speed nil)
  ;; (setq mouse-wheel-follow-mouse t)
  ;; (global-highlight-parentheses-mode nil) ;; not work

  ;; (set-background-color "#385063")
  (set-background-color "#334452")
  (linum-relative-global-mode -1)

  (setq default-tab-width 2)
  (modify-syntax-entry ?_ "w")
  (add-hook 'react-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'react-mode-hook
            (lambda()
              (add-hook 'write-contents-functions
                        (lambda()
                          (save-excursion
                            (delete-trailing-whitespace)))
                        nil t)))

  (global-set-key (kbd "C-c C-p") 'helm-projectile-find-file-dwim)
  ;; (global-set-key (kbd "C-c C-p") 'zilongshanren/open-file-with-projectile-or-counsel-git)

  ;; (define-globalized-minor-mode global-highlight-parentheses-mode
  ;;   highlight-parentheses-mode
  ;;   (lambda ()
  ;;     (highlight-parentheses-mode -1)))
  ;; (global-highlight-parentheses-mode -1)

  (global-set-key (kbd "C-;") 'yas-expand)
  ;; (setq-default mode-require-final-newline t)

  ;; vim surround staff
  (global-evil-surround-mode 1)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-substitute)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)

  ;; multi cursors
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

  (setq-default evil-escape-key-sequence "fd")
  ;; disable auto-recenter
  (setq scroll-step 1)
  (setq scroll-conservatively 10000)
  (setq auto-window-vscroll nil)

  (global-set-key (kbd "C-h") 'delete-backward-char)
  (global-set-key (kbd "C-e") 'end-of-line)
  ;; (global-set-key (kbd "C-l") 'recenter)
  (global-set-key (kbd "C-j") 'newline-and-indent)
  (global-set-key (kbd "C-s") 'evil-search-word-forward)
  ;; (global-set-key (kbd ",.") 'er/expand-region)
  (define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

  (global-set-key (kbd "C-c C-j") 'ace-jump-mode)

  ;; (global-set-key (kbd "C-,") 'spacemacs/previous-useful-buffer)
  ;; (global-set-key (kbd "C-.") 'spacemacs/next-useful-buffer)

  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-h") 'delete-backward-char)
    (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char))

  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-h") 'delete-backward-char))

  (define-key input-decode-map (kbd "C-i") (kbd "H-i"))
  (with-eval-after-load 'evil
    ;; (define-key evil-normal-state-map (kbd "C-o") 'evil-jump-backward)

    (define-key evil-normal-state-map (kbd "C-<up>") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-<down>") 'evil-numbers/dec-at-pt)

    (define-key evil-normal-state-map (kbd "p") 'yank)
    (define-key evil-normal-state-map (kbd "H-i") 'er/expand-region)
    ;; (global-set-key (kbd "C-c C-s") 'helm-swoop)
    (define-key evil-normal-state-map (kbd "C-d") 'evil-delete-char)
    (define-key evil-normal-state-map (kbd "H-i") 'er/expand-region)
    (define-key evil-insert-state-map (kbd "H-i") 'er/expand-region)
    (define-key evil-insert-state-map (kbd "C-w") 'evil-delete-backward-word)
    (define-key evil-normal-state-map (kbd "C-w") 'evil-delete-backward-word)

    (define-key evil-normal-state-map (kbd "gc") 'evilnc-comment-or-uncomment-lines)
    (define-key evil-insert-state-map (kbd "C-g") 'evil-escape)

    (define-key evil-normal-state-map (kbd "C-l") 'recenter)
    (define-key evil-insert-state-map (kbd "C-l") 'hippie-expand)

    (define-key evil-normal-state-map (kbd "C-.") 'helm-projectile-switch-to-buffer)
    ;; (define-key evil-normal-state-map (kbd "C-,") 'er/expand-region)
    (define-key evil-normal-state-map (kbd ",l") 'evil-search-highlight-persist-remove-all)
    (define-key evil-visual-state-map (kbd ",t") 'spacemacs/align-repeat-equal)
    (define-key evil-visual-state-map (kbd ",T") 'spacemacs/align-repeat)
    (define-key evil-normal-state-map (kbd ",f") 'neotree-find-project-root)
    (define-key evil-normal-state-map (kbd "C-s-s") 'helm-swoop)

    (define-key evil-normal-state-map (kbd "C-f") 'evil-forward-char)
    (define-key evil-visual-state-map (kbd "C-f") 'evil-forward-char)
    (define-key evil-insert-state-map (kbd "C-f") 'evil-forward-char)

    (define-key evil-normal-state-map (kbd "C-b") 'evil-backward-char)
    (define-key evil-visual-state-map (kbd "C-b") 'evil-backward-char)
    (define-key evil-insert-state-map (kbd "C-b") 'evil-backward-char)

    (define-key evil-normal-state-map (kbd "C-y") 'scroll-up-line)
    (define-key evil-insert-state-map (kbd "C-y") 'scroll-up-line)

    ;; (define-key evil-normal-state-map (kbd "C-p") 'evil-scroll-up)
    ;; (define-key evil-normal-state-map (kbd "C-n") 'evil-scroll-down)

    (define-key evil-insert-state-map (kbd "C-p") 'evil-previous-visual-line)
    (define-key evil-insert-state-map (kbd "C-n") 'evil-next-visual-line)

    ;; (define-key evil-insert-state-map (kbd "C-p") 'evil-scroll-up)
    ;; (define-key evil-insert-state-map (kbd "C-n") 'evil-scroll-down)
    (define-key evil-visual-state-map (kbd "C-p") 'evil-scroll-up)
    (define-key evil-visual-state-map (kbd "C-n") 'evil-scroll-down)

    (define-key evil-visual-state-map (kbd "C-e") 'evil-end-of-visual-line)
    (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line))


  (global-hl-line-mode t)
  (global-linum-mode t)
  (column-number-mode t)

  ;; (add-hook 'js2-mode-hook 'web-mode)
  (setq-default js2-basic-offset 2)
  (setq javascript-indent-level 2)
  (setq-default indent-tabs-mode nil)

  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  ;; (setq react-mode-markup-indent-offset 2)
  ;; (setq react-mode-css-indent-offset 2)
  ;; (setq react-mode-code-indent-offset 2)

  ;; (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (if (equal web-mode-content-type "javascript")
                  (web-mode-set-content-type "jsx")
                (message "now set to: %s" web-mode-content-type))))
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")))

  ;; (push '("\\.js\\'" . web-mode) auto-mode-alist)
  (push '("\\.js\\'" . react-mode) auto-mode-alist)

  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'react-mode)
  (add-hook 'react-mode-hook 'smartparens-mode)

  (setq js2-bounce-indent-p t)

  ;; (with-eval-after-load 'react-mode
  ;;   (progn
  ;;     (setq-default js2-bounce-indent nil)
  ;;     (setq-default js-indent-level 2)
  ;;     (setq-default js2-indent-level 2)
  ;;     (setq-default js2-basic-offset 2)

  ;;     ))


  ;; (setq imenu-generic-expression '((nil "^\\([A-Z_]+\\)=.*" 1)))
  ;; (add-hook 'web-mode-hook (lambda ()
  ;;          (setq imenu-generic-expression '((nil "^\\([A-Z_]+\\)=.*" 1)))))

  (global-company-mode -1)

  (global-set-key (kbd ",")
                  #'(lambda ()
                      (interactive)
                      (insert ", ")))

  ;; (global-set-key (kbd "=")
  ;;                 #'(lambda ()
  ;;                     (interactive)
  ;;                     (insert " = ")))

  ;; (global-set-key (kbd ":")
  ;;                 #'(lambda ()
  ;;                     (interactive)
  ;;                     (insert " : ")))
  ;; disable backup
  (setq backup-inhibited t)
                                        ;disable auto save
  (setq auto-save-default nil)
  (setq make-backup-files nil)
  ;; dired custom
  (put 'dired-find-alternate-file 'disabled nil)
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

  ;; hexo 博客配置
  ;; (add-to-list 'load-path "~/.spacemacs.d/layers/guanghui/")
  ;; (require 'hexo)
  ;; (defun blog ()
  ;;   (interactive)
  ;;    (hexo "~/blog/"))

  (setq hexo-dir "~/blog")
  (defun blog-deploy ()
    "git add . & git commit & git push & hexo d -g"
    (interactive)
    (async-shell-command (format "cd %s ;git add . ;git commit -am \"update\" ; npm run deploy"
                                 hexo-dir)))
  (defun blog-list ()
    "use dired open hexo source dir"
    (interactive)
    (ido-find-file-in-dir (format "%s/source/_posts" hexo-dir))
    )

  (defun blog-new (post-name)
    "create a hexo org post"
    (interactive "sInput post name:")
    (find-file (format "%s/source/_posts/%s.org" hexo-dir post-name))
    (insert (format "#+TITLE: %s
#+DATE: <%s>
#+TAGS: 默认标签
#+CATEGORIES: 默认分类


写点什么吧
"  post-name (format-time-string "%Y-%m-%d %H:%M:%S"))))

  ;; 博客截图
  ;; {% fi /images/2016.04.25.23.53.jietu.png %}

  (defun insert-org-or-md-img-link (prefix imagename)
    (if (equal (file-name-extension (buffer-file-name)) "md")
        (insert (format "[[%s][%s%s]]" imagename prefix imagename)))
    (insert (format "{%s fi %s%s}" "%" prefix (concat (format-time-string "%Y.%m.%d.%H.%M") "." imagename " %"))))


  (defun capture-screenshot (basename)
    "Take a screenshot into a time stamped unique-named file in the
  same directory as the org-buffer/markdown-buffer and insert a link to this file."
    (interactive "sScreenshot name: ")
    (if (equal basename "")
        (setq basename (format-time-string "%Y%m%d_%H%M%S")))
    (setq fullpath
          (concat (file-name-directory (buffer-file-name))
                  (file-name-base (buffer-file-name))
                  "_"
                  basename))
    (progn
      (call-process "screencapture" nil nil nil "-s"
                    (concat  "/Users/xieyiming/blog/source/images/"  (format-time-string "%Y.%m.%d.%H.%M") "."  basename  ".png"))
      (insert-org-or-md-img-link "/images/" (concat basename ".png")))
    (insert "\n"))

  ;; (setq org-bullets-bullet-list '("■" "◆" "▲" "▶"))

  ;; turn on flychecking globally
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'react-mode)

  ;; flycheck check on save
  (setq flycheck-check-syntax-automatically '(mode-enabled save))

  (set-language-environment "UTF-8")

  ;; Company-mode 中使用 C-n 与 C-p 来选择补全项，
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous))

  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)

  ;; css-mode
  ;; (add-hook 'scss-mode-hook
  ;;           '(lambda()
  ;;              (setq c-basic-offset 2)
  ;;              (setq tab-with 2)
  ;;              (setq indent-tabs-mode nil)))

  ;; (add-hook 'sass-mode-hook
  ;;           '(lambda()
  ;;              (setq c-basic-offset 2)
  ;;              (setq tab-with 2)
  ;;              (setq indent-tabs-mode nil)))

  ;; 添加国内的 elpa 源
  ;; (setq configuration-layer--elpa-archives
  ;;       '(("popkit" . "elpa.popkit.org/packages/")
  ;;         ("org"   . "orgmode.org/elpa/")
  ;;         ("gnu"   . "elpa.gnu.org/packages/")))

  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-substitute)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)
  ;; mydearxym end

  ;;解决org表格里面中英文对齐的问题
  (when (configuration-layer/layer-usedp 'chinese)
    (when (and (spacemacs/system-is-mac) window-system)
      (spacemacs//set-monospaced-font "Source Code Pro" "Hiragino Sans GB" 14 16)))

  ;; (global-company-mode t)
  (setq-default powerline-default-separator 'arrow)

  ;; Utility functions
  (defun bb/define-key (keymap &rest bindings)
    (declare (indent 1))
    (while bindings
      (define-key keymap (pop bindings) (pop bindings))))
  (bb/define-key evil-normal-state-map
    "+" 'spacemacs/evil-numbers-increase
    "_" 'spacemacs/evil-numbers-decrease
    "\\" 'evil-repeat-find-char-reverse
    "[s" (lambda (n) (interactive "p") (dotimes (c n nil) (insert " ")))
    "]s" (lambda (n) (interactive "p")
           (forward-char) (dotimes (c n nil) (insert " ")) (backward-char (1+ n))))

  (bb/define-key company-active-map
    (kbd "C-w") 'evil-delete-backward-word)

  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-w") 'evil-delete-backward-word))

  (with-eval-after-load 'helm-swoop
    (define-key helm-swoop-map (kbd "C-w") 'evil-delete-backward-word))

  (add-hook 'text-mode-hook 'auto-fill-mode)
  (add-hook 'org-mode-hook 'auto-fill-mode)

  (remove-hook 'emacs-lisp-mode-hook 'auto-compile-mode)
  (with-eval-after-load 'helm-files
    (define-key helm-find-files-map (kbd "s-c") 'helm-ff-run-copy-file))

  ;; http://emacsredux.com/blog/2014/04/05/which-function-mode/
  ;; when editing js file, this feature is very useful
  ;; (setq-default header-line-format
  ;;               '((which-func-mode ("" which-func-format " "))))
  (setq mode-line-misc-info
        ;; We remove Which Function Mode from the mode line, because it's mostly
        ;; invisible here anyway.
        (assq-delete-all 'which-func-mode mode-line-misc-info))

  (add-hook 'prog-mode-hook
            (lambda ()
              (when (> (buffer-size) 100000)
                (turn-off-show-smartparens-mode))))

  ;; improve the performance of opening large file
  (add-hook 'org-mode-hook (lambda () (spacemacs/toggle-line-numbers-off)) 'append)
  (defun spacemacs/check-large-file ()
    (when (> (buffer-size) 100000)
      (progn (fundamental-mode)
             (hl-line-mode -1))))

  (add-hook 'find-file-hook 'spacemacs/check-large-file)
  (spacemacs/toggle-automatic-symbol-highlight-on)

  ;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
  ;; (defun my-minibuffer-setup-hook ()
  ;;   (setq gc-cons-threshold 100000000))

  ;; (defun my-minibuffer-exit-hook ()
  ;;   (setq gc-cons-threshold 800000))

  ;; (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
  ;; (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

  ;; For python
  (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  ;; For ruby
  (add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  ;; For Javascript
  (add-hook 'js2-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  (spacemacs/set-leader-keys "rh" 'helm-resume)
  (spacemacs/set-leader-keys "ri" 'ivy-resume)
  (spacemacs|add-company-hook 'text-mode)

  (defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
    "Create parent directory if not exists while visiting file."
    (unless (file-exists-p filename)
      (let ((dir (file-name-directory filename)))
        (unless (file-exists-p dir)
          (make-directory dir t)))))
  ;; temp fix for spacemacs/jump-in-buffer
  (spacemacs/set-leader-keys "sj" 'helm-semantic-or-imenu)
  (spacemacs/set-leader-keys "sS" 'spacemacs/helm-swoop-region-or-symbol)

  (add-hook 'minibuffer-inactive-mode-hook '(lambda() (set (make-local-variable 'semantic-mode) nil)))
  ;; http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
  (defun zilongshanren/stop-using-minibuffer ()
    "kill the minibuffer"
    (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
      (abort-recursive-edit)))

  (add-hook 'mouse-leave-buffer-hook 'zilongshanren/stop-using-minibuffer)

  (spacemacs/declare-prefix "ot" "Toggle")

  (spacemacs|define-transient-state gist-list-mode
    :title "Gist-mode Transient State"
    :doc "
[_k_]  kill current gist
[_e_]  edit gist title
[_+_]  add a file to current gist
[_-_]  delete a file from the current gist
[_y_]  print current gist url
[_b_]  browse current gist in browser
[_*_]  star current gist
[_\\^_]  unstar current gist
[_f_]  fork current gist
"
    :bindings
    ("k" gist-kill-current "delete current gist")
    ("e" gist-edit-current-description "edit current gist title")
    ("+" gist-add-buffer "add a file to current gist ")
    ("-" gist-remove-file "add a file to current gist ")
    ("y" gist-print-current-url "print current gist url")
    ("b" gist-browse-current-url "browse current gist in browser")
    ("*" gist-star "star current gist")
    ("^" gist-unstar "unstar current gist")
    ("f" gist-fork "fork current gist")
    ("q" nil "quit" :exit t)
    ("<escape>" nil nil :exit t))
  (spacemacs/set-leader-keys-for-major-mode 'gist-list-mode
    "." 'spacemacs/gist-list-mode-transient-state/body)
  )

(setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
(load custom-file 'no-error 'no-message)
