;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     ivy
     helm
     react
     better-defaults
     github
     osx
     markdown
     (vinegar :variables vinegar-reuse-dired-buffer t)
     org
     prodigy
     search-engine
     (syntax-checking :variables syntax-checking-enable-by-default nil
                      syntax-checking-enable-tooltips nil)
     (spell-checking :variables spell-checking-enable-by-default nil)


     erlang
     elixir
     python
     ;; go
     windows-scripts

     yaml
     html
     javascript

     themes-megapack
     command-log
     emacs-lisp

     (spacemacs-layouts :variables layouts-enable-autosave t
                        layouts-autosave-delay 60000)
     colors
     (git :variables
          git-magit-status-fullscreen t
          magit-push-always-verify nil
          magit-save-repository-buffers 'dontask
          magit-revert-buffers 'silent
          magit-refs-show-commit-count 'all
          magit-revision-show-gravatars nil)

     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     ;; (c-c++ :variables
     ;;        c-c++-default-mode-for-headers 'c++-mode)
     (auto-completion :variables auto-completion-enable-sort-by-usage t
                      :disabled-for org markdown)
     mydearxym)
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(counsel-projectile
                                    magit-gh-pulls
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

   dotspacemacs-install-packages 'used-only
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https nil
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(brin solarized-dark solarized-light leuven)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-emacs-command-key ":"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text t
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'origami
   dotspacemacs-smartparens-strict-mode t
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters nil
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'changed
   ))

(defun dotspacemacs/user-init ()

  (setq configuration-layer--elpa-archives
        '(("melpa-cn" . "https://elpa.zilongshanren.com/melpa/")
          ("org-cn"   . "https://elpa.zilongshanren.com/org/")
          ("gnu-cn"   . "https://elpa.zilongshanren.com/gnu/")))

  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  ;; ss proxy. But it will cause anacond-mode failed.
  (setq socks-server '("Default server" "127.0.0.1" 1080 5))
  (setq evil-shift-round nil)
  (setq byte-compile-warnings '(not obsolete))
  )

(defun dotspacemacs/user-config ()
  ;; mydearxym
  ;; (setq x-select-enable-clipboard nil)
  ;; (set-background-color "#385063")

  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

  (add-hook 'css-mode-hook
            '(lambda()
               (setq tab-width 2)))

  (setq css-indent-offset 2)

  (set-background-color "#334452")
  (setq-default line-height 1.1)
  (setq-default line-spacing 0.20)
  ;; (set-background-color "#002B37")
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

  ;; disable auto-recenter
  (setq scroll-step 1)
  (setq scroll-conservatively 10000)
  (setq auto-window-vscroll nil)

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

  (add-hook 'after-init-hook #'global-flycheck-mode)
  (eval-after-load 'flycheck
    '(add-to-list 'flycheck-checkers 'stylelint))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'react-mode)
  (add-hook 'react-mode-hook 'smartparens-mode)

  (setq js2-bounce-indent-p t)

  ;; (setq imenu-generic-expression '((nil "^\\([A-Z_]+\\)=.*" 1)))
  ;; (add-hook 'web-mode-hook (lambda ()
  ;;          (setq imenu-generic-expression '((nil "^\\([A-Z_]+\\)=.*" 1)))))

  (global-company-mode t)

  ;; (global-set-key (kbd ",")
  ;;                 #'(lambda ()
  ;;                     (interactive)
  ;;                     (insert ", ")))

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


注意排版哈注意排版哈注意排版哈注意排版哈注意排版哈注意排版哈注意排版哈注意排版哈注意排版|
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


  ;; (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)

  ;; 关掉spacemacs 在空字符串除 `Tab` 时出现的 `helm-complete`（无意义，且反应非常慢）
  (setq tab-always-indent t)

  (setq ranger-cleanup-eagerly t)
  (setq-default cursor-type 'bar)
  (setq helm-input-idle-delay 0.2)

  ;; dim the parentheses when edit lisp code
  (defface paren-face
    '((((class color) (background dark))
       (:foreground "dimgrey"))
      (((class color) (background light))
       (:foreground "grey80")))
    "Face used to dim parentheses.")

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (font-lock-add-keywords nil
                                      '(("(\\|)" . 'paren-face)))))

  (add-hook 'prog-mode-hook 'highlight-numbers-mode)

  ;; mydearxym end

  ;;解决org表格里面中英文对齐的问题
  (when (configuration-layer/layer-usedp 'chinese)
    (when (and (spacemacs/system-is-mac) window-system)
      (spacemacs//set-monospaced-font "Source Code Pro" "Hiragino Sans GB" 15 17)))

  ;; (global-company-mode t)
  (setq-default powerline-default-separator 'arrow)

  ;; Utility functions
  (bb/define-key company-active-map
    (kbd "C-w") 'evil-delete-backward-word)

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

  (when (configuration-layer/layer-usedp 'ivy)
    (setq projectile-switch-project-action
          'zilongshanren/open-file-with-projectile-or-counsel-git)))

(setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
(load custom-file 'no-error 'no-message)
