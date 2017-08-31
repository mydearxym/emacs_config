;;; keybindings.el --- mydearxym Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 mydearxym
;;
;; Author: mydearxym <mydearxym@qq.com>
;; URL: https://github.com/mydearxym/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-i") 'info-display-manual)

(global-set-key [(shift return)] 'mydearxym/smart-open-line)
(global-set-key (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c y") 'youdao-dictionary-search-at-point+)

(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c i e") 'spacemacs/auto-yasnippet-expand)
;; http://emacs.stackexchange.com/questions/220/how-to-bind-c-i-as-different-from-tab
;; (define-key input-decode-map [?\C-i] [C-i])
;; (define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward)
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)
(global-set-key [remap fill-paragraph] #'endless/fill-or-unfill)

;; (global-set-key (kbd "C-.") 'company-capf)

(global-set-key (kbd "C-`") 'toggle-input-method)
(global-set-key (kbd "<f5>") 'mydearxym/run-current-file)

;; "http://endlessparentheses.com/transposing-keybinds-in-emacs.html?source=rss"
;; (global-set-key "\C-t" #'transpose-lines)
;; (define-key ctl-x-map "\C-t" #'transpose-chars)

(when (spacemacs/system-is-mac)
 (spacemacs/set-leader-keys "o!" 'mydearxym/iterm-shell-command))

(spacemacs|add-toggle toggle-shadowsocks-proxy-mode
  :status shadowsocks-proxy-mode
  :on (global-shadowsocks-proxy-mode)
  :off (global-shadowsocks-proxy-mode -1)
  :documentation "Toggle shadowsocks proxy mode."
  :evil-leader "ots")

(global-set-key (kbd "s-s") 'save-buffer)
;; (bind-key* "s-k" 'scroll-other-window-down)
;; (bind-key* "s-j"  'scroll-other-window)
(bind-key* "C-c /" 'company-files)
;; (bind-key* "s-r" 'mydearxym/browser-refresh--chrome-applescript)
(bind-key* "s-;" 'mydearxym/insert-semicolon-at-the-end-of-this-line)
(bind-key* "C-s-;" 'mydearxym/delete-semicolon-at-the-end-of-this-line)
(bind-key* "s-," 'mydearxym/insert-comma-at-the-end-of-this-line)
;; (bind-key* "C-s-," 'mydearxym/delete-comma-at-the-end-of-this-line)
(bind-key* "C-c l" 'mydearxym/insert-chrome-current-tab-url)
(bind-key* "M-s o" 'occur-dwim)
(bind-key* "C-=" 'er/expand-region)
(bind-key* "M--" 'mydearxym/goto-match-paren)
(bind-key* "C-c k" 'which-key-show-top-level)
(bind-key* "C-M-s-y" 'aya-expand)
;; (bind-key* "C-l" 'recenter)


;; Utility functions
(defun bb/define-key (keymap &rest bindings)
  (declare (indent 1))
  (while bindings
    (define-key keymap (pop bindings) (pop bindings))))

(define-key evil-normal-state-map (kbd "-") nil)

(bb/define-key evil-normal-state-map
  "+" 'evil-numbers/inc-at-pt
  "-" 'evil-numbers/dec-at-pt
  "\\" 'evil-repeat-find-char-reverse
  "[s" (lambda (n) (interactive "p") (dotimes (c n nil) (insert " ")))
  "]s" (lambda (n) (interactive "p")
         (forward-char) (dotimes (c n nil) (insert " ")) (backward-char (1+ n))))

(with-eval-after-load 'company
  (progn
    (bb/define-key company-active-map
      (kbd "C-w") 'mydearxym/backward-kill-word)

    (bb/define-key company-active-map
      (kbd "s-w") 'company-show-location)))

(spacemacs/declare-prefix "ot" "Toggle")


(if (configuration-layer/layer-usedp 'helm)
    (progn (global-set-key (kbd "<f1>") 'mydearxym/helm-hotspots)
           (spacemacs/set-leader-keys "oo" 'mydearxym/helm-hotspots)))

(spacemacs/set-leader-keys "oc" 'my-auto-update-tags-when-save)
(spacemacs/set-leader-keys "fR" 'mydearxym/rename-file-and-buffer)

;;Must set key to nil to prevent error: Key sequence b m s starts with non-prefix key b m
(spacemacs/set-leader-keys "bm" nil)
(spacemacs/set-leader-keys "bD" 'spacemacs/kill-other-buffers)
(spacemacs/declare-prefix "bm" "Bookmark")
(spacemacs/set-leader-keys "bms" 'bookmark-set)
(spacemacs/set-leader-keys "bmr" 'bookmark-rename)
(spacemacs/set-leader-keys "bmd" 'bookmark-delete)
(spacemacs/set-leader-keys "bmj" 'counsel-bookmark)

(spacemacs/set-leader-keys "od" 'occur-dwim)
(spacemacs/set-leader-keys "ox" 'org-open-at-point)
(spacemacs/set-leader-keys "oac" 'mydearxym/browser-refresh--chrome-applescript)

;; helm specific keybindings
(if (configuration-layer/layer-usedp 'helm)
    (progn
      (spacemacs/set-leader-keys "rh" 'helm-resume)
      (spacemacs/set-leader-keys "sj" 'counsel-imenu)))

;; ivy specific keybindings
(if (configuration-layer/layer-usedp 'ivy)
    (progn
      (spacemacs/set-leader-keys "ff" 'counsel-find-file)
      (spacemacs/set-leader-keys "fL" 'counsel-locate)
      (spacemacs/set-leader-keys "hi" 'counsel-info-lookup-symbol)))

(spacemacs/set-leader-keys "en" 'flycheck-next-error)
(spacemacs/set-leader-keys "ep" 'flycheck-previous-error)
(spacemacs/set-leader-keys "o(" 'ielm)

(spacemacs/set-leader-keys "gL" 'magit-log-buffer-file)
(spacemacs/set-leader-keys "og" 'my-git-timemachine)

(spacemacs/set-leader-keys "sj" 'helm-imenu)
;; deal with BOM
(spacemacs/set-leader-keys "fl" 'find-file-literally-at-point)
(spacemacs/set-leader-keys "ri" 'ivy-resume)
(spacemacs/set-leader-keys "fh" 'ffap-hexl-mode)
(spacemacs/set-leader-keys "nl" 'spacemacs/evil-search-clear-highlight)
(spacemacs/set-leader-keys "oll" 'mydearxym/load-my-layout)
(spacemacs/set-leader-keys "ols" 'mydearxym/save-my-layout)
(spacemacs/set-leader-keys "ob" 'popwin:display-last-buffer)

(bind-key* "s-p" 'find-file-in-project)
(spacemacs/set-leader-keys "os" 'mydearxym/search-in-fireball)

(spacemacs/set-leader-keys "pa" 'projectile-find-other-file)
(spacemacs/set-leader-keys "pA" 'projectile-find-other-file-other-window)


;; -------------------------------------------------
;; mydearxym
;; -------------------------------------------------

(setq-default evil-escape-key-sequence "fd")
(global-set-key (kbd "C-e") 'end-of-line)
(global-set-key (kbd "C-s") 'evil-search-word-forward)
(global-set-key (kbd "C-c C-p") 'helm-projectile-find-file)

;; org mode // todo add it to org-mode only
(global-set-key (kbd "s-e")
                #'(lambda ()
                    (interactive)
                    (insert "emacs-lisp")))

;; special key
(global-set-key (kbd "s-/")
                #'(lambda ()
                    (interactive)
                    (insert "\\")))

;; Company-mode 中使用 C-n 与 C-p 来选择补全项，
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

;; snippets

(global-set-key (kbd "C-;") 'yas-expand)


;; vim surround staff
(global-evil-surround-mode 1)
;; (evil-define-key 'visual evil-surround-mode-map "s" 'evil-substitute)
;; (evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)
(evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)

;; mark-history
(spacemacs/set-leader-keys "rm" 'helm-global-mark-ring)
(spacemacs/set-leader-keys "rM" 'helm-mark-ring)

;; multi cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; remap C-h
(global-set-key (kbd "C-h") 'delete-backward-char)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

(with-eval-after-load 'helm
  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-h") 'delete-backward-char))


;; remap evil

(with-eval-after-load 'helm
  (define-key helm-map (kbd "C-w") 'mydearxym/backward-kill-word))

(with-eval-after-load 'helm-swoop
  (define-key helm-swoop-map (kbd "C-w") 'mydearxym/backward-kill-word))

(define-key input-decode-map (kbd "C-i") (kbd "H-i"))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "SPC SPC") 'counsel-M-x)
  (define-key evil-normal-state-map (kbd "SPC ;") 'counsel-M-x)
  (define-key evil-visual-state-map (kbd ",a") 'evil-indent)

  (define-key evil-normal-state-map (kbd ",d") 'magit-diff-buffer-file)

  (define-key evil-normal-state-map (kbd ".") 'nil) ;; use it for lispy bound
  (define-key evil-normal-state-map (kbd "C-j") (lambda () (interactive) (evil-next-visual-line 4)))
  (define-key evil-normal-state-map (kbd "C-k") (lambda () (interactive) (evil-next-visual-line -4)))

  (define-key evil-insert-state-map (kbd "C-j") 'newline-and-indent)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)

  (define-key evil-normal-state-map (kbd "C-x C-b") 'ivy-switch-buffer)

  (define-key evil-normal-state-map (kbd "C-<up>") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-<down>") 'evil-numbers/dec-at-pt)

  (define-key evil-normal-state-map (kbd "C-t") 'mydearxym/move-text-up)
  (define-key evil-insert-state-map (kbd "C-t") 'transpose-words)
  ;; (define-key evil-normal-state-map (kbd "p") 'yank)
  ;; (define-key evil-normal-state-map "p" 'evil-paste-after)
  (define-key evil-normal-state-map (kbd "P") 'helm-show-kill-ring)
  (define-key evil-normal-state-map (kbd "H-i") 'er/expand-region)
  ;; (global-set-key (kbd "C-c C-s") 'helm-swoop)
  (define-key evil-normal-state-map (kbd "C-d") 'evil-delete-char)
  (define-key evil-normal-state-map (kbd "H-i") 'er/expand-region)
  (define-key evil-insert-state-map (kbd "H-i") 'er/expand-region)
  (define-key evil-insert-state-map (kbd "C-w") 'mydearxym/backward-kill-word)
  (define-key evil-normal-state-map (kbd "C-w") 'mydearxym/backward-kill-word)
  (define-key evil-normal-state-map (kbd "gc") 'spacemacs/comment-or-uncomment-lines-inverse)
  (define-key evil-normal-state-map (kbd "gC") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-escape)

  (define-key evil-normal-state-map (kbd "C-l") 'recenter)
  (define-key evil-insert-state-map (kbd "C-l") 'hippie-expand)

  (define-key evil-normal-state-map (kbd "C-.") 'helm-projectile-switch-to-buffer)
  ;; (define-key evil-normal-state-map (kbd "C-,") 'er/expand-region)
  (define-key evil-normal-state-map (kbd ",l") 'evil-search-highlight-persist-remove-all)

  (define-key evil-visual-state-map (kbd ",T") 'spacemacs/align-repeat-equal)
  (define-key evil-visual-state-map (kbd "C-s-t") 'spacemacs/align-repeat)
  ;; (define-key evil-normal-state-map (kbd ",f") 'ranger)
  (define-key evil-normal-state-map (kbd ",g") 'evil-avy-goto-char-2)
  ;; avy jump back 以后 auto highlight symbol 会失效，需要 reload buffer 才可以
  (define-key evil-normal-state-map (kbd ",.") 'xym/revert-buffer-no-confirm)

  (define-key evil-normal-state-map (kbd "C-s-s") 'spacemacs/helm-swoop-region-or-symbol)

  (define-key evil-normal-state-map (kbd "C-f") 'evil-forward-char)
  (define-key evil-visual-state-map (kbd "C-f") 'evil-forward-char)
  (define-key evil-insert-state-map (kbd "C-f") 'evil-forward-char)

  (define-key evil-normal-state-map (kbd "C-b") 'evil-backward-char)
  (define-key evil-visual-state-map (kbd "C-b") 'evil-backward-char)
  (define-key evil-insert-state-map (kbd "C-b") 'evil-backward-char)

  (define-key evil-normal-state-map (kbd "C-y") 'scroll-up-line)
  (define-key evil-insert-state-map (kbd "C-y") 'scroll-up-line)

  (define-key evil-normal-state-map (kbd "C-p") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-n") 'evil-scroll-down)

  (define-key evil-insert-state-map (kbd "C-p") 'evil-previous-visual-line)
  (define-key evil-insert-state-map (kbd "C-n") 'evil-next-visual-line)

  ;; (define-key evil-insert-state-map (kbd "C-p") 'evil-scroll-up)
  ;; (define-key evil-insert-state-map (kbd "C-n") 'evil-scroll-down)
  (define-key evil-visual-state-map (kbd "C-p") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-n") 'evil-scroll-down)

  (define-key evil-visual-state-map (kbd "C-e") 'evil-end-of-visual-line)
  (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line))

(spacemacs/set-leader-keys "pf" 'mydearxym/open-file-with-projectile-or-counsel-git)
