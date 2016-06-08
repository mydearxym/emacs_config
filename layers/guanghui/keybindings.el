;;; keybindings.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zilongshanren 
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(bind-key* "C-c l" 'zilongshanren/insert-chrome-current-tab-url)

(global-set-key (kbd "s-/") 'hippie-expand)

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)


(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)

(define-key 'help-command (kbd "C-i") 'info-display-manual)

;; (global-set-key [(shift return)] 'zilongshanren/smart-open-line)
(global-set-key [(shift return)] 'spacemacs/alternate-buffer-in-persp)

;; (define-key global-map (kbd "<f1>") 'zilongshanren/hotspots)
(define-key global-map (kbd "C-c y") 'youdao-dictionary-search-at-point+)

;; (global-set-key (kbd "C-.") 'company-capf)


;; some easy functions for navigate functions
;;C-M-a beginning-of-defun
;;C-M-e end-of-defun
;;C-M-h mark-defun
(global-set-key (kbd "C-s-h") 'mark-defun)

(global-set-key (kbd "s-l") 'goto-line)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "C-`") 'toggle-input-method)

(global-set-key (kbd "<f5>") 'zilongshanren/run-current-file)

;; "http://endlessparentheses.com/transposing-keybinds-in-emacs.html?source=rss"
;; (global-set-key "\C-t" #'transpose-lines)
;; (define-key ctl-x-map "\C-t" #'transpose-chars)

(when (spacemacs/system-is-mac)
 (spacemacs/set-leader-keys "o!" 'zilongshanren/iterm-shell-command))

(spacemacs|add-toggle toggle-shadowsocks-proxy-mode
  :status shadowsocks-proxy-mode
  :on (global-shadowsocks-proxy-mode)
  :off (global-shadowsocks-proxy-mode -1)
  :documentation "Toggle shadowsocks proxy mode."
  :evil-leader "ots")

(global-set-key (kbd "s-s") 'save-buffer)
(bind-key* "s-k" 'scroll-other-window-down)
(bind-key* "s-j"  'scroll-other-window)
(bind-key* "C-c /" 'company-files)

(bind-key* "s-r" 'zilongshanren/browser-refresh--chrome-applescript)
(spacemacs/set-leader-keys "oac" 'zilongshanren/browser-refresh--chrome-applescript)

(bind-key* "s-;" 'zilongshanren/insert-semicolon-at-the-end-of-this-line)
(bind-key* "C-s-;" 'zilongshanren/delete-semicolon-at-the-end-of-this-line)

(bind-key* "s-," 'zilongshanren/insert-comma-at-the-end-of-this-line)
(bind-key* "C-s-," 'zilongshanren/delete-comma-at-the-end-of-this-line)

(bind-key* "C-=" 'er/expand-region)


(bind-key* "M--" 'zilongshanren/goto-match-paren)

(spacemacs/set-leader-keys "ol" 'zilongshanren/load-my-layout)

(global-set-key (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "<f9>") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)
;; http://emacs.stackexchange.com/questions/220/how-to-bind-c-i-as-different-from-tab
(define-key input-decode-map [?\C-i] [C-i])
(define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward)

(bind-key* "C-M-s-y" 'aya-expand)

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

;; -------------------------------------------------
;; mydearxym
;; -------------------------------------------------

(setq-default evil-escape-key-sequence "fd")
(global-set-key (kbd "C-e") 'end-of-line)
(global-set-key (kbd "C-s") 'evil-search-word-forward)
(global-set-key (kbd "C-c C-p") 'helm-projectile-find-file)

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

(define-key input-decode-map (kbd "C-i") (kbd "H-i"))
(with-eval-after-load 'evil
  ;; (define-key evil-normal-state-map (kbd "C-o") 'evil-jump-backward)
  (define-key evil-normal-state-map (kbd "C-j") (lambda () (interactive) (evil-next-visual-line 4)))
  (define-key evil-normal-state-map (kbd "C-k") (lambda () (interactive) (evil-next-visual-line -4)))

  (define-key evil-insert-state-map (kbd "C-j") 'newline-and-indent)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)

  (define-key evil-normal-state-map (kbd "C-x C-b") 'ivy-switch-buffer)

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
  (define-key evil-normal-state-map (kbd ",f") 'ranger)
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
