(setq gc-cons-threshold (* 256 1024 1024))
(message "init-gc-cons-threshold: %d" gc-cons-threshold)
(add-hook #'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 128 1024 1024))
            (message "normal-gc-cons-threshold: %d" gc-cons-threshold)))
(add-hook 'focus-out-hook #'garbage-collect)

(setq read-process-output-max (* 1024 1024))
(message "read-process-output-max: %d" read-process-output-max)

;; Don’t compact font caches during GC.
;; If you experience a slow down in performace when rendering multiple
;; icons simultaneously, you can try setting the following variable.
(setq inhibit-compacting-font-caches t)

;; https://www.masteringemacs.org/article/improving-performance-emacs-display-engine
(setq redisplay-dont-pause t)

;;(setq-default shell-file-name "/bin/sh")
(message "shell-file-name: %s" shell-file-name)
(cond
 ((string-equal system-type "darwin")
  (setq explicit-shell-file-name "/bin/zsh"))
 ((string-equal system-type "gnu/linux")
  (setq explicit-shell-file-name "/usr/bin/zsh")))
(message "explicit-shell-file-name: %s" explicit-shell-file-name)

(setq visible-bell t
      make-backup-files nil
      column-number-mode t
      split-width-threshold (if (< (display-pixel-width) 1920) 127 170)
      split-height-threshold nil ; not to split this way.
      register-preview-delay 0
      gdb-many-windows t
      large-file-warning-threshold (* 100 1024 1024))
(blink-cursor-mode 0)
(setq-default truncate-lines t)
(setq-default tab-width 4)
(put 'erase-buffer 'disabled nil)
(setq show-paren-style 'expression
      show-paren-priority 0)
(show-paren-mode t)
(delete-selection-mode t)
;;(display-time-mode t)
;;(display-battery-mode t)
(require 'cl-lib) ; Common Lisp
(setq winner-dont-bind-my-keys t)
(winner-mode)
(customize-set-variable 'display-buffer-base-action
                        '((display-buffer-reuse-window display-buffer-same-window)
                          (reusable-frames . t)))
(customize-set-variable 'even-window-sizes nil) ; avoid resizing

;; Make script files executable automatically
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(defun other-window-reverse ()
  (interactive)
  (other-window -1))

(add-hook 'shell-mode-hook
          (lambda ()
            (local-set-key "\C-ck" 'erase-buffer)))

(defun hexl-mode-toggle ()
  (interactive)
  (if (string= "hexl-mode" major-mode)
      (hexl-mode-exit)
    (hexl-mode)))

(defun open-user-init-file ()
  (interactive)
  (find-file-read-only user-init-file))

(defun open-zshrc-file ()
  (interactive)
  (find-file-read-only "~/.zshrc"))

(if (string-equal system-type "gnu/linux")
    (defun open-i3-config-file ()
      (interactive)
      (find-file-read-only "~/.config/i3status/config")
      (find-file-read-only "~/.config/i3/config")))

(cond
 ((string-equal system-type "gnu/linux")
  (defun open-qb-config-file ()
    (interactive)
    (find-file-read-only "~/.config/qutebrowser/config.py")
    (find-file-read-only "~/.config/qutebrowser/daftemacs-qb/daftemacs_qb_config.py")))
 ((string-equal system-type "darwin")
  (defun open-qb-config-file ()
    (interactive)
    (find-file-read-only "~/.qutebrowser/config.py")
    (find-file-read-only "~/.qutebrowser/daftemacs-qb/daftemacs_qb_config.py"))))

(message "split-width-threshold: %d" split-width-threshold)
(message "Display pixel size: %dx%d" (display-pixel-width) (display-pixel-height))

;;==============================================================================
;; Read-only directories
;;
;; https://www.reddit.com/r/emacs/comments/rkw3h1/readonly_mode_in_specific_subtree/
;;==============================================================================

(dir-locals-set-class-variables
 'read-only
 '((nil . ((buffer-read-only . t)))))

(dir-locals-set-directory-class "~/.config" 'read-only)

;;==============================================================================
;; hl-line-mode
;;==============================================================================

(global-hl-line-mode)
(add-hook 'term-mode-hook (lambda ()
                            (setq-local global-hl-line-mode nil)))

;;==============================================================================
;; Line Numbers
;;==============================================================================

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'yaml-mode-hook #'display-line-numbers-mode)

;;==============================================================================
;; Fill Column Indicator
;;==============================================================================

(setq-default display-fill-column-indicator-column 79)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;;==============================================================================
;; Hide Show
;;
;; https://www.emacswiki.org/emacs/HideShow
;;==============================================================================

(defun my-hs-minor-mode ()
  (hs-minor-mode)
  (local-set-key (kbd "C-c +") 'hs-show-all)
  (local-set-key (kbd "C-c _") 'hs-hide-level)
  (local-set-key (kbd "C-c =") 'hs-show-block)
  (local-set-key (kbd "C-c -") 'hs-hide-block))
(add-hook 'prog-mode-hook 'my-hs-minor-mode)

;;==============================================================================
;; Smooth Scrolling
;;
;; https://www.emacswiki.org/emacs/SmoothScrolling
;;==============================================================================

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
(setq mouse-wheel-progressive-speed t) ; accelerate scrolling
(setq mouse-wheel-follow-mouse t) ; scroll window under mouse
(setq scroll-step 1) ; keyboard scroll one line at a time
(setq scroll-conservatively 10000)

;;==============================================================================
;; Miscellaneous
;;==============================================================================

;; https://stackoverflow.com/questions/2416655/file-path-to-clipboard-in-emacs
(defun copy-file-name-on-clipboard ()
  "Copy the current buffer file name to the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

;;==============================================================================
;; Global Keys
;;==============================================================================

(global-set-key (kbd "C-c l") 'display-line-numbers-mode)
(global-set-key (kbd "M-g M-w") 'avy-goto-word-0)
(global-set-key (kbd "M-g M-l") 'avy-goto-line)
(global-set-key (kbd "C-x o") 'other-window)
(global-set-key (kbd "C-x O") 'other-window-reverse)
(global-set-key (kbd "C-{") 'winner-undo)
(global-set-key (kbd "C-}") 'winner-redo)
(global-set-key (kbd "C-c x") 'hexl-mode-toggle)

(provide 'init-base)
