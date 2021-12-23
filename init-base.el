(setq gc-cons-threshold (* 256 1024 1024))
(message "init-gc-cons-threshold: %d" gc-cons-threshold)
(add-hook #'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 128 1024 1024))
            (message "normal-gc-cons-threshold: %d" gc-cons-threshold)))
(add-hook 'focus-out-hook #'garbage-collect)

(setq read-process-output-max (* 1024 1024))
(message "read-process-output-max: %d" read-process-output-max)

(setq visible-bell t
      make-backup-files nil
      column-number-mode t
      split-height-threshold nil ; not to split this way.
      register-preview-delay 0
      gdb-many-windows t
      large-file-warning-threshold 50000000)
(blink-cursor-mode 0)
(setq-default truncate-lines t)
(setq-default tab-width 4)
(put 'erase-buffer 'disabled nil)
(show-paren-mode t)
(delete-selection-mode t)
(require 'cl-lib) ; Common Lisp
(setq winner-dont-bind-my-keys t)
(winner-mode)
(customize-set-variable 'display-buffer-base-action
                        '((display-buffer-reuse-window display-buffer-same-window)
                          (reusable-frames . t)))
(customize-set-variable 'even-window-sizes nil) ; avoid resizing

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
      (find-file "~/.config/i3status/config")
      (find-file "~/.config/i3/config")))

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

(if (version< emacs-version "26")
    (progn
      (load-file "~/.emacs.d/linum.elc")
      (setq linum-format "%5d \u2502")
      (setq linum-delay t)
      (global-linum-mode)
      (defcustom linum-disabled-modes-list '(eshell-mode wl-summary-mode compilation-mode org-mode text-mode dired-mode doc-view-mode pdf-view-mode)
        "* List of modes disabled when global linum mode is on"
        :type '(repeat (sexp :tag "Major mode"))
        :tag " Major modes where linum is disabled: "
        :group 'linum)
      (defcustom linum-disable-starred-buffers 't
        "* Disable buffers that have stars in them like *Gnu Emacs*"
        :type 'boolean
        :group 'linum)
      (defun linum-on ()
        "* When linum is running globally, disable line number in modes defined in `linum-disabled-modes-list'. Changed by linum-off. Also turns off numbering in starred modes like *scratch*"
        (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)
                    (and linum-disable-starred-buffers (string-match "*" (buffer-name))))
          (linum-mode 1))))
  (progn
    (add-hook 'prog-mode-hook #'display-line-numbers-mode)))

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
;; Global Keys
;;==============================================================================

(global-set-key (kbd "C-x o") 'other-window)
(global-set-key (kbd "C-x O") 'other-window-reverse)
(global-set-key (kbd "C-c x") 'hexl-mode-toggle)
(if (version< emacs-version "26")
    (global-set-key (kbd "C-c l") 'linum-mode)
  (global-set-key (kbd "C-c l") 'display-line-numbers-mode))
(global-set-key (kbd "C-{") #'winner-undo)
(global-set-key (kbd "C-}") #'winner-redo)


(provide 'init-base)
