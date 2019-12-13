;; Editor: Richard Jaeho Hur

;;==============================================================================
;; Info Messages
;;==============================================================================

(defvar my-initial-buffer nil)
;(defvar my-initial-buffer (concat user-emacs-directory "/init.el"))
(defvar my-default-directory (concat (getenv "HOME") "/Workspace/"))

(message "      _        __ _")
(message "     | |      / _| |")
(message "   __| | __ _| |_| |_ ___ _ __ ___   __ _  ___ ___ ")
(message "  / _` |/ _` |  _| __/ _ \\ '_ ` _ \\ / _` |/ __/ __|")
(message " | (_| | (_| | | | ||  __/ | | | | | (_| | (__\\__ \\")
(message "  \\__,_|\\__,_|_|  \\__\\___|_| |_| |_|\\__,_|\\___|___/\n")
(message "emacs-version: %s %d %d" emacs-version emacs-major-version emacs-minor-version)
(message "system-type: %s" system-type)
(message "system-name: %s" system-name)
(message "user-login-name: %s" user-login-name)
(message "user-init-file: %s" user-init-file)
(message "user-emacs-directory: %s" user-emacs-directory)
(message "my-initial-buffer: %s" my-initial-buffer)
(message "my-default-directory: %s" my-default-directory)

;;==============================================================================
;; init-base
;;==============================================================================

(load-file "~/.emacs.d/init-base.el")
(require 'init-base)

;;==============================================================================
;; Packages
;;==============================================================================

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(message "package-archives: %s" package-archives)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;==============================================================================
;; Custom Set Variables
;;==============================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "bce3ae31774e626dce97ed6d7781b4c147c990e48a35baedf67e185ebc544a56" default))
 '(inhibit-startup-screen t)
 '(initial-buffer-choice my-initial-buffer)
 '(initial-frame-alist '((fullscreen . maximized)))
 '(org-file-apps
   '((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . emacs)))
 '(package-selected-packages
   '(lsp-ivy diff-hl company-statistics treemacs-icons-dired qml-mode highlight-indent-guides lsp-treemacs counsel magit-gitflow keyfreq neato-graph-bar importmagic pip-requirements py-autopep8 elpy json-reformat yasnippet elogcat rg deadgrep ripgrep helm-rg ag helm-ag dumb-jump focus smart-mode-line google-c-style ccls company-lsp lsp-ui lsp-mode flycheck treemacs-magit treemacs-projectile treemacs-evil treemacs pdf-tools helm-gtags imenu-list objc-font-lock neotree zerodark-theme company magit vlf base16-theme flx-isearch flx-ido flx projectile dark-souls haskell-mode ztree with-editor wgrep use-package undo-tree transient tablist swiper spinner shrink-path s rich-minority pyvenv popup pkg-info pfuture memoize markdown-mode magit-popup lv let-alist ivy hydra ht highlight-indentation helm-core helm goto-chg git-commit find-file-in-project f evil epl epc doom-modeline deferred dash-functional dash ctable concurrent bind-key avy async all-the-icons ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-current-diff-A ((t (:background "#7F3C63"))))
 '(ediff-current-diff-B ((t (:background "#287D3D"))))
 '(ediff-current-diff-C ((t (:background "#45747E"))))
 '(ediff-even-diff-A ((t (:background "#464752"))))
 '(ediff-even-diff-B ((t (:background "#464752"))))
 '(ediff-even-diff-C ((t (:background "#464752"))))
 '(ediff-fine-diff-A ((t (:foreground "#282a36" :background "#FF79C6"))))
 '(ediff-fine-diff-B ((t (:foreground "#282a36" :background "#50FA7B"))))
 '(ediff-fine-diff-C ((t (:foreground "#282a36" :background "#8BE9FD"))))
 '(ediff-odd-diff-A ((t (:background "#464752"))))
 '(ediff-odd-diff-B ((t (:background "#464752"))))
 '(ediff-odd-diff-C ((t (:background "#464752")))))

;;==============================================================================
;; init-appearance
;;==============================================================================

(load-file "~/.emacs.d/init-appearance.el")
(require 'init-appearance)

;;==============================================================================
;; startup
;;==============================================================================

(setq desktop-save-mode t)

(switch-to-buffer "*Messages*")
(setq default-directory my-default-directory) ; this line must be excuted after excuting '(switch-to-buffer "*Messages*")'.

(setenv "MANWIDTH" "72")

;;==============================================================================
;; undo-tree
;;
;; https://www.emacswiki.org/emacs/UndoTree
;;==============================================================================

(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)

;;==============================================================================
;; Hangul Input Method
;;==============================================================================

(setq default-input-method "korean-hangul")
(global-set-key (kbd "S-SPC") 'toggle-input-method)

;;==============================================================================
;; projectile
;;==============================================================================

(projectile-mode +1)
(setq projectile-enable-caching t)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(global-set-key (kbd "M-o") 'projectile-find-other-file)

;;==============================================================================
;; company
;;==============================================================================

(use-package company
  :ensure t
  :diminish company-mode
  :commands (company-complete company-mode)
  :bind (;([remap dabbrev-expand] . company-complete)
         :map prog-mode-map
         ([tab] . company-indent-or-complete-common))
  :init (if (fboundp 'evil-declare-change-repeat)
            (mapc #'evil-declare-change-repeat
                  '(company-complete-common
                    company-select-next
                    company-select-previous
                    company-complete-selection
                    company-complete-number)))
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-statistics
    :ensure t
    :init
    (company-statistics-mode))
  (setq company-idle-delay 0)
  (setq company-show-numbers "on"))

;;==============================================================================
;; yasnippet
;;==============================================================================

(require 'yasnippet)
(yas-global-mode 1)

;;==============================================================================
;; ivy
;;==============================================================================

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;;==============================================================================
;; flx-ido
;;==============================================================================

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;;;==============================================================================
;;; flycheck
;;;==============================================================================
;
;(add-hook 'after-init-hook #'global-flycheck-mode)

;;==============================================================================
;; buffer-move
;;==============================================================================

(load-file "~/.emacs.d/buffer-move.elc")

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;;==============================================================================
;; Killing Buffers
;;
;; https://www.emacswiki.org/emacs/KillingBuffers
;;==============================================================================

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;;==============================================================================
;; Asynchronous Shell Command Excution
;;
;; http://stackoverflow.com/questions/16815598/
;; run-commands-in-emacs-asynchronously-but-display-output-incrementally
;;==============================================================================

(defun execute-commands (buffer &rest commands)
  "Execute a list of shell commands sequentially"
  (with-current-buffer buffer
    (set (make-local-variable 'commands-list) commands)
    (start-next-command)))

(defun start-next-command ()
  "Run the first command in the list"
  (if (null commands-list)
      (insert "\nDone.")
    (let ((command  (car commands-list)))
      (setq commands-list (cdr commands-list))
      (insert (format ">> %s\n" command))
      (let ((process (start-process-shell-command command (current-buffer) command)))
        (set-process-sentinel process 'sentinel)))))

(defun sentinel (p e)
  "After a process exited, call `start-next-command' again"
  (let ((buffer (process-buffer p)))
    (when (not (null buffer))
      (with-current-buffer buffer
        ;(insert (format "Command `%s' %s" p e) )
        (start-next-command)))))

;;==============================================================================
;; Window Resize
;;
;; https://www.emacswiki.org/emacs/WindowResize
;;==============================================================================

(defun resize-window (&optional arg)
  "*Resize window interactively."
  (interactive "p")
  (if (one-window-p) (error "Cannot resize sole window"))
  (setq arg 4)
  ;(or arg (setq arg 1))
  (let (c)
    (catch 'done
      (while t
        (message
         "h=heighten, s=shrink, w=widen, n=narrow (by %d);  1-9=unit, q=quit"
         arg)
        (setq c (read-char))
        (condition-case ()
            (cond
             ((= c ?h) (enlarge-window arg))
             ((= c ?s) (shrink-window arg))
             ((= c ?w) (enlarge-window-horizontally arg))
             ((= c ?n) (shrink-window-horizontally arg))
             ((= c ?\^G) (keyboard-quit))
             ((= c ?q) (throw 'done t))
             ((and (> c ?0) (<= c ?9)) (setq arg (- c ?0)))
             (t (beep)))
          (error (beep)))))
    (message "Done.")))

(global-set-key (kbd "C-c r") 'resize-window)

;;==============================================================================
;; find-file-hook for handling the very large file
;;
;; https://stackoverflow.com/questions/18316665/
;; how-to-improve-emacs-performance-when-view-large-file
;;==============================================================================

(defun disable-slow-modes ()
  (interactive)
  (setq bidi-display-reordering nil)
  (jit-lock-mode nil)
  (set (make-variable-buffer-local 'font-lock-mode) nil)
  (if (version< emacs-version "26")
      (set (make-variable-buffer-local 'linum-mode) nil)
    (set (make-variable-buffer-local 'display-line-numbers) nil))
  (set (make-variable-buffer-local 'global-hl-line-mode) nil))

(defun my-find-file-check-if-very-large-file-hook ()
  "If a file is over 5MB, turn off modes of the buffer that make it slow."
  (when (> (buffer-size) (* 5 1024 1024))
    (disable-slow-modes)))
(add-hook 'find-file-hook 'my-find-file-check-if-very-large-file-hook)

;;==============================================================================
;; vlf
;;
;; https://github.com/m00natic/vlfi
;;==============================================================================

(require 'vlf-setup)

;;==============================================================================
;; init-tree-buffer
;;==============================================================================

(load-file "~/.emacs.d/init-tree-buffer.el")
(require 'init-tree-buffer)

;;==============================================================================
;; init-programming
;;==============================================================================

(load-file "~/.emacs.d/init-programming.el")
(require 'init-programming)

;;==============================================================================
;; init-tools
;;==============================================================================

(load-file "~/.emacs.d/init-tools.el")
(require 'init-tools)

;;==============================================================================
;; Dos To Unix
;;
;; https://www.emacswiki.org/emacs/DosToUnix
;;==============================================================================

(defun dos2unix (buffer)
  "Automate M-% C-q C-m RET C-q C-j RET"
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t))))

;;==============================================================================
;; keyfreq
;;==============================================================================

(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;;==============================================================================
;; Custom Keys
;;==============================================================================

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(global-set-key (kbd "C-c d") 'desktop-read)

(global-set-key (kbd "C-c w") 'xwidget-webkit-browse-url)

(global-set-key (kbd "C-c 1") 'eshell)
(global-set-key (kbd "C-c 2") 'shell)
(global-set-key (kbd "C-c 3") 'term)
(global-set-key (kbd "C-c 0") 'neato-graph-bar)

(global-set-key (kbd "C-c C-.") 'whitespace-mode)

(global-set-key (kbd "C-c s") 'swiper)
(global-set-key (kbd "C-c C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-c C-r") 'isearch-backward-regexp)

(global-set-key (kbd "M-m") 'imenu-list)

;; GUD
;(global-set-key [f5] '(lambda ()
;                        (interactive)
;                        (call-interactively 'gud-tbreak)
;                        (call-interactively 'gud-cont)))
;(global-set-key [f6] 'gud-next)
;(global-set-key [f7] 'gud-step)
;(global-set-key [f8] 'gud-finish)
;(global-set-key [f9] 'gud-break)
