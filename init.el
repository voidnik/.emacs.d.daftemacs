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
   '(org-re-reveal markdown-preview-mode graphviz-dot-mode ivy counsel counsel-projectile swiper ivy-posframe ivy-rich all-the-icons-ivy lsp-ivy diff-hl company-statistics treemacs-icons-dired qml-mode highlight-indent-guides lsp-treemacs magit-gitflow keyfreq neato-graph-bar importmagic pip-requirements py-autopep8 elpy json-reformat yasnippet elogcat rg deadgrep ripgrep helm-rg ag helm-ag dumb-jump focus smart-mode-line google-c-style ccls company-lsp lsp-ui lsp-mode flycheck treemacs-magit treemacs-projectile treemacs-evil treemacs pdf-tools helm-gtags imenu-list objc-font-lock neotree company magit vlf base16-theme flx-isearch flx-ido flx projectile dark-souls haskell-mode ztree with-editor wgrep use-package undo-tree transient tablist spinner shrink-path s rich-minority pyvenv popup pkg-info pfuture memoize markdown-mode magit-popup lv let-alist hydra ht highlight-indentation helm-core helm goto-chg git-commit find-file-in-project f evil epl epc doom-modeline deferred dash-functional dash ctable concurrent bind-key avy async all-the-icons ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:foreground "#8be9fd" :background "#8be9fd"))))
 '(diff-hl-delete ((t (:foreground "#ff5555" :background "#ff5555"))))
 '(diff-hl-insert ((t (:foreground "#50fa7b" :background "#50fa7b"))))
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

(setq default-input-method "korean-hangul")
(setq desktop-save-mode t)

(switch-to-buffer "*Messages*")
(setq default-directory my-default-directory) ; this line must be excuted after excuting '(switch-to-buffer "*Messages*")'.

(setenv "MANWIDTH" "72")

;;==============================================================================
;; projectile
;;==============================================================================

(projectile-mode +1)
(setq projectile-enable-caching t)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)

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
;; ivy, counsel, swiper
;;==============================================================================

(use-package ivy
  :ensure t
  :diminish
  :hook (after-init . ivy-mode)
  :config
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "<escape>") #'minibuffer-keyboard-quit)
  (setq ivy-re-builders-alist
        '((counsel-rg . ivy--regex-plus)
          (counsel-projectile-rg . ivy--regex-plus)
          (counsel-ag . ivy--regex-plus)
          (counsel-projectile-ag . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t
        ivy-count-format "[%d/%d] "
        ivy-initial-inputs-alist nil))

(use-package counsel
  :ensure t
  :diminish
  :hook (ivy-mode . counsel-mode)
  :config
  (setq counsel-rg-base-command "rg --vimgrep %s"))

(use-package counsel-projectile
  :ensure t
  ;;:config (counsel-projectile-mode +1) ;; This is commented to prevent that the original shortcuts for projectile are overrided.
  )

(use-package swiper
  :ensure t
  :after ivy
  :config
  (setq swiper-action-recenter t)
  (setq swiper-goto-start-of-match t))

(use-package ivy-posframe
  :ensure t
  :after ivy
  :diminish
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
        ivy-posframe-height-alist '((t . 30))
        ivy-posframe-parameters '((internal-border-width . 10)))
  (setq ivy-posframe-width 150)
  ;(ivy-posframe-mode +1)
  )

(use-package ivy-rich
  :ensure t
  :preface
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
   	    (get-buffer candidate)
	  (let ((icon (all-the-icons-icon-for-mode major-mode)))
	    (if (symbolp icon)
	        (all-the-icons-icon-for-mode 'fundamental-mode)
	      icon))))
  :init
  (setq ivy-rich-display-transformers-list ; max column width sum = (ivy-posframe-width - 1)
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-switch-buffer-icon (:width 2))
            (ivy-rich-candidate (:width 35))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 16 :face warning))
            (ivy-rich-switch-buffer-project (:width 16 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-projectile-switch-to-buffer
          (:columns
           ((ivy-rich-switch-buffer-icon (:width 2))
            (ivy-rich-candidate (:width 35))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 16 :face warning))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 35))
            (ivy-rich-counsel-function-docstring (:width 114 :face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 35))
            (ivy-rich-counsel-function-docstring (:width 114 :face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 35))
            (ivy-rich-counsel-variable-docstring (:width 114 :face font-lock-doc-face))))
          counsel-recentf
          (:columns
           ((ivy-rich-candidate (:width 130))
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
          counsel-bookmark
          (:columns
           ((ivy-rich-bookmark-type (:width 8))
            (ivy-rich-candidate (:width 50))
            (ivy-rich-bookmark-info)))
          package-install
          (:columns
           ((ivy-rich-candidate (:width 25))
            (ivy-rich-package-version (:width 12 :face font-lock-comment-face))
            (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
            (ivy-rich-package-install-summary (:width 55 :face font-lock-doc-face))))))
  (setq ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode +1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  ;; In order to make the changes of ivy-rich-display-transformers-list take effect,
  ;; ivy-rich-reload is needed after emacs-startup.
  (add-hook 'emacs-startup-hook (lambda () (ivy-rich-reload))))

;;==============================================================================
;; find-file-in-project
;;
;; https://github.com/technomancy/find-file-in-project
;;==============================================================================

(use-package find-file-in-project
  :ensure t)

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
;; init-misc
;;==============================================================================

(load-file "~/.emacs.d/init-misc.el")
(require 'init-misc)

;;==============================================================================
;; init-keys
;;==============================================================================

(load-file "~/.emacs.d/init-keys.el")
(require 'init-keys)
