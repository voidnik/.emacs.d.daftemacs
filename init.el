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
   '(org-bullets bufler org-re-reveal markdown-preview-mode graphviz-dot-mode ivy counsel counsel-projectile swiper ivy-posframe ivy-rich all-the-icons-ivy lsp-ivy diff-hl company-statistics treemacs-icons-dired qml-mode highlight-indent-guides lsp-treemacs keyfreq neato-graph-bar importmagic pip-requirements py-autopep8 elpy json-reformat yasnippet elogcat rg deadgrep ripgrep helm-rg ag helm-ag dumb-jump focus smart-mode-line google-c-style ccls company-lsp lsp-ui lsp-mode flycheck treemacs-magit treemacs-projectile treemacs-evil treemacs pdf-tools helm-gtags imenu-list objc-font-lock neotree company magit vlf flx-isearch flx-ido flx projectile dark-souls haskell-mode ztree with-editor wgrep use-package undo-tree transient tablist spinner shrink-path s rich-minority pyvenv popup pkg-info pfuture memoize markdown-mode magit-popup lv let-alist hydra ht highlight-indentation helm goto-chg git-commit find-file-in-project f evil epl epc doom-modeline deferred dash-functional dash ctable concurrent bind-key avy async all-the-icons ace-window)))
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
;; Theme
;;==============================================================================

(defun init-doom-theme ()
  (use-package doom-themes
    :ensure t
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled

    ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each
    ;; theme may have their own settings.
    (load-theme 'doom-one t)
    ;(load-theme 'doom-dracula t)
    ;(load-theme 'doom-city-lights t)
    ;(load-theme 'doom-molokai t)
    ;(load-theme 'doom-nord t)
    ;(load-theme 'doom-nova t)
    ;(load-theme 'doom-peacock t)
    ;(load-theme 'doom-solarized-light t)
    ;(load-theme 'doom-spacegrey t)
    ;(load-theme 'doom-tomorrow-night t)
    ;(load-theme 'doom-tomorrow-day t)

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)

    ;; Enable custom neotree theme
    (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))

  (use-package doom-modeline
    :ensure t
    :hook (after-init . doom-modeline-mode))

  ;; How tall the mode-line should be (only respected in GUI Emacs).
  (setq doom-modeline-height 25)

  ;; How wide the mode-line bar should be (only respected in GUI Emacs).
  (setq doom-modeline-bar-width 3)

  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  ;;
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  ;;
  ;; If you are expereicing the laggy issue, especially while editing remote files
  ;; with tramp, please try `file-name' style.
  ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)

  ;; Whether show `all-the-icons' or not (if nil nothing will be showed).
  (setq doom-modeline-icon t)

  ;; Whether show the icon for major mode. It respects `doom-modeline-icon'.
  (setq doom-modeline-major-mode-icon t)

  ;; Display color icons for `major-mode'. It respects `all-the-icons-color-icons'.
  (setq doom-modeline-major-mode-color-icon nil)

  ;; Whether display minor modes or not. Non-nil to display in mode-line.
  (setq doom-modeline-minor-modes nil)

  ;; If non-nil, a word count will be added to the selection-info modeline segment.
  (setq doom-modeline-enable-word-count nil)

  ;; If non-nil, only display one number for checker information if applicable.
  (setq doom-modeline-checker-simple-format t)

  ;; Whether display perspective name or not. Non-nil to display in mode-line.
  (setq doom-modeline-persp-name t)

  ;; Whether display `lsp' state or not. Non-nil to display in mode-line.
  (setq doom-modeline-lsp t)

  ;; Whether display github notifications or not. Requires `ghub` package.
  (setq doom-modeline-github nil)

  ;; The interval of checking github.
  (setq doom-modeline-github-interval (* 30 60))

  ;; Whether display environment version or not
  (setq doom-modeline-env-version t)
  ;; Or for individual languages
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-env-enable-ruby t)
  (setq doom-modeline-env-enable-perl t)
  (setq doom-modeline-env-enable-go t)
  (setq doom-modeline-env-enable-elixir t)
  (setq doom-modeline-env-enable-rust t)

  ;; Change the executables to use for the language version string
  (setq doom-modeline-env-python-executable "python")
  (setq doom-modeline-env-ruby-executable "ruby")
  (setq doom-modeline-env-perl-executable "perl")
  (setq doom-modeline-env-go-executable "go")
  (setq doom-modeline-env-elixir-executable "iex")
  (setq doom-modeline-env-rust-executable "rustc")

  ;; Whether display mu4e notifications or not. Requires `mu4e-alert' package.
  (setq doom-modeline-mu4e t)

  ;; Whether display irc notifications or not. Requires `circe' package.
  (setq doom-modeline-irc t)

  ;; Function to stylize the irc buffer names.
  (setq doom-modeline-irc-stylize 'identity))

;;==============================================================================
;; Font
;;==============================================================================

;IBM 3270 (https://github.com/rbanffy/3270font)
;Hack (https://github.com/source-foundry/Hack)
;NanumGothicCoding (https://github.com/naver/nanumfont/blob/master/README.md)
(defun init-font ()
  ;(print (font-family-list))
  (cond
   ((string-equal system-type "darwin") ; Font path: ~/Library/Fonts
    (progn
      ;(set-frame-font "-PfEd-IBM 3270-normal-italic-normal-*-*-130-*-*-*-0-iso10646-1")
      (set-face-attribute 'default nil :height 115 :family "Hack")
      ;(set-face-attribute 'default nil :height 115 :family "monospace")
      ;(set-face-attribute 'default nil :height 115 :family "Menlo")
      ))
   ((string-equal system-type "gnu/linux") ; Font path: ~/.local/share/fonts
    (progn
      ;(set-frame-font "-PfEd-IBM 3270-normal-italic-normal-*-*-115-*-*-*-0-iso10646-1")
      (set-face-attribute 'default nil :height 95 :family "Hack")
      ;(set-face-attribute 'default nil :height 100 :family "Inconsolata")
      ;(set-face-attribute 'default nil :height 95 :family "FreeMono")
      ;(set-face-attribute 'default nil :height 100 :family "monospace")
      ;(set-face-attribute 'default nil :height 115 :family "Ubuntu Mono")
      ))))
;To resolve the problem that cells of a table on Org mode containing Hangul are broken
;https://crazia.tistory.com/entry/Emacs-24x-%EB%B2%84%EC%A0%BC-%ED%95%9C%EA%B8%80-%ED%8F%B0%ED%8A%B8-%EC%84%A4%EC%A0%95-orgmode-%EC%9D%98-%ED%95%9C%EA%B8%80-%ED%85%8C%EC%9D%B4%EB%B8%94-%EA%B9%A8%EC%A7%80%EC%A7%80-%EC%95%8A%EA%B2%8C-%EB%B3%B4%EC%9D%B4%EA%B8%B0
(set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))
(setq face-font-rescale-alist
      '((".*hiragino.*" . 1.2)
        ("NanumGothicCoding" . 1.2307692307692308)))

;;==============================================================================
;; fill-column-indicator
;;
;; https://www.emacswiki.org/emacs/FillColumnIndicator
;;==============================================================================

(load-file "~/.emacs.d/fill-column-indicator.el")
(require 'fill-column-indicator)
(setq fci-rule-column 79)
(setq fci-rule-width 1)
(setq fci-rule-color "#303030")
(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'org-mode-hook 'fci-mode)

;;==============================================================================
;; startup
;;==============================================================================

(setq default-input-method "korean-hangul")
(setq desktop-save-mode t)

(switch-to-buffer "*Messages*")
(setq default-directory my-default-directory) ; this line must be excuted after excuting '(switch-to-buffer "*Messages*")'.

(setenv "MANWIDTH" "72")

;; https://draculatheme.com/emacs/
(load-file "~/.emacs.d/dracula-theme.el")
(load-theme 'dracula t)
(dracula-setup-modeline-format)
;; or
;;(init-doom-theme)

(defun startup-on-gui ()
  (init-font)

  (menu-bar-mode -1) ; hide menu bar
  (tool-bar-mode -1) ; hide tool bar

  (set-frame-position (selected-frame) 0 0)
  ;(set-frame-width (selected-frame) 150)
  ;(set-frame-height (selected-frame) 100)

  (setq window-min-width (/ (display-pixel-height) 22))

  (defun toggle-fullscreen (&optional f)
    (interactive)
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))))

(defun startup-on-cui ()
  (setq window-min-width (/ (display-pixel-width) 5)))

(if (display-graphic-p)
    (startup-on-gui)
  (startup-on-cui))

;;==============================================================================
;; all-the-icons
;;==============================================================================

(use-package all-the-icons
  :ensure t
  :config (setq all-the-icons-scale-factor 1.0))

(use-package all-the-icons-ivy
  :ensure t
  :hook (after-init . all-the-icons-ivy-setup))

;;==============================================================================
;; org
;;
;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/
;;==============================================================================

;; Hide the markup for /italic/, *bold*, _underline_
(setq org-hide-emphasis-markers t)

;; Better Bullets
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Better Header Bullets
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Line and Indentation Mode
(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode t)
            (org-indent-mode t)))

;;==============================================================================
;; markdown
;;==============================================================================

(use-package markdown-mode
  :ensure t)

;;==============================================================================
;; ag, rg, dumb-jump
;;==============================================================================

(use-package ag
  :ensure t)

(use-package rg
  :ensure t)

(use-package dumb-jump
  :ensure t)

(use-package deadgrep
  :ensure t)

;;==============================================================================
;; helm
;;==============================================================================

(use-package helm
  :ensure t)

(use-package helm-gtags
  :ensure t)

(use-package helm-ag
  :ensure t)

(use-package helm-rg
  :ensure t)

;;==============================================================================
;; evil
;;==============================================================================

(use-package evil
  :ensure t)

;;==============================================================================
;; hydra
;;==============================================================================

(use-package hydra
  :ensure t)

;;==============================================================================
;; ace-window
;;==============================================================================

(use-package ace-window
  :ensure t)

;;==============================================================================
;; magit
;;==============================================================================

(use-package magit
  :ensure t)

(use-package magit-popup
  :ensure t)

;;==============================================================================
;; projectile
;;==============================================================================

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t))

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

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

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
;; avy
;;==============================================================================

(use-package avy
  :ensure t)

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

;;==============================================================================
;; flycheck
;;==============================================================================

(use-package flycheck
  :ensure t)
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
