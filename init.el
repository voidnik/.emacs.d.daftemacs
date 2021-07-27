;; Editor: Richard Jaeho Hur

;;==============================================================================
;; Setup Frame and Font
;;==============================================================================

(defun setup-frame ()
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-frame-position (selected-frame) 0 0))

(defun setup-font ()
  ;; Source Code Pro (https://github.com/adobe-fonts/source-code-pro)
  ;; Office Code Pro (https://github.com/nathco/Office-Code-Pro)
  ;; Menlo (https://github.com/hbin/top-programming-fonts
  ;; IBM 3270 (https://github.com/rbanffy/3270font)
  ;; Hack (https://github.com/source-foundry/Hack)
  ;; D2Coding (https://github.com/naver/d2codingfont)
  ;; NanumGothicCoding (https://github.com/naver/nanumfont/blob/master/README.md)
  ;;(print (font-family-list))
  (cond
   ((string-equal system-type "darwin") ;; Font path: ~/Library/Fonts
    (progn
      (set-face-attribute 'default nil :height 120 :family "Source Code Pro")
      ;;(set-face-attribute 'default nil :height 120 :family "Office Code Pro")
      ;;(set-face-attribute 'default nil :height 120 :family "Office Code Pro D")
      ;;(set-face-attribute 'default nil :height 115 :family "Menlo")
      ;;(set-face-attribute 'default nil :height 115 :family "Hack")
      ;;(set-face-attribute 'default nil :height 115 :family "FiraCode")
      ;;(set-face-attribute 'default nil :height 115 :family "monospace")
      ;;(set-face-attribute 'default nil :height 115 :family "D2Coding")
      ;;(set-frame-font "-PfEd-IBM 3270-normal-italic-normal-*-*-130-*-*-*-0-iso10646-1")
      ))
   ((string-equal system-type "gnu/linux") ;; Font path: ~/.local/share/fonts
    (progn
      (set-face-attribute 'default nil :height 95 :family "Source Code Pro")
      ;;(set-face-attribute 'default nil :height 95 :family "Office Code Pro")
      ;;(set-face-attribute 'default nil :height 95 :family "Office Code Pro D")
      ;;(set-face-attribute 'default nil :height 95 :family "Menlo")
      ;;(set-face-attribute 'default nil :height 95 :family "Hack")
      ;;(set-face-attribute 'default nil :height 95 :family "FiraCode")
      ;;(set-face-attribute 'default nil :height 100 :family "monospace")
      ;;(set-face-attribute 'default nil :height 95 :family "D2Coding")
      ;;(set-frame-font "-PfEd-IBM 3270-normal-italic-normal-*-*-115-*-*-*-0-iso10646-1")
      ;;(set-face-attribute 'default nil :height 100 :family "Inconsolata")
      ;;(set-face-attribute 'default nil :height 95 :family "FreeMono")
      ;;(set-face-attribute 'default nil :height 115 :family "Ubuntu Mono")
      )))

  ;; To resolve the problem that cells of a table on Org mode containing Hangul are broken
  ;; https://crazia.tistory.com/entry/Emacs-24x-%EB%B2%84%EC%A0%BC-%ED%95%9C%EA%B8%80-%ED%8F%B0%ED%8A%B8-%EC%84%A4%EC%A0%95-orgmode-%EC%9D%98-%ED%95%9C%EA%B8%80-%ED%85%8C%EC%9D%B4%EB%B8%94-%EA%B9%A8%EC%A7%80%EC%A7%80-%EC%95%8A%EA%B2%8C-%EB%B3%B4%EC%9D%B4%EA%B8%B0
  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))
  (setq face-font-rescale-alist
        '((".*hiragino.*" . 1.2)
          ("NanumGothicCoding" . 1.2307692307692308))))

(when (display-graphic-p)
  (setup-frame)
  (setup-font))

;;==============================================================================
;; Info Messages
;;==============================================================================

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
(if (functionp 'json-serialize)
    (message "Native JSON: Enabled")
  (message "Native JSON: Disabled"))

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
 '(initial-frame-alist '((fullscreen . maximized)))
 '(org-file-apps
   '((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . emacs)))
 '(package-selected-packages
   '(centered-window command-log-mode perspective magic-latex-buffer px page-break-lines ein exec-path-from-shell yaml-mode hide-mode-line lsp-pyright centaur-tabs use-package bind-key dashboard google-c-style i3wm-config-mode peep-dired swift-mode focus cuda-mode org-bullets bufler org-re-reveal markdown-preview-mode graphviz-dot-mode ivy counsel counsel-projectile swiper ivy-posframe ivy-rich all-the-icons-ivy all-the-icons-ivy-rich lsp-ivy diff-hl company-statistics treemacs-icons-dired qml-mode highlight-indent-guides lsp-treemacs keyfreq neato-graph-bar epc importmagic pip-requirements py-autopep8 elpy json-reformat yasnippet rg deadgrep ripgrep helm-rg ag helm-ag dumb-jump ccls lsp-ui lsp-mode flycheck spell-fu treemacs-magit treemacs-projectile treemacs-evil treemacs pdf-tools helm-gtags imenu-list objc-font-lock neotree company magit vlf flx-isearch flx-ido flx projectile haskell-mode lua-mode ztree undo-tree shrink-path rich-minority pyvenv markdown-mode magit-popup highlight-indentation helm find-file-in-project evil doom-themes doom-modeline avy all-the-icons ace-window)))
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
 '(ediff-odd-diff-C ((t (:background "#464752"))))
 '(mode-line ((t (:background "#1E2029")))))

(setq default-input-method "korean-hangul")
(setq desktop-save-mode t)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
;;(setq uniquify-separator "/")
(setq uniquify-buffer-name-style 'post-forward)

(setenv "MANWIDTH" "72")

;; How to speed up TRAMP?
;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
(setq remote-file-name-inhibit-cache nil) ;; Set remote-file-name-inhibit-cache to nil if remote files are not independently updated outside TRAMP’s control.
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
(setq vc-handled-backends '(SVN Git))
(setq tramp-verbose 2)

;;==============================================================================
;; exec-path-from-shell
;;
;; https://github.com/purcell/exec-path-from-shell
;;==============================================================================

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)))

;;==============================================================================
;; all-the-icons
;;==============================================================================

(use-package all-the-icons
  :ensure t
  :config
  (setq all-the-icons-scale-factor 1.0))

;;==============================================================================
;; doom-themes
;;==============================================================================

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each
  ;; theme may have their own settings.
  (load-theme 'doom-dracula t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; https://draculatheme.com/emacs/
;(load-file "~/.emacs.d/dracula-theme.el")
;(load-theme 'dracula t)

;;==============================================================================
;; flycheck
;;==============================================================================

(use-package flycheck
  :ensure t)
;(add-hook 'after-init-hook #'global-flycheck-mode)

;;==============================================================================
;; magit
;;==============================================================================

(use-package magit
  :ensure t)

(use-package magit-popup
  :ensure t)

;;==============================================================================
;; Modeline
;;==============================================================================

(defun setup-doom-modeline ()
  (use-package doom-modeline
    :ensure t
    :hook (after-init . doom-modeline-mode))

  ;; How tall the mode-line should be (only respected in GUI Emacs).
  (setq doom-modeline-height 24)

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
  (setq doom-modeline-major-mode-color-icon t)

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

(setup-doom-modeline)
;(dracula-setup-modeline-format)

;;==============================================================================
;; page-break-lines
;;
;; https://github.com/purcell/page-break-lines
;;==============================================================================

(use-package page-break-lines
  :ensure t)

;;==============================================================================
;; dashboard
;;
;; https://github.com/emacs-dashboard/emacs-dashboard
;;==============================================================================

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents . 10)
                          (bookmarks . 10)
                          (projects . 10)
                          (agenda . 10)
                          (registers . 10)))
  (setq dashboard-page-separator "\n\f\n")) ;; This depends on page-break-lines.

;;==============================================================================
;; centaur-tabs
;;
;; https://github.com/ema2159/centaur-tabs
;;==============================================================================

(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "*"
        centaur-tabs-set-close-button nil
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-cycle-scope 'tabs)
  (if (string-equal system-type "darwin")
      (setq centaur-tabs-height 30)
    (if (string-equal system-type "gnu/linux")
        (if (string-equal (getenv "GDK_SCALE") "2")
            (setq centaur-tabs-height 60)
          (setq centaur-tabs-height 30))))
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)

  ;; Projectile integration
  (centaur-tabs-group-by-projectile-project)

  ;; Buffer groups
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
	 (cond
      ((string-prefix-p "*ein" (buffer-name))
       "EIN")
	  ((or (string-equal "*" (substring (buffer-name) 0 1))
	       (memq major-mode '(magit-process-mode
				              magit-status-mode
				              magit-diff-mode
				              magit-log-mode
				              magit-file-mode
				              magit-blob-mode
				              magit-blame-mode
				              )))
	   "Emacs")
	  ((derived-mode-p 'prog-mode)
	   "Editing")
	  ((derived-mode-p 'dired-mode)
	   "Dired")
	  ((memq major-mode '(helpful-mode
			              help-mode))
	   "Help")
	  ((memq major-mode '(org-mode
			              org-agenda-clockreport-mode
			              org-src-mode
			              org-agenda-mode
			              org-beamer-mode
			              org-indent-mode
			              org-bullets-mode
			              org-cdlatex-mode
			              org-agenda-log-mode
			              diary-mode))
	   "OrgMode")
	  (t
	   (centaur-tabs-get-group-name (current-buffer))))))

  ;; Prevent the access to specified buffers
  (defun centaur-tabs-hide-tab (x)
    "Do no to show buffer X in tabs."
    (let ((name (format "%s" x)))
      (or
       ;; Current window is not dedicated window.
       (window-dedicated-p (selected-window))

       ;; Buffer name not match below blacklist.
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*company" name)
       (string-prefix-p "*Flycheck" name)
       (string-prefix-p "*tramp" name)
       (string-prefix-p " *Mini" name)
       (string-prefix-p "*help" name)
       (string-prefix-p "*straight" name)
       (string-prefix-p " *temp" name)
       (string-prefix-p "*Help" name)
       (string-prefix-p "*Ilist" name)
       (string-prefix-p "*Ediff" name)
       (string-prefix-p "*Bufler" name)
       (string-prefix-p "*ein: LaTeX in Markdown preview*" name)

       ;; Is not magit buffer.
       (and (string-prefix-p "magit" name)
	        (not (file-name-extension name)))
       )))
  :hook
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)  
  :bind
  ("M-[" . centaur-tabs-backward)
  ("M-]" . centaur-tabs-forward)
  ("C-M-{" . centaur-tabs-move-current-tab-to-left)
  ("C-M-}" . centaur-tabs-move-current-tab-to-right)
  ("C-c t s" . centaur-tabs-counsel-switch-group)
  ("C-c t p" . centaur-tabs-group-by-projectile-project)
  ("C-c t g" . centaur-tabs-group-buffer-groups)
  ("C-c t k" . centaur-tabs-kill-all-buffers-in-current-group))

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

;; Change size of the inline image for LaTeX fragment in org-mode
;; https://tex.stackexchange.com/questions/78501/change-size-of-the-inline-image-for-latex-fragment-in-emacs-org-mode
(plist-put org-format-latex-options :scale 2)

;;==============================================================================
;; markdown
;;==============================================================================

(use-package markdown-mode
  :ensure t)

;;==============================================================================
;; LaTeX Preview
;;==============================================================================

;; https://github.com/aaptel/preview-latex
(use-package px
  :ensure t)

;; https://github.com/zk-phi/magic-latex-buffer
(use-package magic-latex-buffer
  :ensure t
  :config
  ;;(add-hook 'latex-mode-hook 'magic-latex-buffer)
  (setq magic-latex-enable-block-highlight nil
      magic-latex-enable-suscript        t
      magic-latex-enable-pretty-symbols  t
      magic-latex-enable-block-align     nil
      magic-latex-enable-inline-image    nil
      magic-latex-enable-minibuffer-echo nil))

;;==============================================================================
;; grep variants
;;==============================================================================

(use-package ag
  :ensure t)

(use-package rg
  :ensure t)

(use-package ripgrep
  :ensure t)

(use-package deadgrep
  :ensure t)

(use-package dumb-jump
  :ensure t
  :config
  (dumb-jump-mode))

;;==============================================================================
;; dired
;;==============================================================================

;; peep-dired: preview files in dired
(use-package peep-dired
  :ensure t
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

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
;; ace-window
;;==============================================================================

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;;==============================================================================
;; ztree
;;==============================================================================

(use-package ztree
  :ensure t)

;;==============================================================================
;; projectile
;;==============================================================================

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t)
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it)))

;;==============================================================================
;; company
;;==============================================================================

(use-package company
  :ensure t
  :diminish company-mode
  :commands (company-complete company-mode)
  :bind (;([remap dabbrev-expand] . company-complete)
         ("C-." . company-complete)
         ("C->" . counsel-company)
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

(use-package all-the-icons-ivy
  :ensure t
  :hook (after-init . all-the-icons-ivy-setup))

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
  (use-package all-the-icons-ivy-rich
    :ensure t
    :init (all-the-icons-ivy-rich-mode 1))
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
           ((counsel-M-x-transformer (:width 60))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 60))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 60))
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
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
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  ;; In order to make the changes of ivy-rich-display-transformers-list take effect,
  ;; ivy-rich-mode should be activated after emacs-startup.
  (add-hook 'emacs-startup-hook (lambda () (ivy-rich-mode +1))))

;;(use-package ivy-posframe
;;  :ensure t
;;  :after ivy
;;  :diminish
;;  :config
;;  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
;;        ivy-posframe-height-alist '((t . 30))
;;        ivy-posframe-parameters '((internal-border-width . 10)))
;;  (setq ivy-posframe-width 150)
;;  (ivy-posframe-mode +1))

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
;; flx
;;==============================================================================

(use-package flx
  :ensure t)

;;==============================================================================
;; flx-ido
;;==============================================================================

(use-package flx-ido
  :ensure t
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

;;==============================================================================
;; flx-isearch
;;==============================================================================

(use-package flx-isearch
  :ensure t)

;;==============================================================================
;; spell-fu
;;
;; https://gitlab.com/ideasman42/emacs-spell-fu
;;==============================================================================

(use-package spell-fu
  :ensure t
  :config
  ;(global-spell-fu-mode)
  (add-hook 'org-mode-hook
            (lambda ()
              (setq spell-fu-faces-exclude '(org-meta-line org-link org-code))
              (spell-fu-mode)))
  ;(add-hook 'emacs-lisp-mode-hook
  ;          (lambda ()
  ;            (spell-fu-mode)))
  )

;;==============================================================================
;; treemacs
;;==============================================================================

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         40)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))

  (defun treemacs-hide()
    "Close the Treemacs window."
    (interactive)
    (let ((treemacs-local-window (treemacs-get-local-window)))
      (if treemacs-local-window
          (delete-window treemacs-local-window))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;;==============================================================================
;; neotree
;;==============================================================================

(use-package neotree
  :ensure t
  :config
  (progn
    (setq neo-window-width 40
          neo-window-fixed-size t
          neo-smart-open t
          projectile-switch-project-action 'neotree-projectile-action) ;; To open neotree when projectile project is opend.

    (defun neotree-toggle-project-root-dir-or-current-dir ()
      "Open NeoTree using the project root, using projectile, or the current buffer directory."
      (interactive)
      (let ((project-dir (ignore-errors (projectile-project-root)))
            (file-name (buffer-file-name)))
        (if (neo-global--window-exists-p)
            (neotree-hide)
	      (progn
            (neotree-show)
            (if project-dir
                (neotree-dir project-dir))
            (if file-name
                (neotree-find file-name))))))

    (defun neotree-show-project-root-dir ()
      "Show NeoTree using the project root using projectile."
      (interactive)
      (let ((project-dir (projectile-project-root))
            (file-name (buffer-file-name)))
        (progn
          (neotree-show)
	      (if project-dir
              (neotree-dir project-dir))
	      (if file-name
              (neotree-find file-name))))))
  :bind
  (:map global-map
        ("C-x t n" . neotree-toggle-project-root-dir-or-current-dir)))

;;==============================================================================
;; perspective
;;
;; https://github.com/nex3/perspective-el
;;==============================================================================

(use-package perspective
  :ensure t
  :config
  (persp-mode)

  (setq persp-sort 'access)
  (setq persp-state-default-file "~/.emacs.d/persp-state-default")

  (message "persp-state-default-file: %s (%s)" persp-state-default-file (file-exists-p persp-state-default-file))
  (add-hook 'dashboard-after-initialize-hook #'(lambda ()
                                                 (when (file-exists-p persp-state-default-file)
                                                   (persp-state-load persp-state-default-file))))
  (add-hook 'persp-before-switch-hook #'(lambda ()
                                          (treemacs-hide)
                                          (neotree-hide)))
  (add-hook 'kill-emacs-hook #'(lambda ()
                                 (treemacs-hide)
                                 (neotree-hide)
                                 (persp-state-save)))

  ;; Overriding centaur-tabs-buffer-list
  (defun centaur-tabs-buffer-list ()
    "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space, when they are not
visiting a file.  The current buffer is always included."
    (centaur-tabs-filter-out
     'centaur-tabs-hide-tab-cached
     (delq nil
	       (cl-mapcar #'(lambda (b)
			              (cond
			               ;; Always include the current buffer.
			               ((eq (current-buffer) b) b)
			               ((buffer-file-name b) b)
			               ((char-equal ?\  (aref (buffer-name b) 0)) nil)
			               ((buffer-live-p b) b)))
		              (persp-buffer-list-filter(buffer-list))))))

  ;; Overrinding ibuffer-update
  (defun ibuffer-update (arg &optional silent)
    "Regenerate the list of all buffers.

Prefix arg non-nil means to toggle whether buffers that match
`ibuffer-maybe-show-predicates' should be displayed.

If optional arg SILENT is non-nil, do not display progress messages."
    (interactive "P")
    (if arg
        (setq ibuffer-display-maybe-show-predicates
	          (not ibuffer-display-maybe-show-predicates)))
    (ibuffer-forward-line 0)
    (let* ((bufs (persp-buffer-list-filter(buffer-list)))
	       (blist (ibuffer-filter-buffers
		           (current-buffer)
		           (if (and
		                (cadr bufs)
		                (eq ibuffer-always-show-last-buffer
			                :nomini)
		                (minibufferp (cadr bufs)))
		               (nth 2 bufs)
		             (cadr bufs))
		           (ibuffer-current-buffers-with-marks bufs)
		           ibuffer-display-maybe-show-predicates)))
      (and (null blist)
	       (featurep 'ibuf-ext)
	       ibuffer-filtering-qualifiers
	       (message "No buffers! (note: filtering in effect)"))
      (unless silent
        (message "Updating buffer list..."))
      (ibuffer-redisplay-engine blist arg)
      (unless silent
        (message "Updating buffer list...done")))
    (if (eq ibuffer-shrink-to-minimum-size 'onewindow)
        (ibuffer-shrink-to-fit t)
      (when ibuffer-shrink-to-minimum-size
        (ibuffer-shrink-to-fit)))
    (ibuffer-forward-line 0)
    ;; I tried to update this automatically from the mode-line-process format,
    ;; but changing nil-ness of header-line-format while computing
    ;; mode-line-format is asking a bit too much it seems.  --Stef
    (setq header-line-format
          (and ibuffer-use-header-line
               ibuffer-filtering-qualifiers
               ibuffer-header-line-format))))

;;==============================================================================
;; Bufler
;;
;; https://github.com/alphapapa/bufler.el
;;==============================================================================

(use-package bufler
  :ensure t)

;;==============================================================================
;; Code Style
;;==============================================================================

(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)

(use-package google-c-style
  :ensure t
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

(add-hook 'c-mode-hook
          (lambda ()
            (setq c-basic-offset 2)
            (c-set-offset 'substatement-open 0)))

(add-hook 'c++-mode-hook
          (lambda ()
            (setq c-basic-offset 2)
            (c-set-offset 'substatement-open 0)))

(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 2)
            (c-set-offset 'substatement-open 0)))

(add-hook 'js-mode-hook
          (lambda ()
            (setq js-indent-level 2)))

;;==============================================================================
;; highlight-indent-guides
;;
;; https://github.com/DarthFennec/highlight-indent-guides
;;==============================================================================

(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'top))

;;==============================================================================
;; highlight-indentation
;;==============================================================================

(use-package highlight-indentation
  :ensure t)

;;==============================================================================
;; cc-search-directories
;;==============================================================================

(setq cc-search-directories '("." "../include" "/usr/include" "/usr/local/include/*"
                              "/System/Library/Frameworks" "/Library/Frameworks"))

;;==============================================================================
;; Objective C
;;
;; https://www.emacswiki.org/emacs/ObjectiveCMode
;;==============================================================================

(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

(add-to-list 'magic-mode-alist
             `(,(lambda ()
                  (and (buffer-file-name)
                       (string= (file-name-extension buffer-file-name) "h")
                       (re-search-forward "@\\<interface\\>"
                                          magic-mode-regexp-match-limit t)))
               . objc-mode))

(require 'find-file) ;; for the "cc-other-file-alist" variable
(nconc (cadr (assoc "\\.h\\'" cc-other-file-alist)) '(".m" ".mm"))

(defadvice ff-get-file-name (around ff-get-file-name-framework
                                    (search-dirs
                                     fname-stub
                                     &optional suffix-list))
  "Search for Mac framework headers as well as POSIX headers."
  (or
   (if (string-match "\\(.*?\\)/\\(.*\\)" fname-stub)
       (let* ((framework (match-string 1 fname-stub))
              (header (match-string 2 fname-stub))
              (fname-stub (concat framework ".framework/Headers/" header)))
         ad-do-it))
   ad-do-it))
(ad-enable-advice 'ff-get-file-name 'around 'ff-get-file-name-framework)
(ad-activate 'ff-get-file-name)

;;==============================================================================
;; Swift
;;
;; https://github.com/swift-emacs/swift-mode
;;==============================================================================

(use-package swift-mode
  :ensure t)

;;==============================================================================
;; objc-font-lock
;;==============================================================================

(use-package objc-font-lock
  :ensure t)

;;==============================================================================
;; Language Server Protocol (LSP)
;;
;; https://github.com/emacs-lsp/lsp-mode
;; https://github.com/MaskRay/ccls
;; https://github.com/MaskRay/emacs-ccls
;;==============================================================================

(use-package lsp-mode
  :ensure t
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :ensure t
  :after lsp-mode)

(use-package lsp-ivy
  :ensure t
  :after lsp-mode)

(setq lsp-prefer-flymake nil)
;(setq lsp-enable-file-watchers nil)
(setq lsp-file-watch-threshold 2000)

(lsp-ui-mode)

;;==============================================================================
;; C/C++
;;
;; - ccls
;; 1) Dependencies
;;   RapidJSON: https://rapidjson.org/
;;     Ubuntu: $ sudo apt install rapidjson-dev
;;     MacOS brew: $ brew install rapidjson
;; 2) Building ccls
;;   $ git clone --depth=1 --recursive https://github.com/MaskRay/ccls
;;   $ cd ccls
;;   Ubuntu: $ cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=/usr/lib/llvm-7
;;   MacOS brew: $ cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=/usr/local/opt/llvm
;;   $ cmake --build Release
;; 3) Installing ccls on MacOS brew (https://github.com/twlz0ne/homebrew-ccls)
;;   $ brew tap twlz0ne/homebrew-ccls
;;   $ brew install ccls
;;==============================================================================

(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))
(setq ccls-executable "~/.emacs.d/ccls/Release/ccls")
;TODO
;(setq
; ccls-initialization-options
; `(:index (:multiVersion 1 :trackDependency 1)))

;; # Indentation for ccls
;;
;; https://github.com/MaskRay/ccls/issues/459
;; https://clang.llvm.org/docs/ClangFormatStyleOptions.html
;;
;; 1) Place .clang-format in the project root, e.g.
;;
;; $ emacs .clang-format
;; BasedOnStyle: LLVM
;; IndentWidth: 4
;;
;; 2) If you don't want automatic type formatting, set lsp-enable-on-type-formatting to nil as follows.
;;
;; (setq lsp-enable-on-type-formatting nil)

(setq lsp-file-watch-ignored
      (append lsp-file-watch-ignored
              '("[/\\\\]\\.ccls-cache$"
                "[/\\\\]\\.deps$"
                "[/\\\\]\\.libs$")))

(add-to-list 'projectile-globally-ignored-directories ".ccls-cache")

;;==============================================================================
;; Python
;;
;; - elpy (https://github.com/jorgenschaefer/elpy)
;;   elpy docs (https://elpy.readthedocs.io/en/latest/index.html)
;;   # Python
;;   $ sudo apt install python3
;;   $ sudo apt install python
;;   # virtualenv
;;   $ sudo apt install python3-venv
;;   $ pip3 install virtualenv
;;   # Completion and code navigation
;;   $ pip3 install jedi
;;   # Code checks
;;   $ pip3 install flake8
;;   # Refactoring
;;   $ pip3 install rope
;;   # Automatic formatting (PEP8, Yapf or Black)
;;   $ pip3 install autopep8
;;   $ pip3 install yapf
;;   $ pip3 install black (only available on Python 3)
;;
;;   # Install Python Dependencies
;;   (setq elpy-rpc-python-command "python3")
;;   M-x elpy-rpc-reinstall-virtualenv
;;
;; - importmagic (https://github.com/anachronic/importmagic.el)
;;   $ pip3 install importmagic epc
;;   or
;;   $ sudo apt install python3-importmagic python3-epc
;;
;; - lsp-pyright (https://github.com/emacs-lsp/lsp-pyright)
;;   $ npm install -g pyright
;;==============================================================================

(setq python-shell-interpreter "python3"
      python-shell-completion-native-enable nil)

(use-package python
  :mode ("\\.py" . python-mode))

(use-package pip-requirements
  :ensure t
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

(use-package py-autopep8
  :ensure t
  :config
  ;(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  )

(use-package epc
  :ensure t)

(use-package importmagic
  :ensure t
  :config
  (add-hook 'python-mode-hook 'importmagic-mode)
  (setq importmagic-python-interpreter "~/anaconda3/bin/python3"))

(use-package pyvenv
  :ensure t
  :config
  (cond
   ((file-directory-p "~/anaconda3/envs")
    (setenv "WORKON_HOME" "~/anaconda3/envs"))
   ((file-directory-p "~/opt/anaconda3/envs")
    (setenv "WORKON_HOME" "~/opt/anaconda3/envs")))
  (pyvenv-mode 1))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))
  :config
  (add-hook 'pyvenv-post-activate-hooks (lambda () (lsp-restart-workspace)))
  (add-hook 'pyvenv-post-deactivate-hooks (lambda () (lsp-restart-workspace))))

;;(use-package elpy
;;  :ensure t
;;  :init
;;  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
;;  :config
;;  (setq elpy-rpc-python-command "python3")
;;  (setq eldoc-idle-delay 1)  ;; in second
;;  (defun python-send-buffer-with-args (args)
;;    (interactive "sPython arguments: ")
;;    (let ((source-buffer (current-buffer))
;;          (current-buffer-name (buffer-name)))
;;      (with-temp-buffer
;;        (insert "import sys; sys.argv = '''" current-buffer-name " " args "'''.split()\n")
;;        (insert-buffer-substring source-buffer)
;;        (elpy-shell-send-buffer))))
;;  (define-key elpy-mode-map (kbd "C-c C-a") 'python-send-buffer-with-args)
;;  :bind (:map elpy-mode-map
;;              ("M-." . elpy-goto-definition)
;;              ("M-," . pop-tag-mark)))
;;(elpy-enable)

;; EIN -- Emacs IPython Notebook
;; https://github.com/millejoh/emacs-ipython-notebook
(use-package ein
  :ensure t
  :config
  ;; https://github.com/millejoh/emacs-ipython-notebook/issues/88
  (defun ein:preview-md-cell-latex ()
    "Preview LaTeX from the current markdown cell in a separate buffer."
    (interactive)
    (let* ((cell (ein:worksheet-get-current-cell))
	       (buffer (if (ein:markdowncell-p cell)
		               (get-buffer-create "*ein: LaTeX in Markdown preview*")
		             (error "Not on a markdown cell"))))
      (with-current-buffer buffer
        (when buffer-read-only
	      (toggle-read-only))
        (unless (= (point-min) (point-max))
	      (delete-region (point-min) (point-max)))
        (insert (slot-value cell :input))
        (goto-char (point-min))
        (org-mode)
        (org-toggle-latex-fragment 16)
        (special-mode)
        (unless buffer-read-only
	      (toggle-read-only))
        (display-buffer
         buffer
         '((display-buffer-below-selected display-buffer-at-bottom)
           (inhibit-same-window . t)))
        (fit-window-to-buffer (window-in-direction 'below))))))

;;==============================================================================
;; haskell-mode
;;==============================================================================

(use-package haskell-mode
  :ensure t)

;;==============================================================================
;; lua-mode
;;==============================================================================

(use-package lua-mode
  :ensure t
  :config
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

;;==============================================================================
;; cuda-mode
;;==============================================================================

(use-package cuda-mode
  :ensure t)

;;==============================================================================
;; yaml-mode
;;==============================================================================

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook
            #'(lambda ()
                (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;;==============================================================================
;; qml-mode
;;==============================================================================

(use-package qml-mode
  :ensure t
  :config
  (autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
  (add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode)))

;;==============================================================================
;; i3wm-config-mode
;;==============================================================================

(use-package i3wm-config-mode
  :ensure t)

;;==============================================================================
;; focus
;;==============================================================================

(use-package focus
  :ensure t)

;;==============================================================================
;; rich-minority
;;==============================================================================

(use-package rich-minority
  :ensure t)

;;==============================================================================
;; hide-mode-line
;;==============================================================================

(use-package hide-mode-line
  :ensure t)

;;==============================================================================
;; pdf-tools
;;==============================================================================

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (add-hook 'pdf-tools-enabled-hook (lambda ()
                                      (hide-mode-line-mode)
                                      ;;(pdf-view-midnight-minor-mode)
                                      )))

;;==============================================================================
;; graphviz
;;==============================================================================

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

(use-package company-graphviz-dot)

;;==============================================================================
;; ediff
;;
;; https://oremacs.com/2015/01/17/setting-up-ediff/
;;==============================================================================

(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))
(csetq ediff-window-setup-function 'ediff-setup-windows-plain)
(csetq ediff-split-window-function 'split-window-horizontally)
(csetq ediff-diff-options "-w")
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;;==============================================================================
;; diff-hl
;;
;; diff-hl-mode highlights uncommitted changes on the left side of the window,
;; allows you to jump between and revert them selectively.
;;
;; https://github.com/dgutov/diff-hl
;;==============================================================================

(use-package diff-hl
  :ensure t
  :custom-face
  (diff-hl-insert ((t (:foreground "#50fa7b" :background "#50fa7b"))))
  (diff-hl-delete ((t (:foreground "#ff5555" :background "#ff5555"))))
  (diff-hl-change ((t (:foreground "#8be9fd" :background "#8be9fd"))))
  :config
  (global-diff-hl-mode +1)
  (diff-hl-flydiff-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh t))

;;==============================================================================
;; ansi-term
;;
;; https://oremacs.com/2015/01/01/three-ansi-term-tips/
;;==============================================================================

(cond
 ((string-equal system-type "darwin")
  (setq explicit-shell-file-name "/bin/zsh"))
 ((string-equal system-type "gnu/linux")
  (setq explicit-shell-file-name "/usr/bin/zsh")))

(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'oleh-term-exec-hook)

(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))

;;==============================================================================
;; markdown
;;==============================================================================

(setq markdown-command "pandoc")

;; markdown-preview-mode
;; https://github.com/ancane/markdown-preview-mode
(use-package markdown-preview-mode
  :ensure t
  :config
  (setq markdown-preview-stylesheets (list "~/.emacs.d/css/github-markdown.css"))
  (add-to-list 'markdown-preview-javascript "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML"))

;;==============================================================================
;; org-re-reveal
;;
;; https://gitlab.com/oer/org-re-reveal
;; https://github.com/hakimel/reveal.js
;;==============================================================================

(use-package org-re-reveal
  :ensure t
  :config
  (setq org-re-reveal-root (concat (getenv "HOME") "/.emacs.d/reveal.js")))

;;==============================================================================
;; undo-tree
;;
;; https://www.emacswiki.org/emacs/UndoTree
;;==============================================================================

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

;;==============================================================================
;; Eshell
;;==============================================================================

(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c k") (lambda () (interactive) (eshell/clear 1)))))

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
;; Killing Buffers
;;
;; https://www.emacswiki.org/emacs/KillingBuffers
;;==============================================================================

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

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
         "h=heighten, s=shrink, w=widen, n=narrow (by %d); 1-9=unit, b=balance, q=quit"
         arg)
        (setq c (read-char))
        (condition-case ()
            (cond
             ((= c ?h) (enlarge-window arg))
             ((= c ?s) (shrink-window arg))
             ((= c ?w) (enlarge-window-horizontally arg))
             ((= c ?n) (shrink-window-horizontally arg))
             ((= c ?\^G) (keyboard-quit))
             ((= c ?b) (balance-windows))
             ((= c ?q) (throw 'done t))
             ((and (> c ?0) (<= c ?9)) (setq arg (- c ?0)))
             (t (beep)))
          (error (beep)))))
    (message "Done.")))

;;==============================================================================
;; buffer-move
;;==============================================================================

(load-file "~/.emacs.d/buffer-move.elc")

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

(use-package vlf
  :ensure t)

;;==============================================================================
;; keyfreq
;;==============================================================================

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;==============================================================================
;; imenu-list
;;==============================================================================

(use-package imenu-list
  :ensure t)

;;==============================================================================
;; json-reformat
;;==============================================================================

(use-package json-reformat
  :ensure t)

;;==============================================================================
;; shrink-path
;;==============================================================================

(use-package shrink-path
  :ensure t)

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
;; Syntax Color Hex/HSL Color Strings
;;
;; http://ergoemacs.org/emacs/emacs_CSS_colors.html
;;==============================================================================

(defun syntax-color-hex ()
  "Syntax color text of the form 「#ff1100」 and 「#abc」 in current buffer."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[[:xdigit:]]\\{3\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background
                      (let* (
                             (ms (match-string-no-properties 0))
                             (r (substring ms 1 2))
                             (g (substring ms 2 3))
                             (b (substring ms 3 4)))
                        (concat "#" r r g g b b))))))
     ("#[[:xdigit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-flush))

(defun syntax-color-hsl ()
  "Syntax color CSS's HSL color spec eg 「hsl(0,90%,41%)」 in current buffer."
  (interactive)
  (require 'color)
  (font-lock-add-keywords
   nil
   '(("hsl( *\\([0-9]\\{1,3\\}\\) *, *\\([0-9]\\{1,3\\}\\)% *, *\\([0-9]\\{1,3\\}\\)% *)"
      (0 (put-text-property
          (+ (match-beginning 0) 3)
          (match-end 0)
          'face
          (list
           :background
           (concat
            "#"
            (mapconcat
             'identity
             (mapcar
              (lambda (x) (format "%02x" (round (* x 255))))
              (color-hsl-to-rgb
               (/ (string-to-number (match-string-no-properties 1)) 360.0)
               (/ (string-to-number (match-string-no-properties 2)) 100.0)
               (/ (string-to-number (match-string-no-properties 3)) 100.0)))
             "" )) ;  "#00aa00"
           ))))))
  (font-lock-flush))

(add-hook 'css-mode-hook 'syntax-color-hex)
(add-hook 'css-mode-hook 'syntax-color-hsl)
(add-hook 'php-mode-hook 'syntax-color-hex)
(add-hook 'php-mode-hook 'syntax-color-hsl)
(add-hook 'html-mode-hook 'syntax-color-hex)
(add-hook 'html-mode-hook 'syntax-color-hsl)

;;==============================================================================
;; neato-graph-bar
;;==============================================================================

(use-package neato-graph-bar
  :ensure t)

;;==============================================================================
;; centered-window
;;
;; https://github.com/anler/centered-window-mode
;;==============================================================================

(use-package centered-window
  :ensure t
  :config
  (setq-default
   cwm-centered-window-width 180
   cwm-incremental-padding t
   cwm-incremental-padding-% 1))

;;==============================================================================
;; Instant Stackoverflow Solutions
;;==============================================================================

(load-file "~/.emacs.d/stackoverflow.el")
(require 'stackoverflow)

;;==============================================================================
;; fcitx (OBSOLETE)
;;
;; https://github.com/cute-jumper/fcitx.el
;;==============================================================================

;;(if (string-equal system-type "gnu/linux")
;;    (use-package fcitx
;;      :ensure t
;;      :config
;;      (fcitx-aggressive-setup)
;;      (setq fcitx-use-dbus t)))

;;==============================================================================
;; gtags (OBSOLETE)
;;==============================================================================

;;;; Enable helm-gtags-mode
;;(add-hook 'c-mode-hook 'helm-gtags-mode)
;;(add-hook 'c++-mode-hook 'helm-gtags-mode)
;;(add-hook 'asm-mode-hook 'helm-gtags-mode)
;;
;;;; Set key bindings
;;(eval-after-load "helm-gtags"
;;  '(progn
;;     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
;;     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
;;     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
;;     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
;;     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;;     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
;;     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))

;;==============================================================================
;; etags (OBSOLETE)
;;==============================================================================

;;(defun create-tags (dir-name)
;;  "Create a TAGS file."
;;  (interactive "DDirectory: ")
;;  (setq c++-headers-path "/usr/include/c++")
;;  (with-current-buffer (get-buffer-create "*etags-output*") (erase-buffer))
;;  (execute-commands "*etags-output*"
;;                    (format "find -H %s -name \"*\" | xargs etags -o %sTAGS" c++-headers-path dir-name)
;;                    (format "find -H %s -type f \\( \
;;-name \"*.[csSh]\" \
;;-o \
;;-name \"*.cc\" \
;;-o \
;;-name \"*.cpp\"\
;;-o \
;;-name \"*.m\" \
;;-o \
;;-name \"*.java\" \
;;-o \
;;-name \"*.py\" \
;;-o \
;;-name \"*.pl\" \
;;\\) | \
;;xargs etags -a -o %sTAGS" dir-name dir-name)))

;;==============================================================================
;; find-file-in-tags (OBSOLETE)
;;==============================================================================

;;(load-file "~/.emacs.d/find-file-in-tags.el"
;;(global-set-key (kbd "C-c f") 'find-file-in-tags)

;;==============================================================================
;; Dim for #if 0 ... #endif (OBSOLETE)
;;==============================================================================

;;(defun cpp-highlight-if-0/1 ()
;;  "Modify the face of text in between #if 0 ... #endif."
;;  (setq cpp-known-face '(background-color . "gray15"))
;;  (setq cpp-unknown-face 'default)
;;  (setq cpp-face-type 'dark)
;;  (setq cpp-known-writable 't)
;;  (setq cpp-unknown-writable 't)
;;  (setq cpp-edit-list
;;        '((#("1" 0 1
;;             (fontified nil))
;;           nil
;;           (background-color . "gray15")
;;           both nil)
;;          (#("0" 0 1
;;             (fontified nil))
;;           (background-color . "gray15")
;;           nil
;;           both nil)))
;;  (cpp-highlight-buffer t))
;;(defun jpk/c-mode-hook ()
;;  (cpp-highlight-if-0/1)
;;  (add-hook 'after-save-hook 'cpp-highlight-if-0/1 'append 'local))
;;(add-hook 'c-mode-common-hook 'jpk/c-mode-hook)

;;==============================================================================
;; Global Keys
;;==============================================================================

(global-set-key (kbd "S-SPC") 'toggle-input-method)
(global-set-key (kbd "M-SPC") 'toggle-input-method)

(global-set-key (kbd "C-c o <up>")    'buf-move-up)
(global-set-key (kbd "C-c o <down>")  'buf-move-down)
(global-set-key (kbd "C-c o <left>")  'buf-move-left)
(global-set-key (kbd "C-c o <right>") 'buf-move-right)

(global-set-key (kbd "M-o") 'ff-find-other-file)
(global-set-key (kbd "M-m") 'lsp-ui-imenu)

(global-set-key (kbd "C-c z") 'resize-window)

(global-set-key (kbd "C-\\") 'ace-window)
(global-set-key (kbd "C-|") 'ace-swap-window)

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x C-b") 'persp-ibuffer)
(global-set-key (kbd "C-x b") 'persp-counsel-switch-buffer)
(global-set-key (kbd "C-x k") 'persp-kill-buffer*)

(global-set-key (kbd "C-c s") 'swiper)
(global-set-key (kbd "C-c r") 'counsel-register)
(global-set-key (kbd "C-c e") 'counsel-recentf)
(global-set-key (kbd "C-c f") 'counsel-projectile-find-file)
(global-set-key (kbd "C-c d") 'counsel-projectile-find-dir)
(global-set-key (kbd "C-c b") 'counsel-projectile-switch-to-buffer)
(global-set-key (kbd "C-c g") 'counsel-projectile-rg)
(global-set-key (kbd "C-c p") 'counsel-projectile-switch-project)

(global-set-key (kbd "C-c C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-c C-r") 'isearch-backward-regexp)

(global-set-key (kbd "s-_") 'whitespace-mode)

(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c 1") 'eshell)
(global-set-key (kbd "C-c 2") #'(lambda ()
                                  (interactive)
                                  (ansi-term explicit-shell-file-name)))
(global-set-key (kbd "C-c 3") #'(lambda ()
                                  (interactive)
                                  (calculator)
                                  (balance-windows)))
(global-set-key (kbd "C-c 4") 'neato-graph-bar)
(global-set-key (kbd "C-c 8") 'bufler)
(global-set-key (kbd "C-c 9") 'neotree-show-project-root-dir)
(global-set-key (kbd "C-c 0") 'treemacs-select-window)

;; GUD
(global-set-key [(shift f5)] 'gud-gdb)
(global-set-key [f5] 'gud-run)
(global-set-key [f6] 'gud-next)
(global-set-key [f7] 'gud-step)
(global-set-key [f8] #'(lambda ()
                        (interactive)
                        (call-interactively 'gud-tbreak)
                        (call-interactively 'gud-cont)))
(global-set-key [f9] 'gud-break)
(global-set-key [f10] 'gud-finish)
(global-set-key [f11] 'gdb-many-windows)

;;==============================================================================
;; Local Keys
;;==============================================================================

(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)

(define-key org-mode-map "\C-cv" 'visual-line-mode)

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map "\C-ca" 'py-autopep8)
            (define-key python-mode-map "\C-cv" 'pyvenv-workon)
            (define-key python-mode-map "\C-m" 'newline-and-indent)))

;;==============================================================================
;; command-log-mode
;;
;; https://github.com/lewang/command-log-mode
;;==============================================================================

(use-package command-log-mode
  :ensure t)
