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
      ;;(set-face-attribute 'default nil :height 125 :family "IBM 3270")
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
      ;;(set-face-attribute 'default nil :height 105 :family "IBM 3270")
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
   '(docker typescript-mode flx dockerfile-mode pretty-hydra string-utils org-tree-slide command-log-mode perspective magic-latex-buffer px page-break-lines ein exec-path-from-shell yaml-mode hide-mode-line lsp-pyright centaur-tabs use-package bind-key dashboard google-c-style i3wm-config-mode peep-dired swift-mode focus cuda-mode org-bullets org-re-reveal markdown-preview-mode graphviz-dot-mode ivy counsel counsel-projectile swiper ivy-posframe ivy-rich all-the-icons-ivy all-the-icons-ivy-rich lsp-ivy diff-hl treemacs-icons-dired qml-mode highlight-indent-guides lsp-treemacs keyfreq neato-graph-bar epc importmagic pip-requirements py-autopep8 elpy json-reformat yasnippet rg deadgrep ripgrep helm-rg ag helm-ag dumb-jump ccls lsp-ui lsp-mode flycheck spell-fu treemacs-magit treemacs-projectile treemacs pdf-tools helm-gtags imenu-list objc-font-lock neotree company company-fuzzy company-statistics company-box magit vlf projectile haskell-mode lua-mode ztree undo-tree shrink-path rich-minority pyvenv markdown-mode magit-popup highlight-indentation helm find-file-in-project evil doom-themes doom-modeline avy all-the-icons ace-window)))
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
;; string-utils
;;==============================================================================

(use-package string-utils
  :ensure t)

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
;; flx
;;==============================================================================

(use-package flx
  :ensure t)

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
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; https://draculatheme.com/emacs/
;;(load-file "~/.emacs.d/dracula-theme.el")
;;(load-theme 'dracula t)

;;==============================================================================
;; Modeline
;;==============================================================================

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  ;; How tall the mode-line should be. It's only respected in GUI.
  ;; If the actual char height is larger, it respects the actual height.
  (setq doom-modeline-height 24)

  ;; How wide the mode-line bar should be. It's only respected in GUI.
  (setq doom-modeline-bar-width 3)

  ;; The limit of the window width.
  ;; If `window-width' is smaller than the limit, some information won't be displayed.
  (setq doom-modeline-window-width-limit fill-column)

  ;; How to detect the project root.
  ;; nil means to use `default-directory'.
  ;; The project management packages have some issues on detecting project root.
  ;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
  ;; to hanle sub-projects.
  ;; You can specify one if you encounter the issue.
  (setq doom-modeline-project-detection 'auto)

  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  ;;
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   auto => emacs/lisp/comint.el (in a project) or comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  ;;
  ;; If you are experiencing the laggy issue, especially while editing remote files
  ;; with tramp, please try `file-name' style.
  ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
  (setq doom-modeline-buffer-file-name-style 'auto)

  ;; Whether display icons in the mode-line.
  ;; While using the server mode in GUI, should set the value explicitly.
  (setq doom-modeline-icon (display-graphic-p))

  ;; Whether display the icon for `major-mode'. It respects `doom-modeline-icon'.
  (setq doom-modeline-major-mode-icon t)

  ;; Whether display the colorful icon for `major-mode'.
  ;; It respects `all-the-icons-color-icons'.
  (setq doom-modeline-major-mode-color-icon t)

  ;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
  (setq doom-modeline-buffer-state-icon t)

  ;; Whether display the modification icon for the buffer.
  ;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
  (setq doom-modeline-buffer-modification-icon t)

  ;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
  (setq doom-modeline-unicode-fallback nil)

  ;; Whether display the minor modes in the mode-line.
  (setq doom-modeline-minor-modes nil)

  ;; If non-nil, a word count will be added to the selection-info modeline segment.
  (setq doom-modeline-enable-word-count nil)

  ;; Major modes in which to display word count continuously.
  ;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
  ;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
  ;; remove the modes from `doom-modeline-continuous-word-count-modes'.
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

  ;; Whether display the buffer encoding.
  (setq doom-modeline-buffer-encoding t)

  ;; Whether display the indentation information.
  (setq doom-modeline-indent-info nil)

  ;; If non-nil, only display one number for checker information if applicable.
  (setq doom-modeline-checker-simple-format t)

  ;; The maximum number displayed for notifications.
  (setq doom-modeline-number-limit 99)

  ;; The maximum displayed length of the branch name of version control.
  (setq doom-modeline-vcs-max-length 12)

  ;; Whether display the workspace name. Non-nil to display in the mode-line.
  (setq doom-modeline-workspace-name t)

  ;; Whether display the perspective name. Non-nil to display in the mode-line.
  (setq doom-modeline-persp-name t)

  ;; If non nil the default perspective name is displayed in the mode-line.
  (setq doom-modeline-display-default-persp-name nil)

  ;; If non nil the perspective name is displayed alongside a folder icon.
  (setq doom-modeline-persp-icon t)

  ;; Whether display the `lsp' state. Non-nil to display in the mode-line.
  (setq doom-modeline-lsp t)

  ;; Whether display the GitHub notifications. It requires `ghub' package.
  (setq doom-modeline-github nil)

  ;; The interval of checking GitHub.
  (setq doom-modeline-github-interval (* 30 60))

  ;; Whether display the modal state icon.
  ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  (setq doom-modeline-modal-icon t)

  ;;;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
  ;;(setq doom-modeline-mu4e nil)
  ;;;; also enable the start of mu4e-alert
  ;;(mu4e-alert-enable-mode-line-display)

  ;; Whether display the gnus notifications.
  (setq doom-modeline-gnus t)

  ;; Wheter gnus should automatically be updated and how often (set to 0 or smaller than 0 to disable)
  (setq doom-modeline-gnus-timer 2)

  ;; Wheter groups should be excludede when gnus automatically being updated.
  (setq doom-modeline-gnus-excluded-groups '("dummy.group"))

  ;; Whether display the IRC notifications. It requires `circe' or `erc' package.
  (setq doom-modeline-irc t)

  ;; Function to stylize the irc buffer names.
  (setq doom-modeline-irc-stylize 'identity)

  ;; Whether display the environment version.
  (setq doom-modeline-env-version t)
  ;; Or for individual languages
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-env-enable-ruby t)
  (setq doom-modeline-env-enable-perl t)
  (setq doom-modeline-env-enable-go t)
  (setq doom-modeline-env-enable-elixir t)
  (setq doom-modeline-env-enable-rust t)

  ;; Change the executables to use for the language version string
  (setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
  (setq doom-modeline-env-ruby-executable "ruby")
  (setq doom-modeline-env-perl-executable "perl")
  (setq doom-modeline-env-go-executable "go")
  (setq doom-modeline-env-elixir-executable "iex")
  (setq doom-modeline-env-rust-executable "rustc")

  ;; What to display as the version while a new one is being loaded
  (setq doom-modeline-env-load-string "...")

  ;; Hooks that run before/after the modeline version string is updated
  (setq doom-modeline-before-update-env-hook nil)
  (setq doom-modeline-after-update-env-hook nil))

;;(dracula-setup-modeline-format)

;;(cond
;; ((string-equal system-type "darwin")
;;  (progn
;;    (set-face-attribute 'mode-line nil :height 135 :family "IBM 3270")
;;    (set-face-attribute 'mode-line-inactive nil :height 135 :family "IBM 3270")))
;; ((string-equal system-type "gnu/linux")
;;  (progn
;;    (set-face-attribute 'mode-line nil :height 110 :family "IBM 3270")
;;    (set-face-attribute 'mode-line-inactive nil :height 110 :family "IBM 3270"))))

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
;; centered-window (The customized version)
;;
;; Based on https://github.com/anler/centered-window-mode
;;==============================================================================

(load-file "~/.emacs.d/centered-window.elc")
(require 'centered-window-mode)

(setq-default cwm-centered-window-width 180)

(add-hook 'dashboard-after-initialize-hook #'(lambda ()
                                               (centered-window-mode)))

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
;;  (cond
;;   ((string-equal system-type "darwin")
;;    (centaur-tabs-change-fonts "IBM 3270" 135))
;;   ((string-equal system-type "gnu/linux")
;;    (centaur-tabs-change-fonts "IBM 3270" 110)))
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)

  ;; Projectile integration
  (centaur-tabs-group-by-projectile-project)

  ;; A workaround to solve the problem of centaur-tabs bar disappearing in magit-status.
  (defun magit-status-on-centaur-tabs (&optional directory cache)
    "Run magit-status on centaur-tabs environment."
    (interactive)
    (magit-status directory cache)
    (call-interactively 'centaur-tabs-local-mode)
    (call-interactively 'centaur-tabs-local-mode))
  (defalias 'magit 'magit-status-on-centaur-tabs)

  ;;
  ;; Overriding 'centaur-tabs-buffer-tab-label' in 'centaur-tabs-functions.el'
  ;; Customize tabs label width with 'centaur-tabs-label-max-length'.
  ;;
  (setq centaur-tabs-label-max-length 30)
  (defun centaur-tabs-buffer-tab-label (tab)
    "Return a label for TAB.
That is, a string used to represent it on the tab bar."
    ;; Init tab style.
    ;; Render tab.
    (format " %s"
	        (let ((bufname (if centaur-tabs--buffer-show-groups
		                       (centaur-tabs-tab-tabset tab)
		                     (buffer-name (car tab)))))
              (if (> centaur-tabs-label-fixed-length 0)
                  (centaur-tabs-truncate-string centaur-tabs-label-fixed-length bufname)
                (if (and (> centaur-tabs-label-max-length 0) (< centaur-tabs-label-max-length (string-width bufname)))
                    (centaur-tabs-truncate-string centaur-tabs-label-max-length bufname)
                  bufname)))))

  ;;
  ;; Overriding 'centaur-tabs-buffer-groups' in 'centaur-tabs-functions.el'
  ;; Customize buffer groups.
  ;;
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
	 (cond
	  ((string-equal "*" (substring (buffer-name) 0 1))
	   "Emacs")
      ((string-prefix-p "*ein" (buffer-name))
       "EIN")
      ((or (string-match "magit-?[0-9a-zA-Z]*?: " (buffer-name))
           (memq major-mode '(magit-process-mode
			                  magit-status-mode
			                  magit-diff-mode
			                  magit-log-mode
			                  magit-file-mode
			                  magit-blob-mode
			                  magit-blame-mode)))
       "Magit")
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

  ;;
  ;; Overriding 'centaur-tabs-hide-tab' in 'centaur-tabs-functions.el'
  ;; Prevent the access to specified buffers.
  ;;
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
       (string-prefix-p "*Ibuffer" name)
       (string-prefix-p "*ein: LaTeX in Markdown preview*" name))))

  ;;
  ;; Overriding 'centaur-tabs-line-format' in 'centaur-tabs-functions.el'
  ;; To solve the miscalculation of visible tabs when using 'centered-window-mode'.
  ;;
  (defun centaur-tabs-line-format (tabset)
    "Return the `centaur-tabs-display-line-format' value to display TABSET."
    (let* ((sel (centaur-tabs-selected-tab tabset))
           (tabs (centaur-tabs-view tabset))
           (padcolor centaur-tabs-background-color)
           atsel elts)
      ;; Track the selected tab to ensure it is always visible.
      (when centaur-tabs--track-selected
        (while (not (memq sel tabs))
          (centaur-tabs-scroll tabset -1)
          (setq tabs (centaur-tabs-view tabset)))
        (while (and tabs (not atsel))
          (setq elts  (cons (centaur-tabs-line-tab (car tabs)) elts)
                atsel (eq (car tabs) sel)
                tabs  (cdr tabs)))
        (setq elts (nreverse elts))
        ;; At this point the selected tab is the last elt in ELTS.
        ;; Scroll TABSET and ELTS until the selected tab becomes
        ;; visible.
        (let (buffer-list-update-hook)
          (with-temp-buffer
            (set-window-margins (selected-window) 0 0) ;; added by daftcoder
            (let ((truncate-partial-width-windows nil)
                  (inhibit-modification-hooks t)
                  deactivate-mark ;; Prevent deactivation of the mark!
                  start)
              (setq truncate-lines nil
                    buffer-undo-list t)
              (setq start (point))
              (while (and (cdr elts) ;; Always show the selected tab!
                          (progn
                            (delete-region start (point-max))
                            (goto-char (point-max))
                            (apply #'insert elts)
                            (goto-char (point-min))
                            (> (vertical-motion 1) 0)))
                (centaur-tabs-scroll tabset 1)
                (setq elts (cdr elts))))))
        (setq elts (nreverse elts))
        (setq centaur-tabs--track-selected nil))
      ;; Format remaining tabs.
      (while tabs
        (setq elts (cons (centaur-tabs-line-tab (car tabs)) elts)
              tabs (cdr tabs)))
      ;; Cache and return the new tab bar.
      (centaur-tabs-set-template
       tabset
       (list
        (centaur-tabs-line-format--buttons)
        (nreverse elts)
        (propertize "% "
                    'face (list :background padcolor)
                    'pointer 'arrow)
        (centaur-tabs-line-format--new-button)))
      ))
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

(setq org-hide-emphasis-markers t) ;; Hide the markup for /italic/, *bold*, _underline_
(setq org-startup-with-inline-images t)
(setq org-image-actual-width nil)

;; Better Bullets
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Better Header Bullets
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(defun daftemacs/org-style ()
  (visual-line-mode t)
  (org-indent-mode t)

  (mapc
   (lambda (face)
     (set-face-attribute face nil :height 0.75))
   (list 'org-document-info-keyword
         'org-meta-line
         'org-block-begin-line
         'org-drawer
         'org-property-value))

  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.25))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.15))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.05))))))

(add-hook 'org-mode-hook 'daftemacs/org-style)

;; Change size of the inline image for LaTeX fragment in org-mode
;; https://tex.stackexchange.com/questions/78501/change-size-of-the-inline-image-for-latex-fragment-in-emacs-org-mode
(plist-put org-format-latex-options :scale 2)

;;==============================================================================
;; org-tree-slide
;;==============================================================================

(use-package org-tree-slide
  :ensure t
  :config
  (define-key org-mode-map (kbd "<f12>") 'org-tree-slide-mode)
  (define-key org-mode-map (kbd "S-<f12>") 'org-tree-slide-skip-done-toggle)
  (define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "<f10>") 'org-tree-slide-move-next-tree)
  (define-key org-tree-slide-mode-map (kbd "<f11>") 'org-tree-slide-content)

  (custom-set-faces
   '(org-tree-slide-heading-level-1-init ((t (:inherit outline-1 :height 1.25))))
   '(org-tree-slide-heading-level-2-init ((t (:inherit outline-2 :height 1.15))))
   '(org-tree-slide-heading-level-3-init ((t (:inherit outline-3 :height 1.1))))
   '(org-tree-slide-heading-level-4-init ((t (:inherit outline-4 :height 1.05)))))

  (defun daftemacs/presentation-start ()
    (setq org-tree-slide-skip-outline-level 4)
    (org-tree-slide-presentation-profile)
    (setq org-tree-slide-heading-emphasis t)
    (setq org-tree-slide-skip-done nil)
    (view-mode-enter)
    (text-scale-set 5)
    (set-window-margins (selected-window) 12 12)
    (hide-mode-line-mode)
    (centaur-tabs-local-mode nil))

  (defun daftemacs/presentation-end ()
    (View-exit-and-edit)
    (text-scale-set 0)
    (set-window-margins (selected-window) 0 0)
    (hide-mode-line-mode -1)
    (centaur-tabs-local-mode 0))

  (add-hook 'org-tree-slide-play-hook 'daftemacs/presentation-start)
  (add-hook 'org-tree-slide-stop-hook 'daftemacs/presentation-end))

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

;; http://company-mode.github.io/
(use-package company
  :ensure t
  :diminish company-mode
  :commands (company-complete company-mode)
  :bind
  (;([remap dabbrev-expand] . company-complete)
   ("C-." . company-complete)
   ("C->" . counsel-company)
   :map prog-mode-map
   ([tab] . company-indent-or-complete-common))
  :init
  (if (fboundp 'evil-declare-change-repeat)
      (mapc #'evil-declare-change-repeat
            '(company-complete-common
              company-select-next
              company-select-previous
              company-complete-selection
              company-complete-number)))
  (setq company-require-match nil            ; Don't require match, so you can still move your cursor as expected.
        company-tooltip-align-annotations t  ; Align annotation to the right side.
        company-eclim-auto-save nil          ; Stop eclim auto save.
        company-dabbrev-downcase nil         ; No downcase when completion.
        company-idle-delay 1
        company-show-numbers "on")
  (add-hook 'after-init-hook 'global-company-mode))

;; https://github.com/jcs-elpa/company-fuzzy
(use-package company-fuzzy
  :ensure t
  :init
  (with-eval-after-load 'company
    (setq company-fuzzy-sorting-backend 'alphabetic)
    (setq company-fuzzy-show-annotation t)
    (global-company-fuzzy-mode 1)))

;; https://github.com/company-mode/company-statistics
(use-package company-statistics
  :ensure t
  :hook (company-mode . company-statistics-mode))

;; https://github.com/sebastiencs/company-box
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(with-eval-after-load 'company
  (message "company-backends: %s" company-backends)
  (message "company-fuzzy--backends: %s" company-fuzzy--backends))

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
  :after ivy
  :ensure t
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
;;  :after ivy
;;  :ensure t
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
;; spell-fu
;;
;; https://gitlab.com/ideasman42/emacs-spell-fu
;;==============================================================================

(use-package spell-fu
  :ensure t
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (setq ispell-personal-dictionary "~/.emacs.d/spell-fu/personal_dictionary.txt")
              (setq spell-fu-faces-exclude '(org-meta-line org-link org-code))
              (spell-fu-mode))))

;;==============================================================================
;; treemacs
;;
;; https://github.com/Alexander-Miller/treemacs
;;==============================================================================

(use-package treemacs
  :ensure t
  :defer t
  ;;:init
  ;;(with-eval-after-load 'winum
  ;;  (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
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
     (treemacs-git-mode 'simple)))

  (defun treemacs-hide()
    "Close the Treemacs window."
    (interactive)
    (let ((treemacs-local-window (treemacs-get-local-window)))
      (if treemacs-local-window
          (delete-window treemacs-local-window))))
  :bind
  (:map global-map
        ("C-c 0"     . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

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
;;
;; https://github.com/jaypei/emacs-neotree
;;==============================================================================

(use-package neotree
  :ensure t
  :config
  (if (and (fboundp 'doom-themes-neotree-config)
           nil) ;; Currently not used because doom-themes-neotree-config doesn't support neo-vc-integration.
      (progn
        ;; Enable custom neotree theme (all-the-icons must be installed!)
        (doom-themes-neotree-config)
        (setq doom-themes-neotree-file-icons t))
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

  (setq neo-window-width 40
        neo-window-fixed-size t
        neo-show-hidden-files t
        neo-smart-open t
        neo-vc-integration (quote (face char)) ;; https://github.com/jaypei/emacs-neotree/issues/166
        projectile-switch-project-action 'neotree-projectile-action) ;; To open neotree when projectile project is opend.

  (when (boundp 'aw-ignored-buffers)
    (push neo-buffer-name aw-ignored-buffers))

  (defun neotree-show-project-root-dir ()
    "Show NeoTree using the project root using projectile."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-show)
      (when project-dir
        (neotree-dir project-dir))
      (when file-name
        (neotree-find file-name))))

  (defun neotree-toggle-project-root-dir-or-current-dir ()
    "Open NeoTree using the project root, using projectile, or the current buffer directory."
    (interactive)
    (let ((project-dir (ignore-errors (projectile-project-root)))
          (file-name (buffer-file-name)))
      (if (neo-global--window-exists-p)
          (neotree-hide)
        (progn
          (neotree-show)
          (when project-dir
            (neotree-dir project-dir))
          (when file-name
            (neotree-find file-name))))))

  (defun neotree-show-project-root-dir-or-find-file-in-project-dir-or-current-dir ()
    "Show NeoTree using the project root, using projectile, find-file-in-project or the current buffer directory."
    (interactive)
    (let* ((filepath (buffer-file-name))
           (project-dir
            (with-demoted-errors
                (cond
                 ((featurep 'projectile)
                  (projectile-project-root))
                 ((featurep 'find-file-in-project)
                  (ffip-project-root))
                 (t ;; Fall back to version control root.
                  (if filepath
                      (vc-call-backend
                       (vc-responsible-backend filepath) 'root filepath)
                    nil)))))
           (neo-smart-open t))
      (neotree-show)
      (when project-dir
        (neotree-dir project-dir))
      (when filepath
        (neotree-find filepath))))

  (defun neotree-toggle-project-root-dir-or-find-file-in-project-dir-or-current-dir ()
    "Open NeoTree using the project root, using projectile, find-file-in-project or the current buffer directory."
    (interactive)
    (let* ((filepath (buffer-file-name))
           (project-dir
            (with-demoted-errors
                (cond
                 ((featurep 'projectile)
                  (projectile-project-root))
                 ((featurep 'find-file-in-project)
                  (ffip-project-root))
                 (t ;; Fall back to version control root.
                  (if filepath
                      (vc-call-backend
                       (vc-responsible-backend filepath) 'root filepath)
                    nil)))))
           (neo-smart-open t))
      (if (neo-global--window-exists-p)
          (neotree-hide)
        (progn
          (neotree-show)
          (when project-dir
            (neotree-dir project-dir))
          (when filepath
            (neotree-find filepath))))))

  (defun neotree-select-window ()
    "Select the neotree window if it is visible."
    (interactive)
    (let ((neotree-buffer (get-buffer neo-buffer-name)))
      (if neotree-buffer
          (let ((p (with-current-buffer neotree-buffer (point))))
            (neotree-show-project-root-dir-or-find-file-in-project-dir-or-current-dir)
            (goto-char p))
        (neotree-show-project-root-dir-or-find-file-in-project-dir-or-current-dir)))))

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
  (setq persp-state-default-file-loaded nil)

  (message "persp-state-default-file: %s (%s)" persp-state-default-file (file-exists-p persp-state-default-file))

  (defun persp-state-load-default ()
    "Restore the perspective state saved in the default file."
    (interactive)
    (when (file-exists-p persp-state-default-file)
      (persp-state-load persp-state-default-file)
      (setq persp-state-default-file-loaded t)))

  ;;(add-hook 'dashboard-after-initialize-hook #'(lambda ()
  ;;                                               (persp-state-load-default)))
  (add-hook 'persp-before-switch-hook #'(lambda ()
                                          (treemacs-hide)
                                          (neotree-hide)))
  (add-hook 'kill-emacs-hook #'(lambda ()
                                 (treemacs-hide)
                                 (neotree-hide)
                                 (when (or (not (file-exists-p persp-state-default-file)) persp-state-default-file-loaded)
                                   (persp-state-save))))

  ;;
  ;; Overriding 'centaur-tabs-buffer-list' in 'centaur-tabs-functions.el'
  ;; For smooth work with 'perspective', '(buffer-list)' is replaced with '(persp-buffer-list-filter (buffer-list))'.
  ;;
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
                      (persp-buffer-list-filter (buffer-list)))))) ;; modified by daftcoder

  ;;
  ;; Overriding 'ibuffer-update'
  ;;
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
    (let* ((bufs (persp-buffer-list-filter (buffer-list)))
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
;; pretty-hydra
;;==============================================================================

(use-package pretty-hydra
  :ensure t)

;;==============================================================================
;; Bufler
;;
;; https://github.com/alphapapa/bufler.el
;;==============================================================================

(use-package bufler
  :load-path "bufler"
  :config
  (setq bufler-use-header-line-format nil
        bufler-delete-bufler-window-when-switching-to-buffer nil)
  (setq bufler-filter-buffer-modes
        '(bufler-list-mode special-mode timer-list-mode)))

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

;; objc-font-lock
(use-package objc-font-lock
  :ensure t)

;;==============================================================================
;; Swift
;;
;; https://github.com/swift-emacs/swift-mode
;;==============================================================================

(use-package swift-mode
  :ensure t)

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
;; typescript-mode
;;
;; https://github.com/emacs-typescript/typescript.el
;;==============================================================================

(use-package typescript-mode
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
;; Docker
;;==============================================================================

;; https://github.com/Silex/docker.el
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;; https://github.com/emacs-pe/docker-tramp.el
(use-package docker-tramp
  :ensure t)

;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

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
                                      ;;(hide-mode-line-mode)
                                      ;;(pdf-view-midnight-minor-mode)
                                      (pdf-view-fit-page-to-window))))

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

(defun enlarge-margin-horizontally (delta)
  (let ((wm (window-margins (selected-window))))
    (if (not (car wm))
        (set-window-margins (selected-window) delta delta)
      (set-window-margins (selected-window) (+ (cdr wm) delta) (+ (cdr wm) delta))))
  ;; The workaround for the problem that the window's margin is not changed immediately when 'centaur-tabs-local-mode' is turned on.
  (if centaur-tabs-local-mode
      (window-resize nil 0 t)))

(defun shrink-margin-horizontally (delta)
  (let ((wm (window-margins (selected-window))))
    (if (car wm)
        (let ((nlm (- (cdr wm) delta))
              (nrm (- (cdr wm) delta)))
          (set-window-margins (selected-window) (if (< nlm 0) 0 nlm) (if (< nrm 0) 0 nrm)))))
  ;; The workaround for the problem that the window's margin is not changed immediately when 'centaur-tabs-local-mode' is turned on.
  (if centaur-tabs-local-mode
      (window-resize nil 0 t)))

(defun resize-window (&optional arg)
  "*Resize window interactively."
  (interactive "p")
  (setq arg 4)
  (if (one-window-p)
      (let (c)
        (catch 'done
          (while t
            (message
             "+=widen margin, -=narrow margin, 1-9=unit(%d), q=quit"
             arg)
            (setq c (read-char))
            (condition-case ()
                (cond
                 ((= c ?+) (enlarge-margin-horizontally arg))
                 ((= c ?-) (shrink-margin-horizontally arg))
                 ((= c ?\^G) (keyboard-quit))
                 ((= c ?q) (throw 'done t))
                 ((and (> c ?0) (<= c ?9)) (setq arg (- c ?0)))
                 (t (beep)))
              (error (beep))))))
    (let (c)
      (catch 'done
        (while t
          (message
           "h=heighten, s=shrink, w=widen, n=narrow, b=balance, +=widen margin, -=narrow margin, 1-9=unit(%d), q=quit"
           arg)
          (setq c (read-char))
          (condition-case ()
              (cond
               ((= c ?h) (enlarge-window arg))
               ((= c ?s) (shrink-window arg))
               ((= c ?w) (enlarge-window-horizontally arg))
               ((= c ?n) (shrink-window-horizontally arg))
               ((= c ?+) (enlarge-margin-horizontally arg))
               ((= c ?-) (shrink-margin-horizontally arg))
               ((= c ?b) (balance-windows))
               ((= c ?\^G) (keyboard-quit))
               ((= c ?q) (throw 'done t))
               ((and (> c ?0) (<= c ?9)) (setq arg (- c ?0)))
               (t (beep)))
            (error (beep)))))))
  (message "Done."))

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
  ;;(if (version< emacs-version "26")
  ;;    (set (make-variable-buffer-local 'linum-mode) nil)
  ;;  (set (make-variable-buffer-local 'display-line-numbers) nil))
  (set (make-variable-buffer-local 'global-hl-line-mode) nil))

(defun my-find-file-check-if-very-large-file-hook ()
  "If a file is over 2MB, turn off modes of the buffer that make it slow."
  (when (> (buffer-size) (* 2 1024 1024))
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
;; Setting default web browser
;;
;; http://ergoemacs.org/emacs/emacs_set_default_browser.html
;;==============================================================================

(when (string-equal system-type "gnu/linux")
  (defun browse-url-surf (url &optional new-window)
    "Ask the Surf web browser to load URL."
    (interactive (browse-url-interactive-arg "URL: "))
    (setq url (browse-url-encode-url url))
    (let* ((process-environment (browse-url-process-environment)))
      (apply #'start-process
             (concat "surf " url) nil
             "surf"
             (list url))))

  (setq browse-url-browser-function 'browse-url-surf))

;;==============================================================================
;; Instant Stackoverflow Solutions
;;==============================================================================

(load-file "~/.emacs.d/stackoverflow.elc")
(require 'stackoverflow)

;;==============================================================================
;; Embedding YouTube videos with org-mode links
;;
;; Based on http://endlessparentheses.com/embedding-youtube-videos-with-org-mode-links.html
;;
;; - Examples
;;   [[YouTube:KJritAFWnUM]]
;;   [[YouTube:KJritAFWnUM/640x360]]
;;==============================================================================

(defun org-youtube-link-parse-path (path default-width default-height)
  (if (string-match "/" path)
      (let* ((split-path (split-string path "/"))
             (real-path (car split-path))
             (resolution (car (cdr split-path))))
        (if (string-match "x" resolution)
            (let ((split-resolution (split-string resolution "x")))
              (if (and split-resolution (= (length split-resolution) 2))
                  (let ((w (string-to-number (car split-resolution)))
                        (h (string-to-number (car (cdr split-resolution)))))
                    (if (and (> w 0) (> h 0))
                        (values real-path w h)
                      (values real-path default-width default-height)))
                (values real-path default-width default-height)))
          (values real-path default-width default-height)))
    (values path default-width default-height)))

(defvar org-youtube-link-iframe-format
  (concat "<iframe width=\"%d\""
          " height=\"%d\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe><br>"))

(org-add-link-type
 "YouTube"
 (lambda (handle)
   (let ((parsed-path (org-youtube-link-parse-path handle 640 360)))
     (browse-url
      (concat "https://www.youtube.com/embed/"
              (car parsed-path)))))
 (lambda (path desc backend)
   (let ((parsed-path (org-youtube-link-parse-path path 640 360)))
     (cl-case backend
       (html
        (format org-youtube-link-iframe-format
                (nth 1 parsed-path)
                (nth 2 parsed-path)
                (nth 0 parsed-path)
                (or desc "")))
       (latex
        (format "\href{%s}{%s}"
                (nth 1 parsed-path)
                (nth 2 parsed-path)
                (nth 0 parsed-path)
                (or desc "video")))))))

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
(global-set-key (kbd "M-SPC") 'toggle-input-method) ;; 'Shift + Space' is recognized as 'M-SPC' on MacOS.

(global-set-key (kbd "C-s-<up>") 'buf-move-up)
(global-set-key (kbd "C-s-<down>") 'buf-move-down)
(global-set-key (kbd "C-s-<left>") 'buf-move-left)
(global-set-key (kbd "C-s-<right>") 'buf-move-right)

(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "M-O") 'ace-swap-window)
(global-set-key (kbd "M-s-o") 'ff-find-other-file)
(global-set-key (kbd "M-m") 'lsp-ui-imenu)
(global-set-key (kbd "M-M") 'imenu-list)
(global-set-key (kbd "M-0") #'(lambda ()
                                (interactive)
                                (if (neo-global--window-exists-p)
                                    (neotree-select-window)
                                  (pcase (treemacs-current-visibility)
                                    ('visible (treemacs--select-visible-window))
                                    (code (neotree-select-window))))))

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x C-b") 'bufler)
(global-set-key (kbd "C-x C-S-b") 'persp-ibuffer)
(global-set-key (kbd "C-x b") 'persp-counsel-switch-buffer)
(global-set-key (kbd "C-x k") 'persp-kill-buffer*)

(global-set-key (kbd "C-x x l") 'persp-state-load-default)

(global-set-key (kbd "C-c s") 'swiper)

(global-set-key (kbd "C-c w c") 'centered-window-mode)
(global-set-key (kbd "C-c w z") 'resize-window)

(global-set-key (kbd "C-c c r") 'counsel-register)
(global-set-key (kbd "C-c c e") 'counsel-recentf)
(global-set-key (kbd "C-c c f") 'counsel-projectile-find-file)
(global-set-key (kbd "C-c c d") 'counsel-projectile-find-dir)
(global-set-key (kbd "C-c c b") 'counsel-projectile-switch-to-buffer)
(global-set-key (kbd "C-c c g") 'counsel-projectile-rg)
(global-set-key (kbd "C-c c p") 'counsel-projectile-switch-project)

(global-set-key (kbd "C-c C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-c C-r") 'isearch-backward-regexp)

(global-set-key (kbd "s-_") 'whitespace-mode)

(global-set-key (kbd "C-c m") 'magit-status-on-centaur-tabs)
(global-set-key (kbd "C-c 1") 'eshell)
(global-set-key (kbd "C-c 2") #'(lambda ()
                                  (interactive)
                                  (ansi-term explicit-shell-file-name)))
(global-set-key (kbd "C-c 3") #'(lambda ()
                                  (interactive)
                                  (calculator)
                                  (balance-windows)))
(global-set-key (kbd "C-c 4") 'neato-graph-bar)
(global-set-key (kbd "C-c 9") 'neotree-select-window)

(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)

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
