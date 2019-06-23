;; Editor: Richard Jaeho Hur

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

(setq gc-cons-threshold (* 100 1024 1024))
(message "gc-cons-threshold: %d" gc-cons-threshold)

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
;; My Variables
;;==============================================================================

(defvar my-initial-buffer nil)
;(defvar my-initial-buffer (concat user-emacs-directory "/init.el"))
(defvar my-default-directory (concat (getenv "HOME") "/Workspace/"))

;;==============================================================================
;; Basic Customization
;;==============================================================================

(defun open-user-init-file ()
  (interactive)
  (find-file-read-only user-init-file))

(defun open-zshrc-file ()
  (interactive)
  (find-file-read-only "~/.zshrc"))

(defun open-i3-config-file ()
  (interactive)
  (find-file-read-only "~/.config/i3/config"))

(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

(defun init-doom-theme ()
  (require 'doom-themes)

  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each
  ;; theme may have their own settings.
  ;(load-theme 'doom-one t)
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

(defun init-zerodark-theme ()
  (load-theme 'zerodark t)
  ;; Optionally setup the modeline
  (zerodark-setup-modeline-format))

(defun init-color-theme ()
  (add-to-list 'load-path "~/.emacs.d/emacs-goodies-el/")
  (require 'color-theme)
  (load-file "~/.emacs.d/color-theme/color-theme-sunburst.el")
  ;(load-file "~/.emacs.d/color-theme/color-theme-tangotango.el")
  (eval-after-load "color-theme"
    '(progn
       (color-theme-initialize)
       (color-theme-sunburst))))

(defun init-themes ()
  ;(load-theme 'base16-default-dark t)
  ;(init-doom-theme)
  ;(init-zerodark-theme)
  ;(init-color-theme)
  (load-theme 'dracula t) ; https://draculatheme.com/emacs/
  )

(defun init-doom-mode-line ()
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

(defun init-mode-line ()
  ;(init-doom-mode-line)

  (setq sml/theme 'respectful)
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  )

;** Font Repositories **
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

(defun startup-on-gui ()
  (init-font)

  (tool-bar-mode -1) ; hide tool bar
  ;(menu-bar-mode -1) ; hide menu bar

  (set-frame-position (selected-frame) 0 0)
  ;(set-frame-width (selected-frame) 150)
  ;(set-frame-height (selected-frame) 100)

  (setq window-min-width (/ (display-pixel-height) 22))

  (require 'all-the-icons)

  (defun toggle-fullscreen (&optional f)
    (interactive)
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

  (setq desktop-save-mode t))

(defun startup-on-cui ()
  (setq window-min-width (/ (display-pixel-width) 5)))

;;==============================================================================
;; Startup
;;==============================================================================

(setq visible-bell t
      make-backup-files nil
      column-number-mode t
      split-height-threshold nil ; not to split this way.
      gdb-many-windows t)
(blink-cursor-mode 0)
(global-hl-line-mode)
(setq-default truncate-lines t)
(put 'erase-buffer 'disabled nil)
(show-paren-mode t)
(delete-selection-mode t)
(require 'cl) ; Common Lisp

(init-themes)
(init-mode-line)

(if (display-graphic-p)
    (startup-on-gui)
  (startup-on-cui))

(switch-to-buffer "*Messages*")

(setq default-directory my-default-directory) ; this line must be excuted after excuting '(switch-to-buffer "*Messages*")'.
(message "default-directory: %s\n" default-directory)

;;==============================================================================
;; Custom Set Variables
;;==============================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "bce3ae31774e626dce97ed6d7781b4c147c990e48a35baedf67e185ebc544a56" default)))
 '(inhibit-startup-screen t)
 '(initial-buffer-choice my-initial-buffer)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (counsel magit-gitflow keyfreq neato-graph-bar importmagic pip-requirements py-autopep8 elpy json-reformat yasnippet elogcat rg deadgrep ripgrep helm-rg ag helm-ag dumb-jump focus smart-mode-line google-c-style dracula-theme ccls company-lsp lsp-ui lsp-mode flycheck treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs pdf-tools helm-gtags imenu-list objc-font-lock neotree zerodark-theme company magit vlf base16-theme flx-isearch flx-ido flx projectile dark-souls haskell-mode))))
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
;; Hangul Input Method
;;==============================================================================

(setq default-input-method "korean-hangul")
(global-set-key (kbd "S-SPC") 'toggle-input-method)

;;==============================================================================
;; exec-path
;;==============================================================================

(cond
 ((string-equal system-type "darwin")
  (progn
    (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
    (setq exec-path (append exec-path '("/usr/local/bin")))
    (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
    (setq exec-path (append exec-path '("/Library/TeX/texbin")))
    (setenv "PATH" (concat (concat (getenv "PATH") ":") (expand-file-name "~/Library/Android/sdk/platform-tools")))
    (setq exec-path (append exec-path '((expand-file-name "~/Library/Android/sdk/platform-tools"))))
    (setenv "PATH" (concat (concat (getenv "PATH") ":") (expand-file-name "~/Workspace/trtc/depot_tools")))
    (setq exec-path (append exec-path '((expand-file-name "~/Workspace/trtc/depot_tools"))))))
 ((string-equal system-type "gnu/linux")
  (progn
    (setenv "PATH" (concat (concat (getenv "PATH") ":") (expand-file-name "~/Workspace/depot_tools")))
    (setq exec-path (append exec-path '((expand-file-name "~/Workspace/depot_tools")))))))

;;==============================================================================
;; ivy
;;==============================================================================

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;;==============================================================================
;; projectile
;;==============================================================================

(projectile-mode +1)
(setq projectile-enable-caching t)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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
;; vlf
;;
;; https://github.com/m00natic/vlfi
;;==============================================================================

(require 'vlf-setup)

;;==============================================================================
;; find-file-hook for handling the very large file
;;
;; https://stackoverflow.com/questions/18316665/
;; how-to-improve-emacs-performance-when-view-large-file
;;==============================================================================

(defun my-find-file-check-if-very-large-file-hook ()
  "If a file is over 5MB, turn off modes of the buffer that make it slow."
  (when (> (buffer-size) (* 5 1024 1024))
    ;(setq buffer-read-only t)
    (setq bidi-display-reordering nil)
    (buffer-disable-undo)
    (jit-lock-mode nil)
    (set (make-variable-buffer-local 'font-lock-mode) nil)
    (set (make-variable-buffer-local 'linum-mode) nil)
    (set (make-variable-buffer-local 'global-hl-line-mode) nil)))
(add-hook 'find-file-hook 'my-find-file-check-if-very-large-file-hook)

;;;==============================================================================
;;; flycheck
;;;==============================================================================
;
;(add-hook 'after-init-hook #'global-flycheck-mode)

;;==============================================================================
;; company
;;==============================================================================

(add-hook 'after-init-hook 'global-company-mode)

;;==============================================================================
;; neotree
;;==============================================================================

(require 'neotree)
(setq neo-window-width window-min-width)
;(setq neo-window-position 'right)
(setq neo-window-fixed-size nil)
(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action) ;; To open neotree when projectile project is opend.

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
      (if project-dir
          (neotree-dir project-dir))
      (if file-name
          (neotree-find file-name)))))

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
    (setq treemacs-collapse-dirs                 (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
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
;; pdf-tools
;;==============================================================================

(pdf-tools-install)

;;==============================================================================
;; ediff
;;
;; https://oremacs.com/2015/01/17/setting-up-ediff/
;;==============================================================================

(csetq ediff-window-setup-function 'ediff-setup-windows-plain)
(csetq ediff-split-window-function 'split-window-horizontally)
(csetq ediff-diff-options "-w")
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;;==============================================================================
;; Undo Tree
;;
;; https://www.emacswiki.org/emacs/UndoTree
;;==============================================================================

(global-undo-tree-mode)

;;==============================================================================
;; linum
;;==============================================================================

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
    (linum-mode 1)))

;;==============================================================================
;; buffer-move
;;==============================================================================

(load-file "~/.emacs.d/buffer-move.elc")

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

;;;==============================================================================
;;; gtags (OBSOLETE)
;;;==============================================================================
;
;;; Enable helm-gtags-mode
;(add-hook 'c-mode-hook 'helm-gtags-mode)
;(add-hook 'c++-mode-hook 'helm-gtags-mode)
;(add-hook 'asm-mode-hook 'helm-gtags-mode)
;
;;; Set key bindings
;(eval-after-load "helm-gtags"
;  '(progn
;     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
;     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
;     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
;     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
;     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
;     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))

;;;==============================================================================
;;; etags (OBSOLETE)
;;;==============================================================================
;
;(defun create-tags (dir-name)
;  "Create a TAGS file."
;  (interactive "DDirectory: ")
;  (setq c++-headers-path "/usr/include/c++")
;  (with-current-buffer (get-buffer-create "*etags-output*") (erase-buffer))
;  (execute-commands "*etags-output*"
;                    (format "find -H %s -name \"*\" | xargs etags -o %sTAGS" c++-headers-path dir-name)
;                    (format "find -H %s -type f \\( \
;-name \"*.[csSh]\" \
;-o \
;-name \"*.cc\" \
;-o \
;-name \"*.cpp\"\
;-o \
;-name \"*.m\" \
;-o \
;-name \"*.java\" \
;-o \
;-name \"*.py\" \
;-o \
;-name \"*.pl\" \
;\\) | \
;xargs etags -a -o %sTAGS" dir-name dir-name)))

;;;==============================================================================
;;; find-file-in-tags (OBSOLETE)
;;;==============================================================================
;
;(load-file "~/.emacs.d/find-file-in-tags.el")

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
;; alternative
;(load-file "~/.emacs.d/smooth-scroll.el")
;(require 'smooth-scroll)
;(smooth-scroll-mode t)

;;==============================================================================
;; magit
;;==============================================================================

(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
(setq magit-gitflow-popup-key "~")

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
;; Code Style
;;==============================================================================

(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
;(add-hook 'c-mode-hook
;          (lambda()
;            (setq c-basic-offset 2)
;            (c-set-offset 'substatement-open 0)))
;(add-hook 'c++-mode-hook
;          (lambda()
;            (setq c-basic-offset 2)
;            (c-set-offset 'substatement-open 0)))

(add-hook 'java-mode-hook
          (lambda()
            (setq c-basic-offset 2)
            (c-set-offset 'substatement-open 0)))

(add-hook 'js-mode-hook
          (lambda()
            (setq js-indent-level 2)))

;;;==============================================================================
;;; Dim for #if 0 ... #endif
;;;==============================================================================
;
;(defun cpp-highlight-if-0/1 ()
;  "Modify the face of text in between #if 0 ... #endif."
;  (setq cpp-known-face '(background-color . "gray15"))
;  (setq cpp-unknown-face 'default)
;  (setq cpp-face-type 'dark)
;  (setq cpp-known-writable 't)
;  (setq cpp-unknown-writable 't)
;  (setq cpp-edit-list
;        '((#("1" 0 1
;             (fontified nil))
;           nil
;           (background-color . "gray15")
;           both nil)
;          (#("0" 0 1
;             (fontified nil))
;           (background-color . "gray15")
;           nil
;           both nil)))
;  (cpp-highlight-buffer t))
;(defun jpk/c-mode-hook ()
;  (cpp-highlight-if-0/1)
;  (add-hook 'after-save-hook 'cpp-highlight-if-0/1 'append 'local))
;(add-hook 'c-mode-common-hook 'jpk/c-mode-hook)

;;==============================================================================
;; Objective C
;; https://www.emacswiki.org/emacs/ObjectiveCMode
;;==============================================================================

(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

(add-to-list 'magic-mode-alist
             `(,(lambda ()
                  (and (string= (file-name-extension buffer-file-name) "h")
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

(use-package lsp-mode :commands lsp)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)

(setq lsp-prefer-flymake nil)

;;
;; - Building ccls
;; $ git clone --depth=1 --recursive https://github.com/MaskRay/ccls
;; $ cd ccls
;; On Ubuntu
;; $ cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=/usr/lib/llvm-7
;; On MacOS brew llvm
;; $ cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=/usr/local/opt/llvm
;; $ cmake --build Release
;;
;; - Installing ccls on MacOS brew (https://github.com/twlz0ne/homebrew-ccls)
;; $ brew tap twlz0ne/homebrew-ccls
;; $ brew install ccls
;;
(use-package ccls
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))
(setq ccls-executable "~/.emacs.d/ccls/Release/ccls")
;TODO
;(setq
; ccls-initialization-options
; `(:index (:multiVersion 1 :trackDependency 1)))

(add-to-list 'projectile-globally-ignored-directories ".ccls-cache")

;;==============================================================================
;; yasnippet
;;==============================================================================

(require 'yasnippet)
(yas-global-mode 1)

;;==============================================================================
;; Python
;;
;; elpy (https://github.com/jorgenschaefer/elpy)
;; # Completion and code navigation
;; pip3 install jedi
;; # Code checks
;; pip3 install flake8
;; # Refactoring
;; pip3 install rope
;; # Automatic formatting (PEP8, Yapf or Black)
;; pip3 install autopep8
;; pip3 install yapf
;; pip3 install black (only available on Python 3)
;;
;; importmagic (https://github.com/anachronic/importmagic.el)
;; pip3 install importmagic epc
;;==============================================================================

(setq python-shell-interpreter "python3"
      python-shell-completion-native-enable nil)

(use-package python
  :mode ("\\.py" . python-mode)
  :config
  (use-package elpy
    :init
    (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
    :config
    (setq elpy-rpc-python-command "python3")
    (setq elpy-rpc-backend "jedi")
    (defun python-send-buffer-with-args (args)
      (interactive "sPython arguments: ")
      (let ((source-buffer (current-buffer))
            (current-buffer-name (buffer-name)))
        (with-temp-buffer
          (insert "import sys; sys.argv = '''" current-buffer-name " " args "'''.split()\n")
          (insert-buffer-substring source-buffer)
          (elpy-shell-send-buffer))))
    (define-key elpy-mode-map (kbd "C-c C-a") 'python-send-buffer-with-args)
    :bind (:map elpy-mode-map
                ("M-." . elpy-goto-definition)
                ("M-," . pop-tag-mark)))
  (elpy-enable))

(use-package pip-requirements
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

(use-package py-autopep8
  :config
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

(use-package importmagic
  :ensure t
  :config
  (add-hook 'python-mode-hook 'importmagic-mode))

;;==============================================================================
;; Key Mapping Customiaztion
;;==============================================================================

(defun my-shell-hook ()
  (local-set-key "\C-cl" 'erase-buffer))
(add-hook 'shell-mode-hook 'my-shell-hook)

(defun hexl-mode-toggle ()
  (interactive)
  (if (string= "hexl-mode" major-mode)
      (hexl-mode-exit)
    (hexl-mode)))

(defun other-window-reverse ()
  (interactive)
  (other-window -1))


(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x p") 'other-window-reverse)

(global-set-key (kbd "C-c /") 'undo-tree-visualize)
(global-set-key (kbd "C-c d") 'desktop-read)
(global-set-key (kbd "C-c \\") 'neotree-toggle-project-root-dir-or-current-dir)
(global-set-key (kbd "C-c |") 'neotree-show-project-root-dir)
;(global-set-key (kbd "C-c f") 'find-file-in-tags) ;; OBSOLETE
(global-set-key (kbd "C-c r") 'resize-window)
(global-set-key (kbd "C-c k") 'erase-buffer)

(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c w") 'xwidget-webkit-browse-url)

(global-set-key (kbd "C-c 1") 'eshell)
;(global-set-key (kbd "C-c 1") 'shell)
;(global-set-key (kbd "C-c 1") 'term)
(global-set-key (kbd "C-c 2") 'neato-graph-bar)

(global-set-key (kbd "C-c _") 'whitespace-mode)
(global-set-key (kbd "C-c =") 'hexl-mode-toggle)

(global-set-key (kbd "C-c s") 'swiper)
(global-set-key (kbd "C-c C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-c C-r") 'isearch-backward-regexp)

(global-set-key (kbd "C-c <up>") 'buf-move-up)
(global-set-key (kbd "C-c <down>") 'buf-move-down)
(global-set-key (kbd "C-c <left>") 'buf-move-left)
(global-set-key (kbd "C-c <right>") 'buf-move-right)

(global-set-key (kbd "M-o") 'projectile-find-other-file)
;(global-set-key (kbd "M-o") 'ff-find-other-file) ;; OBSOLETE
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
