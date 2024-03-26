;; Editor: Richard Jaeho Hur

;; Dracula color codes
;;----------------------------------------------------------
;; Background   | #282a36 | hsl(231,15%,18%)  | 40 42 54
;; Current Line | #44475a | hsl(232,14%,31%)  | 68 71 90
;; Foreground   | #f8f8f2 | hsl(60,30%,96%)   | 248 248 242
;; Comment      | #6272a4 | hsl(225,27%,51%)  | 98 114 164
;; Cyan         | #8be9fd | hsl(191,97%,77%)  | 139 233 253
;; Green        | #50fa7b | hsl(135,94%,65%)  | 80 250 123
;; Orange       | #ffb86c | hsl(31,100%,71%)  | 255 184 108
;; Pink         | #ff79c6 | hsl(326,100%,74%) | 255 121 198
;; Purple       | #bd93f9 | hsl(265,89%,78%)  | 189 147 249
;; Red          | #ff5555 | hsl(0,100%,67%)   | 255 85 85
;; Yellow       | #f1fa8c | hsl(65,92%,76%)   | 241 250 140
;;----------------------------------------------------------
;; https://draculatheme.com/contribute

;; My favorite special characters: 🔥 🚀 ⚡ 🌙 🪐 ⭐ 👍 🦊 🐳 🌈 🍭
;; https://unicode-table.com/en/sets/top-emoji/

(setq with-pgtk (string-equal window-system "pgtk"))

(defun daftemacs-display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'daftemacs-display-startup-time)

;;==============================================================================
;; Setup Frame and Font
;;==============================================================================

(defun setup-frame ()
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-frame-position (selected-frame) 0 0))

(defun setup-font ()
  ;; JetBrainsMono (https://www.jetbrains.com/lp/mono/, https://fonts.google.com/specimen/JetBrains+Mono)
  ;; Source Code Pro (https://github.com/adobe-fonts/source-code-pro)
  ;; Office Code Pro (https://github.com/nathco/Office-Code-Pro)
  ;; Menlo (https://github.com/hbin/top-programming-fonts
  ;; IBM 3270 (https://github.com/rbanffy/3270font)
  ;; Hack (https://github.com/source-foundry/Hack)
  ;; D2Coding (https://github.com/naver/d2codingfont)
  ;; NanumGothicCoding (https://github.com/naver/nanumfont/blob/master/README.md)
  ;;(print (font-family-list))
  (cond
   ((string-equal system-type "gnu/linux") ;; Font path: ~/.local/share/fonts
    (progn
      (if with-pgtk
          (set-face-attribute 'default nil :width 'normal :height 94 :family "JetBrains Mono")
        (set-face-attribute 'default nil :width 'normal :height 95 :family "JetBrains Mono"))
      ;;(if with-pgtk
      ;;    (set-face-attribute 'default nil :height 94 :family "Source Code Pro")
      ;;  (set-face-attribute 'default nil :height 95 :family "Source Code Pro"))
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
      ))
   ((string-equal system-type "darwin") ;; Font path: ~/Library/Fonts
    (progn
      (set-face-attribute 'default nil :width 'normal :height 120 :family "JetBrains Mono")
      ;;(set-face-attribute 'default nil :height 120 :family "Source Code Pro")
      ;;(set-face-attribute 'default nil :height 120 :family "Office Code Pro")
      ;;(set-face-attribute 'default nil :height 120 :family "Office Code Pro D")
      ;;(set-face-attribute 'default nil :height 115 :family "Menlo")
      ;;(set-face-attribute 'default nil :height 115 :family "Hack")
      ;;(set-face-attribute 'default nil :height 115 :family "FiraCode")
      ;;(set-face-attribute 'default nil :height 115 :family "monospace")
      ;;(set-face-attribute 'default nil :height 115 :family "D2Coding")
      ;;(set-face-attribute 'default nil :height 125 :family "IBM 3270")
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
(message "Version: %s" (replace-regexp-in-string "\n" "" (emacs-version)))
(message "Repository: %s %s" emacs-repository-version emacs-repository-branch)

(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (message "Native compilation: Enabled")
  (message "Native compilation: Disabled"))

(if with-pgtk
    (message "PGTK: Enabled")
  (message "PGTK: Disabled"))

(if (functionp 'json-serialize)
    (message "Native JSON: Enabled")
  (message "Native JSON: Disabled"))

(message "system-type: %s" system-type)
(message "system-name: %s" system-name)
(message "window-system: %s" window-system)
(message "user-login-name: %s" user-login-name)
(message "user-init-file: %s" user-init-file)
(message "user-emacs-directory: %s" user-emacs-directory)

;;==============================================================================
;; init-base
;;==============================================================================

(load-file "~/.emacs.d/init-base.el")
(require 'init-base)

;;==============================================================================
;; Packages
;;==============================================================================

(require 'package)
(setq package-archives `(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("nongnu" . "http://elpa.nongnu.org/nongnu/")))

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
(require 'use-package)
(setq use-package-always-ensure t)

;; Ahead-of-time native compilation when installing a package
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq package-native-compile t))

;;==============================================================================
;; Custom Set Variables
;;==============================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(initial-frame-alist '((fullscreen . maximized)))
 '(org-file-apps
   '((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . emacs)))
 '(package-selected-packages
   '(string-utils transient flx helpful elisp-demos undo-tree vundo exec-path-from-shell openwith magit magit-popup magit-stats all-the-icons nerd-icons nerd-icons-completion nerd-icons-dired nerd-icons-ibuffer doom-themes doom-modeline nyan-mode hide-mode-line minibar centaur-tabs page-break-lines dashboard centered-cursor-mode which-key hydra pretty-hydra flycheck auto-complete org-bullets org-present org-tree-slide org-re-reveal markdown-mode markdown-preview-mode obsidian texfrag magic-latex-buffer ag wgrep-ag rg ripgrep deadgrep wgrep-deadgrep dumb-jump occurx-mode peep-dired dirvish helm helm-ag helm-rg ace-window projectile restclient company company-statistics company-box company-restclient company-fuzzy yasnippet ivy all-the-icons-ivy counsel counsel-projectile counsel-at-point swiper ivy-rich all-the-icons-ivy-rich nerd-icons-ivy-rich ivy-posframe avy redacted insecure-lock find-file-in-project spell-fu perspective treemacs treemacs-projectile treemacs-magit treemacs-perspective treemacs-nerd-icons neotree dir-treeview dir-treeview-themes ztree google-c-style highlight-indent-guides highlight-indentation filldent gnu-indent rainbow-delimiters tree-sitter tree-sitter-langs lsp-mode lsp-ui helm-lsp lsp-ivy lsp-treemacs dap-mode ccls objc-font-lock swift-mode pip-requirements py-autopep8 epc importmagic pyvenv lsp-pyright elpy ein typescript-mode haskell-mode lua-mode cuda-mode json-mode json-snatcher json-reformat yaml-mode qml-mode cmake-mode i3wm-config-mode ligature docker dockerfile-mode docker-compose-mode graphviz-dot-mode focus rich-minority vdiff vdiff-magit diff-hl pdf-tools nov vterm multi-vterm vlf keyfreq imenu-list shrink-path neato-graph-bar proced-narrow disk-usage go-translate mpv yeetube elfeed elfeed-tube elfeed-tube-mpv md4rd devdocs devdocs-browser arxiv-mode arxiv-citation wordel command-log-mode use-package)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:foreground "#8be9fd" :background "#8be9fd"))))
 '(diff-hl-delete ((t (:foreground "#ff5555" :background "#ff5555"))))
 '(diff-hl-insert ((t (:foreground "#50fa7b" :background "#50fa7b"))))
 '(ediff-current-diff-A ((t (:background "#7f3c63"))))
 '(ediff-current-diff-B ((t (:background "#287d3d"))))
 '(ediff-current-diff-C ((t (:background "#45747e"))))
 '(ediff-even-diff-A ((t (:background "#464752"))))
 '(ediff-even-diff-B ((t (:background "#464752"))))
 '(ediff-even-diff-C ((t (:background "#464752"))))
 '(ediff-fine-diff-A ((t (:foreground "#282a36" :background "#ff79c6"))))
 '(ediff-fine-diff-B ((t (:foreground "#282a36" :background "#50fa7b"))))
 '(ediff-fine-diff-C ((t (:foreground "#282a36" :background "#8be9fd"))))
 '(ediff-odd-diff-A ((t (:background "#464752"))))
 '(ediff-odd-diff-B ((t (:background "#464752"))))
 '(ediff-odd-diff-C ((t (:background "#464752"))))
 '(ein:basecell-input-area-face ((t (:extend t :background "#23242f"))) t)
 '(fringe ((t (:background "#282a36"))))
 '(header-line ((t (:background "#23242f"))))
 '(md4rd--greentext-face ((((type graphic) (background dark)) :background unspecified :foreground "#50fa7b") (((type graphic) (background light)) :background unspecified :foreground "#50fa7b") (t :background unspecified :foreground "#50fa7b")))
 '(minimap-active-region-background ((t (:background "#1e2029" :extend t))))
 '(minimap-current-line-face ((t (:background "#0189cc" :extend t))))
 '(minimap-font-face ((default :family "JetBrains Mono" :height 30)))
 '(mode-line ((t (:background "#455073"))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.25))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.15))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.05))))
 '(org-tree-slide-heading-level-1-init ((t (:inherit outline-1 :height 1.25))))
 '(org-tree-slide-heading-level-2-init ((t (:inherit outline-2 :height 1.15))))
 '(org-tree-slide-heading-level-3-init ((t (:inherit outline-3 :height 1.1))))
 '(org-tree-slide-heading-level-4-init ((t (:inherit outline-4 :height 1.05))))
 '(show-paren-match ((((class color)) :foreground unspecified :background "#455073" :weight bold) (t :weight bold)))
 '(wordel-almost ((t (:background "#acb364"))))
 '(wordel-box ((t (:box (:line-width -4 :color "#1e2029" :style released-button)))))
 '(wordel-correct ((t (:background "#39b357"))))
 '(wordel-current-box ((t (:box (:line-width -4 :color "#ffb86c" :style released-button)))))
 '(wordel-default ((t (:weight ultra-bold :background "#44475a" :foreground "#D7DADC" :height 3.0))))
 '(wordel-error ((t (:inherit compilation-error))))
 '(wordel-guessed ((t (:background "#202020" :foreground "#616580"))))
 '(wordel-spacer ((t (:width ultra-condensed :height 0.1 :background unspecified)))))

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

(use-package string-utils)

;;==============================================================================
;; transient
;;==============================================================================

(use-package transient)

;;==============================================================================
;; flx
;;==============================================================================

(use-package flx)

;;==============================================================================
;; helpful
;;
;; https://github.com/Wilfred/helpful
;;==============================================================================

(use-package helpful
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))

;;==============================================================================
;; elisp-demos
;;
;; https://github.com/xuchunyang/elisp-demos
;;==============================================================================

(use-package elisp-demos
  :config
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;;==============================================================================
;; buffer-focus-hook
;;
;; https://github.com/mschuldt/buffer-focus-hook
;;==============================================================================

(load-file "~/.emacs.d/buffer-focus-hook.elc")

;;==============================================================================
;; ace-window
;;==============================================================================

(use-package ace-window
  :config
  (setq aw-dispatch-when-more-than 4
        aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  (defun aw-select-previous (mode-line &optional action)
    "Return a selected other window.
Amend MODE-LINE to the mode line for the duration of the selection."
    (setq aw-action action)
    (let ((start-window (selected-window))
          (previous-window-scope (cl-case aw-scope
                                   (visible 'visible)
                                   (global 'visible)
                                   (frame 'frame)))
          (wnd-list (aw-window-list))
          window)
      (setq window
            (cond ((<= (length wnd-list) 1)
                   (when aw-dispatch-always
                     (setq aw-action
                           (unwind-protect
                               (catch 'done
                                 (funcall aw-dispatch-function (read-char)))
                             (aw--done)))
                     (when (eq aw-action 'exit)
                       (setq aw-action nil)))
                   (or (car wnd-list) start-window))
                  ((and (<= (+ (length wnd-list) (if (aw-ignored-p start-window) 1 0))
                            aw-dispatch-when-more-than)
                        (not aw-dispatch-always)
                        (not aw-ignore-current))
                   (let ((wnd (previous-window nil nil previous-window-scope)))
                     (while (and (or (not (memq wnd wnd-list))
                                     (aw-ignored-p wnd))
                                 (not (equal wnd start-window)))
                       (setq wnd (previous-window wnd nil previous-window-scope)))
                     wnd))
                  (t
                   (let ((candidate-list
                          (mapcar (lambda (wnd)
                                    (cons (aw-offset wnd) wnd))
                                  wnd-list)))
                     (aw--make-backgrounds wnd-list)
                     (aw-set-mode-line mode-line)
                     ;; turn off helm transient map
                     (remove-hook 'post-command-hook 'helm--maybe-update-keymap)
                     (unwind-protect
                         (let* ((avy-handler-function aw-dispatch-function)
                                (avy-translate-char-function aw-translate-char-function)
                                (transient-mark-mode nil)
                                (res (avy-read (avy-tree candidate-list aw-keys)
                                               (if (and ace-window-display-mode
                                                        (null aw-display-mode-overlay))
                                                   (lambda (_path _leaf))
                                                 aw--lead-overlay-fn)
                                               aw--remove-leading-chars-fn)))
                           (if (eq res 'exit)
                               (setq aw-action nil)
                             (or (cdr res)
                                 start-window)))
                       (aw--done))))))
      (if aw-action
          (funcall aw-action window)
        window)))

  (defun ace-select-previous-window ()
    "Ace select window."
    (interactive)
    (aw-select-previous " Ace - Window"
                        #'aw-switch-to-window)))

;;==============================================================================
;; undo-tree
;;
;; https://www.emacswiki.org/emacs/UndoTree
;;==============================================================================

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  ;; Prevent undo tree files from polluting your git repo
  ;; https://www.reddit.com/r/emacs/comments/tejte0/undotree_bug_undotree_files_scattering_everywhere/
  (setq undo-tree-auto-save-history nil)
  ;;(setq undo-tree-history-directory-alist
  ;;      `((".*" . ,(concat user-emacs-directory "undo-tree-history"))))
  )

;;==============================================================================
;; vundo
;;
;; https://github.com/casouri/vundo
;;==============================================================================

(use-package vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

;;==============================================================================
;; exec-path-from-shell
;;
;; https://github.com/purcell/exec-path-from-shell
;;==============================================================================

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)))

;;==============================================================================
;; Setting the default web browser
;;
;; http://ergoemacs.org/emacs/emacs_set_default_browser.html
;;==============================================================================

(defun browse-url-qutebrowser (url &optional new-window)
  "Ask the Nyxt web browser to load URL."
  (interactive (browse-url-interactive-arg "URL (qutebrowser): "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply #'start-process
           (concat "qutebrowser " url) nil
           "qutebrowser"
           (list url))))

(when (string-equal system-type "gnu/linux")
  (defun browse-url-surf (url &optional new-window)
    "Ask the Surf web browser to load URL."
    (interactive (browse-url-interactive-arg "URL (Surf): "))
    (setq url (browse-url-encode-url url))
    (let* ((process-environment (browse-url-process-environment)))
      (apply #'start-process
             (concat "surf " url) nil
             "surf"
             (list url))))

  (defun browse-url-nyxt (url &optional new-window)
    "Ask the Nyxt web browser to load URL."
    (interactive (browse-url-interactive-arg "URL (Nyxt): "))
    (setq url (browse-url-encode-url url))
    (let* ((process-environment (browse-url-process-environment)))
      (apply #'start-process
             (concat "nyxt " url) nil
             "nyxt"
             (list url)))))

(if (string-equal system-type "gnu/linux")
    (setq browse-url-browser-function 'browse-url-qutebrowser))

;;==============================================================================
;; Setting the default app with an association
;;==============================================================================

(use-package openwith
  :config
  (openwith-mode t)
  (setq openwith-associations '(("\\.\\(?:ts\\|mxf\\|mpe?g\\|mov\\|mp4\\|avi\\|mkv\\|wmv\\)\\'" "mpv" (file))))

  ;; https://emacs.stackexchange.com/questions/17095/how-supress-dired-confirmation-of-large-file-for-specific-extensions
  (defvar daftemacs-ok-large-file-types
    (mapconcat 'car openwith-associations "\\|")
    "Regexp matching filenames which are definitely ok to visit,
even when the file is larger than `large-file-warning-threshold'.")
  (defun abort-if-file-too-large--daftemacs-check-ok-large-file-types (orig-fun size op-type filename &rest args)
    "If FILENAME matches `daftemacs-ok-large-file-types', do not abort."
    (unless (string-match-p daftemacs-ok-large-file-types filename)
      (apply orig-fun size op-type filename args)))
  (advice-add 'abort-if-file-too-large :around #'abort-if-file-too-large--daftemacs-check-ok-large-file-types))

;;==============================================================================
;; magit
;;==============================================================================

(use-package magit)

(use-package magit-popup)

(use-package magit-stats)

;; https://emacs.stackexchange.com/questions/3108/git-stage-the-current-file-visiting-buffer
(defun git-add-current-buffer ()
  "Call 'git add [current-buffer]'"
  (interactive)
  (let* ((buffile (buffer-file-name))
         (output (shell-command-to-string
                  (concat "git add " (buffer-file-name)))))
    (message (if (not (string= output ""))
                 output
               (concat "Added " buffile)))))

;;==============================================================================
;; all-the-icons
;;==============================================================================

(use-package all-the-icons
  :config
  (setq all-the-icons-scale-factor 1.0))

;;==============================================================================
;; nerd-icons
;;
;; https://github.com/ryanoasis/nerd-fonts
;;
;; Run 'nerd-icons-install-fonts' to install Nerd fonts.
;;==============================================================================

(use-package nerd-icons)

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :hook
  (ibuffer-mode . nerd-icons-ibuffer-mode))

;;==============================================================================
;; doom-themes
;;==============================================================================

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (setq doom-themes-neotree-file-icons t)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (setq doom-themes-treemacs-enable-variable-pitch nil)
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; https://draculatheme.com/emacs/
;;(load-file "~/.emacs.d/dracula-theme.el")
;;(load-theme 'dracula t)

;;==============================================================================
;; Modeline
;;==============================================================================

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom-face
  (doom-modeline-buffer-modified ((t (:inherit (warning bold) :foreground "#ff5555" :background unspecified))))
  :init
  ;; If non-nil, cause imenu to see `doom-modeline' declarations.
  ;; This is done by adjusting `lisp-imenu-generic-expression' to
  ;; include support for finding `doom-modeline-def-*' forms.
  ;; Must be set before loading doom-modeline.
  (setq doom-modeline-support-imenu t)

  ;; How tall the mode-line should be. It's only respected in GUI.
  ;; If the actual char height is larger, it respects the actual height.
  (cond
   ((string-equal system-type "darwin")
    (setq doom-modeline-height 25))
   ((string-equal system-type "gnu/linux")
    (if with-pgtk
        (setq doom-modeline-height 25)
      (setq doom-modeline-height 50))))

  ;; How wide the mode-line bar should be. It's only respected in GUI.
  (setq doom-modeline-bar-width 3)

  ;; Whether to use hud instead of default bar. It's only respected in GUI.
  (setq doom-modeline-hud nil)

  ;; The limit of the window width.
  ;; If `window-width' is smaller than the limit, some information won't be
  ;; displayed. It can be an integer or a float number. `nil' means no limit."
  (setq doom-modeline-window-width-limit 85)

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
  ;;   auto => emacs/l/comint.el (in a project) or comint.el
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
  (setq doom-modeline-icon t)

  ;; Whether display the icon for `major-mode'. It respects `doom-modeline-icon'.
  (setq doom-modeline-major-mode-icon t)

  ;; Whether display the colorful icon for `major-mode'.
  ;; It respects `nerdg-icons-color-icons'.
  (setq doom-modeline-major-mode-color-icon t)

  ;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
  (setq doom-modeline-buffer-state-icon t)

  ;; Whether display the modification icon for the buffer.
  ;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
  (setq doom-modeline-buffer-modification-icon t)

  ;; Whether display the time icon. It respects variable `doom-modeline-icon'.
  (setq doom-modeline-time-icon t)

  ;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
  (setq doom-modeline-unicode-fallback nil)

  ;; Whether display the buffer name.
  (setq doom-modeline-buffer-name t)

  ;; Whether highlight the modified buffer name.
  (setq doom-modeline-highlight-modified-buffer-name t)

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

  ;; Whether display the modal state.
  ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  (setq doom-modeline-modal t)

  ;; Whether display the modal state icon.
  ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  (setq doom-modeline-modal-icon t)

  ;;;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
  ;;(setq doom-modeline-mu4e nil)
  ;;;; also enable the start of mu4e-alert
  ;;(mu4e-alert-enable-mode-line-display)

  ;; Whether display the gnus notifications.
  (setq doom-modeline-gnus t)

  ;; Whether gnus should automatically be updated and how often (set to 0 or smaller than 0 to disable)
  (setq doom-modeline-gnus-timer 2)

  ;; Wheter groups should be excludede when gnus automatically being updated.
  (setq doom-modeline-gnus-excluded-groups '("dummy.group"))

  ;; Whether display the IRC notifications. It requires `circe' or `erc' package.
  (setq doom-modeline-irc t)

  ;; Function to stylize the irc buffer names.
  (setq doom-modeline-irc-stylize 'identity)

  ;; Whether display the battery status. It respects `display-battery-mode'.
  (setq doom-modeline-battery t)

  ;; Whether display the time. It respects `display-time-mode'.
  (setq doom-modeline-time t)

  ;; Whether display the misc segment on all mode lines.
  ;; If nil, display only if the mode line is active.
  (setq doom-modeline-display-misc-in-all-mode-lines t)

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

  ;;;; By default, almost all segments are displayed only in the active window. To
  ;;;; display such segments in all windows, specify e.g.
  ;;(setq doom-modeline-always-visible-segments '(mu4e irc))

  ;; Hooks that run before/after the modeline version string is updated
  (setq doom-modeline-before-update-env-hook nil)
  (setq doom-modeline-after-update-env-hook nil))

;;(dracula-setup-modeline-format)

;; Custom visible bell
;; https://www.emacswiki.org/emacs/AlarmBell
(setq ring-bell-function
      (lambda ()
        (let ((orig-bg (face-background 'mode-line)))
          (set-face-background 'mode-line "#ff5555")
          (run-with-idle-timer 0.1 nil
                               (lambda (bg) (set-face-background 'mode-line bg))
                               orig-bg))))

;;==============================================================================
;; nyan-mode
;;
;; https://github.com/TeMPOraL/nyan-mode
;;==============================================================================

(use-package nyan-mode
  :config
  (setq nyan-animate-nyancat nil
        nyan-wavy-trail nil
        nyan-bar-length 40)
  (nyan-mode 1))

;;==============================================================================
;; hide-mode-line
;;==============================================================================

(use-package hide-mode-line)

;;==============================================================================
;; minibar
;;
;; https://codeberg.org/akib/emacs-minibar
;;==============================================================================

(cond
 ((string-equal system-type "darwin")
  (use-package minibar
    :config
    (minibar-mode +1)
    (setq minibar-group-middle '(minibar-module-battery minibar-module-time))))
 ((string-equal system-type "gnu/linux")
  (use-package minibar
    :config
    (minibar-mode +1)

    (defun daftemacs-minibar-module-temperature ()
      "Module for showing CPU temperature."
      (when (or (not minibar--module-temperature-cache)
                (>= (float-time
                     (time-since (cdr minibar--module-temperature-cache)))
                    minibar-module-temperature-cache-for))
        (setq
         minibar--module-temperature-cache
         (cons
          (let ((zone 1))
            (with-temp-buffer
              (insert-file-contents
               (car (file-expand-wildcards (format "/sys/devices/platform/coretemp.0/hwmon/hwmon*/temp%i_input" zone))))
              (let* ((temp (/ (string-to-number (buffer-string)) 1000))
                     (str (concat (int-to-string temp)
                                  (if (char-displayable-p ?°) "°" " ")
                                  "C")))
                (if (>= temp minibar-module-temperature-high-threshold)
                    (propertize
                     str 'face
                     (if (>=
                          temp
                          minibar-module-temperature-very-high-threshold)
                         'minibar-module-temperature-high-face
                       'minibar-module-temperature-very-high-face))
                  str))))
          (current-time))))
      (car minibar--module-temperature-cache))

    (setq minibar-group-middle '(minibar-module-cpu daftemacs-minibar-module-temperature minibar-module-network-speeds minibar-module-battery minibar-module-time)))))

;;==============================================================================
;; minimap
;;
;; https://github.com/dengste/minimap
;;==============================================================================

(load-file "~/.emacs.d/minimap.elc")
(require 'minimap)

(setq minimap-window-location 'right
      minimap-update-delay 0.2
      minimap-enlarge-certain-faces 'always
      minimap-major-modes '(prog-mode yaml-mode))

(defun minimap-ignore-specific-buffers ()
  (or (string-match "*scratch" (buffer-name))
      (string-match "\s?\*ein" (buffer-name))))

(when (boundp 'aw-ignored-buffers)
  (push minimap-buffer-name aw-ignored-buffers))

;;==============================================================================
;; centaur-tabs
;;
;; https://github.com/ema2159/centaur-tabs
;;==============================================================================

(use-package centaur-tabs
  :demand
  :custom-face
  (centaur-tabs-default ((t (:background "#1e2029" :foreground "#f8f8f2"))))
  (centaur-tabs-selected ((t (:background "#455073" :foreground "#f8f8f2"))))
  (centaur-tabs-selected-modified ((t (:background "#455073" :foreground "#ff5555"))))
  (centaur-tabs-unselected ((t (:background "#1e2029" :foreground "#6272a4"))))
  (centaur-tabs-unselected-modified ((t (:background "#1e2029" :foreground "#b33939"))))
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "🔥"
        centaur-tabs-set-close-button nil
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-down-tab-text " 🚀 "
        centaur-tabs-backward-tab-text ""
        centaur-tabs-forward-tab-text ""
        centaur-tabs-show-count t
        centaur-tabs-new-tab-text " ⭐ ")
  (cond
   ((string-equal system-type "darwin")
    (setq centaur-tabs-height 25)
    (setq centaur-tabs-bar-height 25))
   ((string-equal system-type "gnu/linux")
    (if (string-equal (getenv "GDK_SCALE") "2")
        (if with-pgtk
            (progn
              (setq centaur-tabs-height 25)
              (setq centaur-tabs-bar-height 25))
          (progn
            (setq centaur-tabs-height 50)
            (setq centaur-tabs-bar-height 34)))
      (progn
        (setq centaur-tabs-height 25)
        (setq centaur-tabs-bar-height 25)))))
  (setq centaur-tabs-active-bar
        (centaur-tabs--make-xpm 'centaur-tabs-active-bar-face
                                2
                                centaur-tabs-bar-height))

  (defsubst daftemacs-centaur-tabs-button-tab (button)
    "Return the display representation of button BUTTON.
That is, a propertized string used as an `centaur-tabs-display-line-format'
template element."
    (let* ((face 'centaur-tabs-default))
      (concat
       (propertize
        button
        'face face
        'mouse-face 'highlight))))

  (defun centaur-tabs-count (index count)
    "Return a centaur-tabs-button-tab with the current tab INDEX and the total tabs COUNT."
    (if centaur-tabs-show-count
        (propertize (daftemacs-centaur-tabs-button-tab (format " [%d/%d] " index count))
                    'help-echo "Tabs count")
      ""))

  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)

  ;; Projectile integration
  (centaur-tabs-group-by-projectile-project)

  (defun centaur-tabs-toggle-group-by ()
    "Toggle centaur tabs group by 'projectile project' or 'buffer group'."
    (interactive)
    (if (eq centaur-tabs-buffer-groups-function 'centaur-tabs-buffer-groups)
        (progn
          (message "Centaur tabs group by 'projectile project'.")
          (centaur-tabs-group-by-projectile-project))
      (progn
        (message "Centaur tabs group by 'buffer group'.")
        (centaur-tabs-group-buffer-groups))))

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
            (if centaur-tabs--buffer-show-groups
                (let ((bufname (centaur-tabs-tab-tabset tab)))
                  (if (> centaur-tabs-label-fixed-length 0)
                      (centaur-tabs-truncate-string centaur-tabs-label-fixed-length bufname)
                    bufname))
              (let ((bufname (buffer-name (car tab))))
                (if (> centaur-tabs-label-fixed-length 0)
                    (centaur-tabs-truncate-string centaur-tabs-label-fixed-length bufname)
                  (if (and (> centaur-tabs-label-max-length 0) (< centaur-tabs-label-max-length (string-width bufname)))
                      (centaur-tabs-truncate-string centaur-tabs-label-max-length bufname)
                    bufname))))))

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
      ((or (string-match "magit-?[0-9a-zA-Z]*?: " (buffer-name))
           (string-match "COMMIT_EDITMSG" (buffer-name))
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
       (string-prefix-p " *which-key" name)
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*Backtrace*" name)
       (string-prefix-p "*Bufler*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*LSP" name)
       (string-prefix-p "*company" name)
       (string-prefix-p "*Flycheck" name)
       (string-prefix-p "*tramp" name)
       (string-prefix-p " *Mini" name)
       (string-prefix-p "*help" name)
       (string-prefix-p "*straight" name)
       (string-prefix-p " *temp" name)
       (string-prefix-p "*Help" name)
       (string-prefix-p "*Process List" name)
       (string-prefix-p "*Register Preview" name)
       (string-prefix-p "*Calc" name)
       (string-prefix-p "*calculator" name)
       (string-prefix-p "*Ilist" name)
       (string-prefix-p "*Ediff" name)
       (string-prefix-p "*vterminal - dedicated" name)
       (string-match "\s?\*ein" (buffer-name)))))
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
  ("C-c t g" . centaur-tabs-toggle-group-by)
  ("C-c t k" . centaur-tabs-kill-all-buffers-in-current-group))

;;==============================================================================
;; page-break-lines
;;
;; https://github.com/purcell/page-break-lines
;;==============================================================================

(use-package page-break-lines)

;;==============================================================================
;; dashboard
;;
;; https://github.com/emacs-dashboard/emacs-dashboard
;;==============================================================================

(use-package dashboard
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents . 10)
                          (bookmarks . 10)
                          (projects . 10)
                          (agenda . 10)
                          (registers . 10)))
  (setq dashboard-page-separator "\n\f\n") ;; This depends on page-break-lines.
  (dashboard-setup-startup-hook)
  (when (< (length command-line-args) 2)
    (add-hook 'emacs-startup-hook (lambda ()
                                    (switch-to-buffer dashboard-buffer-name)
                                    (set-buffer-modified-p nil)))))

;;==============================================================================
;; centered-window (The customized version)
;;
;; Based on https://github.com/anler/centered-window-mode
;;==============================================================================

(load-file "~/.emacs.d/centered-window.elc")
(require 'centered-window-mode)

(if with-pgtk
    (setq-default cwm-centered-window-width 200)
  (setq-default cwm-centered-window-width 180))

(add-hook 'dashboard-after-initialize-hook #'(lambda ()
                                               (centered-window-mode)))

;;==============================================================================
;; centered-cursor-mode
;;
;; https://github.com/andre-r/centered-cursor-mode.el
;;==============================================================================

(use-package centered-cursor-mode)

;;==============================================================================
;; which-key
;;
;; https://github.com/justbur/emacs-which-key
;;
;; 'which-key' should be loaded after loading 'centaur-tabs' in order to hide
;; centaur-tabs in which-key buffer.
;;==============================================================================

(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

;;==============================================================================
;; hydra
;;
;; https://github.com/abo-abo/hydra
;; https://github.com/jerrypnz/major-mode-hydra.el
;;==============================================================================

(use-package hydra)

(use-package pretty-hydra)

;;==============================================================================
;; flycheck
;;==============================================================================

(use-package flycheck)
;;(add-hook 'after-init-hook #'global-flycheck-mode)

;;==============================================================================
;; auto-complete
;;
;; https://github.com/auto-complete/auto-complete
;;==============================================================================

(use-package auto-complete)

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
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(defun daftemacs-org-style ()
  (read-only-mode)
  (visual-line-mode)
  (org-indent-mode)

  (mapc
   (lambda (face)
     (set-face-attribute face nil :height 0.75))
   (list 'org-document-info-keyword
         'org-meta-line
         'org-block-begin-line
         'org-drawer
         'org-property-value)))

(add-hook 'org-mode-hook 'daftemacs-org-style)

;; Change size of the inline image for LaTeX fragment in org-mode
;; https://tex.stackexchange.com/questions/78501/change-size-of-the-inline-image-for-latex-fragment-in-emacs-org-mode
(plist-put org-format-latex-options :scale 2)

;; org-present
(use-package org-present
  :config
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)))

  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images))))

;; org-tree-slide
(use-package org-tree-slide
  :config
  (define-key org-mode-map (kbd "<f12>") 'org-tree-slide-mode)
  (define-key org-mode-map (kbd "S-<f12>") 'org-tree-slide-skip-done-toggle)
  (define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "<f10>") 'org-tree-slide-move-next-tree)
  (define-key org-tree-slide-mode-map (kbd "<f11>") 'org-tree-slide-content)

  (defun daftemacs-ots-presentation-start ()
    (setq org-tree-slide-skip-outline-level 4)
    (org-tree-slide-presentation-profile)
    (setq org-tree-slide-heading-emphasis t)
    (setq org-tree-slide-skip-done nil)
    (view-mode-enter)
    (text-scale-set 5)
    (set-window-margins (selected-window) 12 12)
    (hide-mode-line-mode)
    (centaur-tabs-local-mode nil))

  (defun daftemacs-ots-presentation-end ()
    (View-exit-and-edit)
    (text-scale-set 0)
    (set-window-margins (selected-window) 0 0)
    (hide-mode-line-mode -1)
    (centaur-tabs-local-mode 0))

  (add-hook 'org-tree-slide-play-hook 'daftemacs-ots-presentation-start)
  (add-hook 'org-tree-slide-stop-hook 'daftemacs-ots-presentation-end))

;; org-re-reveal
;; https://gitlab.com/oer/org-re-reveal
;; https://github.com/hakimel/reveal.js
(use-package org-re-reveal
  :config
  (setq org-re-reveal-root (concat (getenv "HOME") "/.emacs.d/reveal.js")))

;; Embedding YouTube videos with org-mode links
;;
;; Based on http://endlessparentheses.com/embedding-youtube-videos-with-org-mode-links.html
;;
;; - Examples
;;   [[YouTube:KJritAFWnUM]]
;;   [[YouTube:KJritAFWnUM/640x360]]
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
;; markdown
;;==============================================================================

(use-package markdown-mode
  :config
  (setq markdown-command "pandoc"
        markdown-enable-math t
        markdown-fontify-code-blocks-natively t)

  ;; 'C-M-{' and 'C-M-}' are used for 'centaur-tabs'.
  (define-key markdown-mode-map (kbd "C-M-{") nil)
  (define-key markdown-mode-map (kbd "C-M-}") nil)

  (defun daftemacs-markdown-style ()
    (setq indent-tabs-mode t)
    (setq tab-width 4)
    (visual-line-mode t)
    (texfrag-mode +1))

  (add-hook 'markdown-mode-hook 'daftemacs-markdown-style)
  :bind
  ("M-+" . markdown-backward-block)
  ("M-\"" . markdown-forward-block))

;; markdown-preview-mode
;; https://github.com/ancane/markdown-preview-mode
(use-package markdown-preview-mode
  :config
  (setq markdown-preview-stylesheets (list "~/.emacs.d/css/github-markdown.css"))
  (add-to-list 'markdown-preview-javascript "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML"))

;;==============================================================================
;; obsidian
;;
;; https://github.com/licht1stein/obsidian.el
;;==============================================================================

(use-package obsidian
  :demand t
  :config
  (obsidian-specify-path "~/Workspace/daftvaults")
  (global-obsidian-mode t)
  :custom
  ;; This directory will be used for `obsidian-capture' if set.
  (obsidian-inbox-directory "Inbox")
  ;; Create missing files in inbox? - when clicking on a wiki link
  ;; t: in inbox, nil: next to the file with the link
  ;; default: t
  ;(obsidian-wiki-link-create-file-in-inbox nil)
  ;; The directory for daily notes (file name is YYYY-MM-DD.md)
  (obsidian-daily-notes-directory "Daily Notes")
  ;; Directory of note templates, unset (nil) by default
  ;(obsidian-templates-directory "Templates")
  ;; Daily Note template name - requires a template directory. Default: Daily Note Template.md
  ;(setq obsidian-daily-note-template "Daily Note Template.md")
  :bind (:map obsidian-mode-map
  ;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
  ("C-c C-o" . obsidian-follow-link-at-point)
  ;; Jump to backlinks
  ("C-c C-b" . obsidian-backlink-jump)
  ;; If you prefer you can use `obsidian-insert-link'
  ("C-c C-l" . obsidian-insert-wikilink)))

;;==============================================================================
;; LaTeX Preview
;;==============================================================================

;; https://github.com/TobiasZawada/texfrag
(use-package texfrag)

;; https://github.com/zk-phi/magic-latex-buffer
(use-package magic-latex-buffer
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
  :config
  (setq ag-arguments (list "--smart-case" "--stats" "-a" "--hidden")))

(use-package wgrep-ag)

(use-package rg
  :config
  (setq rg-command-line-flags '("--no-ignore" "--hidden" "--unrestricted")))

(use-package ripgrep
  :config
  (setq ripgrep-arguments (list "--no-ignore" "--hidden" "--unrestricted")))

(use-package deadgrep
  :config
  (defun deadgrep--arguments (search-term search-type case context)
    "Return a list of command line arguments that we can execute in a shell
to obtain ripgrep results."
    (let (args)
      (push "--hidden" args)
      (push "--no-ignore" args)
      (push "--unrestricted" args)

      (push "--color=ansi" args)
      (push "--line-number" args)
      (push "--no-heading" args)
      (push "--no-column" args)
      (push "--with-filename" args)
      (push "--no-config" args)

      (cond
       ((eq search-type 'string)
        (push "--fixed-strings" args))
       ((eq search-type 'words)
        (push "--fixed-strings" args)
        (push "--word-regexp" args))
       ((eq search-type 'regexp))
       (t
        (error "Unknown search type: %s" search-type)))

      (cond
       ((eq case 'smart)
        (push "--smart-case" args))
       ((eq case 'sensitive)
        (push "--case-sensitive" args))
       ((eq case 'ignore)
        (push "--ignore-case" args))
       (t
        (error "Unknown case: %s" case)))

      (cond
       ((eq deadgrep--file-type 'all))
       ((eq (car-safe deadgrep--file-type) 'type)
        (push (format "--type=%s" (cdr deadgrep--file-type)) args))
       ((eq (car-safe deadgrep--file-type) 'glob)
        (push (format "--type-add=custom:%s" (cdr deadgrep--file-type)) args)
        (push "--type=custom" args))
       (t
        (error "Unknown file-type: %S" deadgrep--file-type)))

      (when context
        (push (format "--before-context=%s" (car context)) args)
        (push (format "--after-context=%s" (cdr context)) args))

      (push "--" args)
      (push search-term args)
      (push "." args)

      (nreverse args))))

(use-package wgrep-deadgrep)

(use-package dumb-jump
  :config
  (dumb-jump-mode))

;;==============================================================================
;; occurx-mode
;;
;; https://github.com/k32/occurx-mode
;;==============================================================================

(use-package occurx-mode)

;;==============================================================================
;; dired
;;==============================================================================

;; peep-dired: preview files in dired
(use-package peep-dired
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

;;==============================================================================
;; dirvish
;;
;; https://github.com/alexluigit/dirvish
;;==============================================================================

(use-package dirvish
  :init
  ;; Let Dirvish take over Dired globally
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"           "Home")
     ("d" "~/Downloads/" "Downloads")
     ("c" "~/Documents/" "Documents")))
  :config
  ;; Placement
  ;;(setq dirvish-use-header-line nil)     ; hide header line (show the classic dired header)
  ;;(setq dirvish-use-mode-line nil)       ; hide mode line
  ;;(setq dirvish-use-header-line 'global) ; make header line span all panes

  ;; Height
  ;;; '(25 . 35) means
  ;;;   - height in single window sessions is 25
  ;;;   - height in full-frame sessions is 35
  (setq dirvish-header-line-height '(34 . 34))
  (setq dirvish-mode-line-height 34) ; shorthand for '(34 . 34)

  ;; Segments
  ;;; 1. the order of segments *matters* here
  ;;; 2. it's ok to place raw string inside
  (setq dirvish-header-line-format
        '(:left (path) :right (free-space))
        dirvish-mode-line-format
        '(:left (sort file-time " " file-size symlink) :right (omit yank index)))

  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))

  (setq dirvish-attributes
        '(;;all-the-icons
          file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls" dired-use-ls-dired t))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (:map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
        ("a"   . dirvish-quick-access)
        ("f"   . dirvish-file-info-menu)
        ("y"   . dirvish-yank-menu)
        ("N"   . dirvish-narrow)
        ("^"   . dirvish-history-last)
        ("h"   . dirvish-history-jump) ; remapped `describe-mode'
        ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
        ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
        ("TAB" . dirvish-subtree-toggle)
        ("M-f" . dirvish-history-go-forward)
        ("M-b" . dirvish-history-go-backward)
        ("M-l" . dirvish-ls-switches-menu)
        ("M-m" . dirvish-mark-menu)
        ("M-t" . dirvish-layout-toggle)
        ("M-s" . dirvish-setup-menu)
        ("M-e" . dirvish-emerge-menu)
        ("M-j" . dirvish-fd-jump))
  :hook
  (dirvish-mode . centaur-tabs-local-mode))

;;==============================================================================
;; helm
;;==============================================================================

(use-package helm)

(use-package helm-ag)

(use-package helm-rg)

;;==============================================================================
;; projectile
;;
;; https://projectile.mx/
;;==============================================================================

(use-package projectile
  :config
  (setq projectile-enable-caching t)

  (defun projectile-project-root--ignore-remote (orig-fun &rest args)
    (unless (file-remote-p default-directory)
      (apply orig-fun args)))
  (advice-add 'projectile-project-root :around #'projectile-project-root--ignore-remote)
  ;;(message "projectile-globally-ignored-directories: %s" projectile-globally-ignored-directories)

  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c j") 'projectile-command-map))

;;==============================================================================
;; restclient
;;
;; https://github.com/pashky/restclient.el
;;==============================================================================

(use-package restclient
  :mode ("\\.http" . restclient-mode))

;;==============================================================================
;; company
;;==============================================================================

;; http://company-mode.github.io/
(use-package company
  :diminish company-mode
  :commands (company-complete company-mode)
  :bind
  (;([remap dabbrev-expand] . company-complete)
   ("C-." . company-complete)
   ("C-c c ." . counsel-company)
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
        company-idle-delay 0.5
        company-minimum-prefix-length 1
        company-show-numbers "on")
  (global-company-mode 1))

;; https://github.com/company-mode/company-statistics
(use-package company-statistics
  :hook (company-mode . company-statistics-mode))

;; https://github.com/sebastiencs/company-box
(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-restclient
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-restclient)))

;; https://github.com/jcs-elpa/company-fuzzy
(use-package company-fuzzy
  :hook (company-mode . company-fuzzy-mode)
  :init
  (setq company-fuzzy-sorting-backend 'flx
        company-fuzzy-prefix-on-top nil
        company-fuzzy-show-annotation t
        company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@"))
  (global-company-fuzzy-mode 1))

(with-eval-after-load 'company
  (message "company-backends: %s" company-backends))

(with-eval-after-load 'company-fuzzy
  (message "company-fuzzy--backends-%s" company-fuzzy--backends))

;;==============================================================================
;; yasnippet
;;==============================================================================

(use-package yasnippet
  :config
  (yas-global-mode 1))

;;==============================================================================
;; ivy, counsel, swiper
;;==============================================================================

(use-package ivy
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
  :hook (after-init . all-the-icons-ivy-setup))

(use-package counsel
  :diminish
  :hook (ivy-mode . counsel-mode)
  :config
  (setq counsel-rg-base-command "rg --vimgrep %s"))

(use-package counsel-projectile
  :config
  ;; The original keymaps for projectile are overrided.
  (setq counsel-projectile-key-bindings
        '((projectile-find-file        . counsel-projectile-find-file)
          (projectile-find-file-dwim   . counsel-projectile-find-file-dwim)
          (projectile-find-dir         . counsel-projectile-find-dir)
          (projectile-switch-to-buffer . counsel-projectile-switch-to-buffer)
          (projectile-switch-project   . counsel-projectile-switch-project)
          (" "                         . counsel-projectile)
          ("si"                        . counsel-projectile-git-grep)
          ("Oc"                        . counsel-projectile-org-capture)
          ("Oa"                        . counsel-projectile-org-agenda)))
  (counsel-projectile-mode +1)

  (global-set-key (kbd "C-c j s G") #'counsel-projectile-grep)
  (global-set-key (kbd "C-c j s R") #'counsel-projectile-rg)
  (global-set-key (kbd "C-c j s S") #'counsel-projectile-ag))

(use-package counsel-at-point)

(use-package swiper
  :after ivy
  :config
  (setq swiper-action-recenter t)
  (setq swiper-goto-start-of-match t))

(use-package ivy-rich
  :preface
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode)
          icon))))
  :init
  ;;(use-package all-the-icons-ivy-rich
  ;;  :init (all-the-icons-ivy-rich-mode 1))
  (use-package nerd-icons-ivy-rich
    :init (nerd-icons-ivy-rich-mode 1))
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

(use-package avy)

;;==============================================================================
;; insecure-lock - Screen locker in pure Emacs Lisp
;;
;; https://github.com/kchanqvq/insecure-lock
;;==============================================================================

(use-package redacted)

(use-package insecure-lock
  :config
  ;;(insecure-lock-run-idle 1800) ;; in seconds
  (setq insecure-lock-posframe-parameters
    '(:position (0 . 0) ;; workaround posframe bug
                :poshandler posframe-poshandler-frame-center
                :foreground-color "#50fa7b"
                :background-color "#44475a"
                :internal-border-width 3
                :internal-border-color "#ff79c6"))
  (defun daftemacs-insecure-lock-mode ()
    (insecure-lock-redact)
    (insecure-lock-posframe)
    (if insecure-lock-mode
        (progn
          (centaur-tabs-mode 0)
          (doom-modeline-mode 0)
          (global-hide-mode-line-mode))
      (progn
        (global-hide-mode-line-mode 0)
        (doom-modeline-mode t)
        (centaur-tabs-mode t))))
  (setq insecure-lock-mode-hook 'daftemacs-insecure-lock-mode)

  (defun daftemacs-after-insecure-lock-lock-keys ()
    (global-set-key (kbd "C-g") #'(lambda ()
                                    (interactive)
                                    (if (active-minibuffer-window)
                                        (select-window (active-minibuffer-window))))))
  (advice-add 'insecure-lock-lock-keys :after #'daftemacs-after-insecure-lock-lock-keys))

;;==============================================================================
;; find-file-in-project
;;
;; https://github.com/technomancy/find-file-in-project
;;==============================================================================

(use-package find-file-in-project)

;;==============================================================================
;; spell-fu
;;
;; https://gitlab.com/ideasman42/emacs-spell-fu
;;==============================================================================

(use-package spell-fu
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (setq ispell-personal-dictionary "~/.emacs.d/spell-fu/personal_dictionary.txt")
              (setq spell-fu-faces-exclude '(org-meta-line org-link org-code))
              (spell-fu-mode))))

;;==============================================================================
;; perspective
;;
;; https://github.com/nex3/perspective-el
;;==============================================================================

(use-package perspective
  :init
  (setq persp-modestring-dividers '("<" ">" "|")
        persp-modestring-short t
        persp-mode-prefix-key (kbd "C-c p")
        persp-avoid-killing-last-buffer-in-perspective nil)
  :config
  (persp-mode)
  (setq persp-sort 'access)

  (defun persp-buffer-list ()
    "Return a list of buffers of all living buffers in the current perspective."
    (let ((ignore-rx (persp--make-ignore-buffer-rx)))
      (cl-loop for buf in (persp-current-buffers)
               if (and (buffer-live-p buf)
                       (not (string-match-p ignore-rx (buffer-name buf))))
               collect buf)))

  (setq persp-state-default-file "~/.emacs.d/persp-state-default")
  (setq persp-state-default-file-loaded nil)

  (message "persp-state-default-file: %s (%s)" persp-state-default-file (file-exists-p persp-state-default-file))

  (defun persp-state-load-default ()
    "Restore the perspective state saved in the default file."
    (interactive)
    (if (file-exists-p persp-state-default-file)
        (if persp-state-default-file-loaded
            (message "The default perspective state has been already restored.")
          (progn
            (message "Restoring the default perspective state...")
            (persp-state-load persp-state-default-file)
            (message "The default perspective state has been restored.")
            (setq persp-state-default-file-loaded t)))
      (message "There is no saved file for the default perspective state.")))

  (add-hook 'persp-before-switch-hook #'(lambda ()
                                          (kill-side-windows)))

  (add-hook 'kill-emacs-hook #'(lambda ()
                                 (kill-side-windows)
                                 (when (or (not (file-exists-p persp-state-default-file)) persp-state-default-file-loaded)
                                   (persp-state-save))))

  ;;
  ;; Overriding 'centaur-tabs-buffer-list' in 'centaur-tabs-functions.el'
  ;; For smooth work with 'perspective', '(buffer-list)' is replaced with '(persp-buffer-list)'.
  ;;
  (defun centaur-tabs-buffer-list ()
    "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space, when they are not
visiting a file.  The current buffer is always included."
    (let* ((bufs (persp-buffer-list)))
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
                        bufs))))) ;; modified by daftcoder

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
    (let* ((bufs (persp-buffer-list))
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
;; File Tree
;;
;; treemacs (https://github.com/Alexander-Miller/treemacs)
;; neotree (https://github.com/jaypei/emacs-neotree)
;; dir-treeview (https://github.com/tilmanrassy/emacs-dir-treeview)
;; ztree (https://github.com/fourier/ztree)
;;==============================================================================

(use-package treemacs
  :defer t
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     t
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil)

    (defun treemacs-hide ()
      (interactive)
      (dolist (frame (frame-list))
        (dolist (window (window-list frame))
          (when (treemacs-is-treemacs-window? window)
            (delete-window window)))))

    (defun treemacs-kill ()
      (interactive)
      (dolist (buffer (buffer-list))
        (when (s-starts-with? treemacs--buffer-name-prefix (buffer-name buffer))
          (kill-buffer buffer)))))
  :bind
  (:map global-map
        ("C-c f 1"   . treemacs-delete-other-windows)
        ("C-c f t"   . treemacs)
        ("C-c f d"   . treemacs-select-directory)
        ("C-c f B"   . treemacs-bookmark)
        ("C-c f C-t" . treemacs-find-file)
        ("C-c f M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-perspective ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs perspective) ;;or perspective vs. persp-mode
  :config
  (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package neotree
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
    (let ((project-dir (ignore-errors (projectile-project-root)))
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
           (project-dir (ignore-errors
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
           (project-dir (ignore-errors
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
        (neotree-show-project-root-dir-or-find-file-in-project-dir-or-current-dir))))

  (defun neotree-kill ()
    (interactive)
    (when (and neo-global--buffer (buffer-name neo-global--buffer))
      (kill-buffer (buffer-name neo-global--buffer)))))

(defun treemacs-or-neotree-select-window ()
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-select-window)
    (treemacs-select-window)))

(defun neotree-or-treemacs-select-window ()
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-select-window)
    (pcase (treemacs-current-visibility)
      ('visible (treemacs--select-visible-window))
      (code (neotree-select-window)))))

(use-package dir-treeview
  :custom-face
  (dir-treeview-control-mouse-face ((t (:foreground "#282a36" :background "#bd93f9"))))
  (dir-treeview-label-mouse-face ((t (:foreground "#282a36" :background "#50fa7b"))))
  (dir-treeview-start-dir-face ((t (:foreground "#282a36" :background "#ff79c6"))))
  (dir-treeview-select-face ((t (:background "#6272a4"))))
  :config
  (setq dir-treeview-control-keymap
        '(("<mouse-1>" . treeview-toggle-node-state-at-event)
          ("<mouse-2>" . treeview-toggle-node-state-at-event)
          ("<mouse-3>" . dir-treeview-popup-node-menu-at-mouse)
          ("RET" . treeview-toggle-node-state-at-point)
          ("SPC" . dir-treeview-open-file-at-point)
          ("e" . dir-treeview-popup-node-menu-at-point)))
  (setq dir-treeview-label-keymap
        '(("<mouse-1>" . treeview-toggle-node-state-at-event)
          ("<mouse-2>" . dir-treeview-open-file-at-event)
          ("<mouse-3>" . dir-treeview-popup-node-menu-at-mouse)
          ("RET" . treeview-toggle-node-state-at-point)
          ("SPC" . dir-treeview-open-file-at-point)
          ("e" . dir-treeview-popup-node-menu-at-point)
          ("<C-down-mouse-1>" . ignore)
          ("<C-mouse-1>" . treeview-toggle-select-node-at-event)
          ("<S-down-mouse-1>" . ignore)
          ("<S-mouse-1>" . treeview-select-gap-above-node-at-event)))
  (setq dir-treeview-parent-dir-control-keymap
        '(("<mouse-1>" . dir-treeview-to-parent-dir)
          ("<mouse-2>" . dir-treeview-to-parent-dir)
          ("RET" . dir-treeview-to-parent-dir))))

(use-package dir-treeview-themes)

(use-package ztree)

;;==============================================================================
;; Bufler
;;
;; https://github.com/alphapapa/bufler.el
;;==============================================================================

(use-package bufler
  :load-path "bufler"
  :config
  (setq bufler-show-header-string nil
        bufler-use-header-line-format nil
        bufler-delete-bufler-window-when-switching-to-buffer t)
  (setq bufler-filter-buffer-modes
        '(bufler-list-mode special-mode timer-list-mode))

  (defun bufler-refresh-when-visible (frame)
    (when (string-prefix-p "*Bufler" (buffer-name (window-buffer (selected-window))))
      (bufler-list)))
  (add-hook 'window-buffer-change-functions 'bufler-refresh-when-visible)

  (add-hook 'bufler-list-mode-hook (lambda ()
                                     (let ((buffer (current-buffer)))
                                       (when (string-prefix-p "*Bufler" (buffer-name buffer))
                                         (setq mode-line-format nil)
                                         (buffer-focus-out-callback 'bufler-sidebar-close buffer))))))

;;==============================================================================
;; Code Style
;;==============================================================================

(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)

(use-package google-c-style
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
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;;==============================================================================
;; highlight-indentation
;;==============================================================================

(use-package highlight-indentation)

;;==============================================================================
;; filldent
;;
;; https://github.com/duckwork/filldent.el
;;==============================================================================

(use-package filldent)

;;==============================================================================
;; gnu-indent
;;
;; https://codeberg.org/akib/emacs-gnu-indent
;;==============================================================================

(use-package gnu-indent
  :config
  (if (string-equal system-type "darwin")
      (setq gnu-indent-program "gindent")))

;;==============================================================================
;; rainbow-delimiters
;;
;; https://github.com/Fanael/rainbow-delimiters
;;==============================================================================

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

;;==============================================================================
;; tree-sitter
;;
;; https://emacs-tree-sitter.github.io/
;; https://tree-sitter.github.io/tree-sitter/
;; https://github.com/tree-sitter/tree-sitter
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;;==============================================================================

;;(setq treesit-language-source-alist
;;      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;        (cmake "https://github.com/uyha/tree-sitter-cmake")
;;        (css "https://github.com/tree-sitter/tree-sitter-css")
;;        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;        (c "https://github.com/tree-sitter/tree-sitter-c")
;;        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;        (go "https://github.com/tree-sitter/tree-sitter-go")
;;        (html "https://github.com/tree-sitter/tree-sitter-html")
;;        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;        (json "https://github.com/tree-sitter/tree-sitter-json")
;;        (make "https://github.com/alemuller/tree-sitter-make")
;;        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;        (python "https://github.com/tree-sitter/tree-sitter-python")
;;        (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;;        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
;;
;;(setq major-mode-remap-alist
;;      '((yaml-mode . yaml-ts-mode)
;;        (bash-mode . bash-ts-mode)
;;        (js2-mode . js-ts-mode)
;;        (typescript-mode . typescript-ts-mode)
;;        (json-mode . json-ts-mode)
;;        (css-mode . css-ts-mode)
;;        (python-mode . python-mode)))

(use-package tree-sitter
  :hook
  ((c-mode
    c++-mode
    python-mode
    css-mode
    js-mode
    json-mode
    php-mode
    ruby-mode
    rust-mode
    sh-mode
    terraform-mode
    typescript-mode
    yaml-mode) . daft-tree-sitter-mode-enable)
  :preface
  (defun daft-tree-sitter-mode-enable ()
    (tree-sitter-mode t))
  :defer t)

(use-package tree-sitter-langs
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode))

;;==============================================================================
;; cc-search-directories
;;==============================================================================

(setq cc-search-directories '("." "../include" "/usr/include" "/usr/local/include/*"
                              "/System/Library/Frameworks" "/Library/Frameworks"))

;;==============================================================================
;; Language Server Protocol (LSP)
;;
;; https://github.com/emacs-lsp/lsp-mode
;; https://github.com/Microsoft/language-server-protocol/
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;;==============================================================================

(setenv "LSP_USE_PLISTS" "true")

(use-package lsp-mode
  :init
  (message "LSP_USE_PLISTS: %s" (getenv "LSP_USE_PLISTS"))
  (setq lsp-use-plists t)
  ;;(setq lsp-enable-file-watchers nil)
  (setq lsp-file-watch-threshold 2000)
  (setq lsp-idle-delay 0.5)
  :hook ((lsp-mode . (lambda ()
                       (lsp-enable-which-key-integration)
                       (define-key lsp-mode-map "\M-." 'lsp-find-definition)
                       (define-key lsp-mode-map (kbd "s-l 0") 'lsp-treemacs-symbols))))
  :config
  (setq lsp-file-watch-ignored
        (append lsp-file-watch-ignored
                '("[/\\\\]\\.cache$"
                  "[/\\\\]\\.libs$")))
  ;;(message "lsp-file-watch-ignored: %s" lsp-file-watch-ignored)
  )

(use-package lsp-ui)

(use-package helm-lsp)

(use-package lsp-ivy)

(use-package lsp-treemacs)

;;==============================================================================
;; Debug Adapter Protocol (DAP)
;;
;; https://github.com/emacs-lsp/dap-mode
;; https://microsoft.github.io/debug-adapter-protocol/
;;==============================================================================

(use-package dap-mode)

;;==============================================================================
;; C/C++
;;
;; - bear
;; https://github.com/rizsotto/Bear
;;
;; 1) Installing
;;   Ubuntu: $ sudo apt-get install bear
;;   MacOS brew: $ brew install bear
;;
;; 2) Usage
;;   Ubuntu: $ bear make
;;   MacOS brew: $ bear -- make
;;
;; - clangd
;; https://clangd.llvm.org/
;;
;; 1) Installing
;;   Ubuntu: $ sudo apt-get install clangd-12
;;   MacOS brew: $ brew install llvm
;;
;; - ccls
;; https://github.com/MaskRay/ccls
;; https://github.com/MaskRay/emacs-ccls
;;
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

(add-hook 'c-mode-hook 'lsp-deferred)
(add-hook 'c++-mode-hook 'lsp-deferred)

;;(use-package ccls
;;  :hook ((c-mode c++-mode objc-mode) . (lambda () (require 'ccls) (lsp-deferred)))
;;  :config
;;  (setq ccls-executable "~/.emacs.d/ccls/Release/ccls")
;;  ;;TODO
;;  ;;(setq
;;  ;; ccls-initialization-options
;;  ;; `(:index (:multiVersion 1 :trackDependency 1)))
;;
;;  ;; # Indentation for ccls
;;  ;;
;;  ;; https://github.com/MaskRay/ccls/issues/459
;;  ;; https://clang.llvm.org/docs/ClangFormatStyleOptions.html
;;  ;;
;;  ;; 1) Place .clang-format in the project root, e.g.
;;  ;;
;;  ;; $ emacs .clang-format
;;  ;; BasedOnStyle: LLVM
;;  ;; IndentWidth: 4
;;  ;;
;;  ;; 2) If you don't want automatic type formatting, set lsp-enable-on-type-formatting to nil as follows.
;;  ;;
;;  ;; (setq lsp-enable-on-type-formatting nil)
;;  )

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

(defun ff-get-file-name--ff-get-file-name-framework (orig-fun search-dirs fname-stub &rest args)
  "Search for Mac framework headers as well as POSIX headers."
  (or
   (if (string-match "\\(.*?\\)/\\(.*\\)" fname-stub)
       (let* ((framework (match-string 1 fname-stub))
              (header (match-string 2 fname-stub))
              (fname-stub (concat framework ".framework/Headers/" header)))
         (apply orig-fun search-dirs fname-stub args)))
   (apply orig-fun search-dirs fname-stub args)))
(advice-add 'ff-get-file-name :around #'ff-get-file-name--ff-get-file-name-framework)

;; for lsp-mode
(add-hook 'objc-mode-hook 'lsp-deferred)

;; objc-font-lock
(use-package objc-font-lock)

;;==============================================================================
;; Swift
;;
;; https://github.com/swift-emacs/swift-mode
;;==============================================================================

(use-package swift-mode)

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
;; - lsp-pyright server installation (https://github.com/emacs-lsp/lsp-pyright)
;;   $ npm install -g pyright
;;==============================================================================

(setq python-shell-interpreter "python3"
      python-shell-completion-native-enable nil)

(use-package python
  :mode ("\\.py" . python-mode))

(use-package pip-requirements
  ;;:config
  ;;(add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup)
  )

(use-package py-autopep8
  ;;:config
  ;;(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  )

(use-package epc)

(use-package importmagic
  :config
  (add-hook 'python-mode-hook 'importmagic-mode)
  (setq importmagic-python-interpreter "~/miniconda3/bin/python3"))

(use-package pyvenv
  :config
  (cond
   ((file-directory-p "~/miniconda3/envs")
    (setenv "WORKON_HOME" "~/miniconda3/envs"))
   ((file-directory-p "~/opt/miniconda3/envs")
    (setenv "WORKON_HOME" "~/opt/miniconda3/envs")))
  (pyvenv-mode 1))

(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))
  :config
  (add-hook 'pyvenv-post-activate-hooks (lambda () (lsp-restart-workspace)))
  (add-hook 'pyvenv-post-deactivate-hooks (lambda () (lsp-restart-workspace))))

;;(use-package elpy
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
  :custom-face
  (ein:basecell-input-area-face ((t (:extend t :background "#23242f"))))
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
;; Javascript/Typescript
;;
;; - lsp-typescript server installation
;;   M-x lsp-install-server RET ts-ls RET
;;    or
;;   npm i -g typescript-language-server; npm i -g typescript
;;
;; - vscode-json-languageserver installation
;;   M-x lsp-install-server RET json-ls RET
;;    or
;;   npm i -g vscode-json-languageserver
;;
;;   https://emacs-lsp.github.io/lsp-mode/page/lsp-typescript/
;;   https://github.com/typescript-language-server/typescript-language-server
;;==============================================================================

;; https://github.com/emacs-typescript/typescript.el
(use-package typescript-mode)

(add-hook 'js-mode-hook (lambda () (lsp-deferred)))
(add-hook 'typescript-mode-hook (lambda () (lsp-deferred)))

;;==============================================================================
;; haskell-mode
;;==============================================================================

(use-package haskell-mode)

;;==============================================================================
;; lua-mode
;;==============================================================================

(use-package lua-mode
  :config
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

;;==============================================================================
;; cuda-mode
;;==============================================================================

(use-package cuda-mode)

;;==============================================================================
;; JSON
;;==============================================================================

(use-package json-mode)

;; https://github.com/sterlingg/json-snatcher
(use-package json-snatcher
  :config
  (defun js-mode-bindings ()
    "Sets a hotkey for using the json-snatcher plugin"
    (when (string-match  "\\.json$" (buffer-name))
      (local-set-key (kbd "C-c C-g") 'jsons-print-path)))
  (add-hook 'js-mode-hook 'js-mode-bindings)
  (add-hook 'js2-mode-hook 'js-mode-bindings))

(use-package json-reformat)

;;==============================================================================
;; yaml-mode
;;==============================================================================

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook
            #'(lambda ()
                (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;;==============================================================================
;; qml-mode
;;==============================================================================

(use-package qml-mode
  :config
  (autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
  (add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode)))

;;==============================================================================
;; cmake-mode
;;==============================================================================

(use-package cmake-mode)

;;==============================================================================
;; i3wm-config-mode
;;==============================================================================

(use-package i3wm-config-mode)

;;==============================================================================
;; ligature
;;
;; https://github.com/mickeynp/ligature.el
;;==============================================================================

(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;;==============================================================================
;; Docker 🐳
;;==============================================================================

;; https://github.com/Silex/docker.el
(use-package docker)

;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;; https://github.com/meqif/docker-compose-mode
(use-package docker-compose-mode)

;;==============================================================================
;; Graphviz
;;
;; https://graphviz.org/
;;==============================================================================

;; https://github.com/ppareit/graphviz-dot-mode
(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4)
  (add-hook 'graphviz-dot-mode-hook 'company-mode))

;;==============================================================================
;; focus
;;==============================================================================

(use-package focus)

;;==============================================================================
;; rich-minority
;;==============================================================================

(use-package rich-minority)

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
;; vdiff
;;
;; https://github.com/justbur/emacs-vdiff
;; https://github.com/justbur/emacs-vdiff-magit
;;==============================================================================

(use-package vdiff
  :config
  (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)

  ;; Whether to lock scrolling by default when starting vdiff
  (setq vdiff-lock-scrolling t)

  ;; diff program/algorithm to use. Allows choice of diff or git diff along with
  ;; the various algorithms provided by these commands. See
  ;; `vdiff-diff-algorithms' for the associated command line arguments.
  (setq vdiff-diff-algorithm 'git-diff)

  ;; diff3 command to use. Specify as a list where the car is the command to use
  ;; and the remaining elements are the arguments to the command.
  (setq vdiff-diff3-command '("diff3"))

  ;; Don't use folding in vdiff buffers if non-nil.
  (setq vdiff-disable-folding t)

  ;; Unchanged lines to leave unfolded around a fold
  (setq vdiff-fold-padding 6)

  ;; Minimum number of lines to fold
  (setq vdiff-min-fold-size 4)

  ;; If non-nil, allow closing new folds around point after updates.
  (setq vdiff-may-close-fold-on-point t)

  ;; Function that returns the string printed for a closed fold. The arguments
  ;; passed are the number of lines folded, the text on the first line, and the
  ;; width of the buffer.
  (setq vdiff-fold-string-function 'vdiff-fold-string-default)

  ;; Default syntax table class code to use for identifying "words" in
  ;; `vdiff-refine-this-change'. Some useful options are
  ;;
  ;; "w"   (default) words
  ;; "w_"  symbols (words plus symbol constituents)
  ;;
  ;; For more information see
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html
  (setq vdiff-default-refinement-syntax-code "w")

  ;; If non-nil, automatically refine all hunks.
  (setq vdiff-auto-refine t)

  ;; How to represent subtractions (i.e., deleted lines). The
  ;; default is full which means add the same number of (fake) lines
  ;; as those that were removed. The choice single means add only one
  ;; fake line. The choice fringe means don't add lines but do
  ;; indicate the subtraction location in the fringe.
  (setq vdiff-subtraction-style 'full)

  ;; Character to use for filling subtraction lines. See also
  ;; `vdiff-subtraction-style'.
  (setq vdiff-subtraction-fill-char ?-))

(use-package vdiff-magit
  :config
  (define-key magit-mode-map "e" 'vdiff-magit-dwim)
  (define-key magit-mode-map "E" 'vdiff-magit)
  (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
  (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
  (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
  (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit)

  ;; This flag will default to using ediff for merges.
  ;; (setq vdiff-magit-use-ediff-for-merges nil)

  ;; Whether vdiff-magit-dwim runs show variants on hunks.  If non-nil,
  ;; vdiff-magit-show-staged or vdiff-magit-show-unstaged are called based on what
  ;; section the hunk is in.  Otherwise, vdiff-magit-dwim runs vdiff-magit-stage
  ;; when point is on an uncommitted hunk.
  ;; (setq vdiff-magit-dwim-show-on-hunks nil)

  ;; Whether vdiff-magit-show-stash shows the state of the index.
  ;; (setq vdiff-magit-show-stash-with-index t)

  ;; Only use two buffers (working file and index) for vdiff-magit-stage
  ;; (setq vdiff-magit-stage-is-2way nil)
  )

;;==============================================================================
;; diff-hl
;;
;; diff-hl-mode highlights uncommitted changes on the left side of the window,
;; allows you to jump between and revert them selectively.
;;
;; https://github.com/dgutov/diff-hl
;;==============================================================================

(use-package diff-hl
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
;; pdf-tools
;;==============================================================================

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (add-hook 'pdf-tools-enabled-hook (lambda ()
                                      (pdf-view-fit-page-to-window)
                                      (pdf-isearch-batch-mode))))

;;==============================================================================
;; nov - EPUB file reader
;;
;; https://depp.brause.cc/nov.el/
;;==============================================================================

(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-text-width t)
  (defun daftemacs-nov-mode-hook ()
    (face-remap-add-relative 'variable-pitch :family "IBM 3270" :height 2.0)
    (visual-line-mode t))
  (add-hook 'nov-mode-hook 'daftemacs-nov-mode-hook))

;;==============================================================================
;; vterm
;;
;; - Install libvterm for MacOS
;;   $ brew install libvterm
;;
;; https://github.com/akermu/emacs-libvterm
;; https://github.com/suonlight/multi-vterm
;;==============================================================================

(use-package vterm
  :config
  (setq vterm-max-scrollback 1000000)

  (defun vterm-clear-all ()
    (interactive)
    (vterm-clear)
    (vterm-clear-scrollback))

  (add-hook 'vterm-mode-hook (lambda ()
                               (define-key vterm-mode-map (kbd "M-0") 'treemacs-select-window)
                               (define-key vterm-mode-map (kbd "M-]") 'centaur-tabs-forward)
                               (define-key vterm-mode-map (kbd "C-S-k") 'vterm-clear-all)

                               (setq-local global-hl-line-mode nil))))

(use-package multi-vterm
  :config
  (setq multi-vterm-dedicated-window-height 40)

  (defvar multi-vterm-dedicated-buffer-lock-state nil)

  (defun multi-vterm-dedicated-buffer-lock-toggle ()
    (interactive)
    (if multi-vterm-dedicated-buffer-lock-state
        (progn
          (hide-mode-line-mode)
          (setq multi-vterm-dedicated-buffer-lock-state nil)
          (message "multi-vterm-dedicated-buffer UNLOCKED."))
      (progn
        (hide-mode-line-mode -1)
        (redraw-frame)
        (setq multi-vterm-dedicated-buffer-lock-state t)
        (message "multi-vterm-dedicated-buffer LOCKED."))))

  (defun multi-vterm-dedicated--buffer-focus-out ()
    (if (and (multi-vterm-dedicated-exist-p) (not multi-vterm-dedicated-buffer-lock-state))
        (multi-vterm-dedicated-close)))

  (add-hook 'vterm-mode-hook (lambda ()
                               (define-key vterm-mode-map (kbd "C-c r") 'multi-vterm-rename-buffer)
                               (define-key vterm-mode-map (kbd "C-c l") 'multi-vterm-dedicated-buffer-lock-toggle)

                               (let ((buffer (current-buffer)))
                                 (when (string-prefix-p "*vterminal - dedicated" (buffer-name buffer))
                                   (hide-mode-line-mode)
                                   (buffer-focus-out-callback 'multi-vterm-dedicated--buffer-focus-out buffer)))))

  (defun multi-vterm-dedicated--change-height-on-frame-size-change (&optional _)
    (when (multi-vterm-dedicated-exist-p)
      (setq multi-vterm-dedicated-window-height (multi-vterm-current-window-height multi-vterm-dedicated-window))))

  (add-hook 'window-size-change-functions
            #'multi-vterm-dedicated--change-height-on-frame-size-change))

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

(defun resize-window-and-margin (&optional arg)
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
           "h=heighten, s=shrink, w=widen, n=narrow, b=balance, +=widen margin, -=narrow margin | 1-9=unit(%d), q=quit"
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

(defun resize-window (&optional arg)
  "*Resize window interactively."
  (interactive "p")
  (if (one-window-p)
      (error "Cannot resize sole window"))
  (setq arg 4)
  (let (c)
    (catch 'done
      (while t
        (message
         "h=heighten, s=shrink, w=widen, n=narrow, b=balance | 1-9=unit(%d), q=quit"
         arg)
        (setq c (read-char))
        (condition-case ()
            (cond
             ((= c ?h) (enlarge-window arg))
             ((= c ?s) (shrink-window arg))
             ((= c ?w) (enlarge-window-horizontally arg))
             ((= c ?n) (shrink-window-horizontally arg))
             ((= c ?b) (balance-windows))
             ((= c ?\^G) (keyboard-quit))
             ((= c ?q) (throw 'done t))
             ((and (> c ?0) (<= c ?9)) (setq arg (- c ?0)))
             (t (beep)))
          (error (beep))))))
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
  (set (make-variable-buffer-local 'global-hl-line-mode) nil))

(defun daftemacs-find-file-check-if-very-large-file-hook ()
  "If a file is over 2MB, turn off modes of the buffer that make it slow."
  (when (> (buffer-size) (* 2 1024 1024))
    (disable-slow-modes)))
(add-hook 'find-file-hook 'daftemacs-find-file-check-if-very-large-file-hook)

;;==============================================================================
;; vlf
;;
;; https://github.com/m00natic/vlfi
;;==============================================================================

(use-package vlf)

;;==============================================================================
;; keyfreq
;;==============================================================================

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;==============================================================================
;; imenu-list
;;==============================================================================

(use-package imenu-list)

;;==============================================================================
;; shrink-path
;;==============================================================================

(use-package shrink-path)

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

(when (string-equal system-type "gnu/linux")
  (use-package neato-graph-bar))

;;==============================================================================
;; proced
;;==============================================================================

(use-package proced-narrow)

(setq proced-tree-flag t)

(add-hook 'proced-mode-hook
          (lambda ()
            (local-set-key (kbd "N") #'proced-narrow)
            (local-set-key (kbd "!") #'proced-update)))

;;==============================================================================
;; disk-usage
;;
;; https://gitlab.com/ambrevar/emacs-disk-usage
;;==============================================================================

(use-package disk-usage)

;;==============================================================================
;; go-translate
;;
;; https://github.com/lorniu/go-translate
;; https://www.emacswiki.org/emacs/GoTranslate
;;==============================================================================

(use-package go-translate
  :config
  ;; your languages pair used to translate
  (setq gts-translate-list '(("en" "ko") ("ko" "en")))

  ;; config the default translator, it will be used by command gts-do-translate
  (setq gts-default-translator
        (gts-translator

         :picker ; used to pick source text, from, to. choose one.

         ;;(gts-noprompt-picker)
         ;;(gts-noprompt-picker :texter (gts-whole-buffer-texter))
         (gts-prompt-picker)
         ;;(gts-prompt-picker :single t)
         ;;(gts-prompt-picker :texter (gts-current-or-selection-texter) :single t)

         :engines ; engines, one or more. Provide a parser to give different output.

         (list
          (gts-bing-engine)
          ;;(gts-google-engine)
          ;;(gts-google-rpc-engine)
          ;;(gts-deepl-engine :auth-key [YOUR_AUTH_KEY] :pro nil)
          (gts-google-engine :parser (gts-google-summary-parser))
          (gts-google-engine :parser (gts-google-parser))
          ;;(gts-google-rpc-engine :parser (gts-google-rpc-summary-parser) :url "https://translate.google.com")
          ;;(gts-google-rpc-engine :parser (gts-google-rpc-parser) :url "https://translate.google.com")
          )

         :render ; render, only one, used to consumer the output result. Install posframe yourself when use gts-posframe-xxx

         (gts-buffer-render)
         ;;(gts-posframe-pop-render)
         ;;(gts-posframe-pop-render :backcolor "#333333" :forecolor "#ffffff")
         ;;(gts-posframe-pin-render)
         ;;(gts-posframe-pin-render :position (cons 1200 20))
         ;;(gts-posframe-pin-render :width 80 :height 25 :position (cons 1000 20) :forecolor "#ffffff" :backcolor "#111111")
         ;;(gts-kill-ring-render)

         :splitter ; optional, used to split text into several parts, and the translation result will be a list.

         (gts-paragraph-splitter)
         )))

;;==============================================================================
;; mpv
;;==============================================================================

(use-package mpv)

;;==============================================================================
;; yeetube
;;
;; https://git.sr.ht/~thanosapollo/yeetube.el
;; https://invidious.io/
;;==============================================================================

(use-package yeetube
  :config
  (setq yeetube-results-limit 50)

  ;; Overriding 'yeetube-play' in 'yeetube.el'
  (defun yeetube-play ()
    "Open the url at point in an `'org-mode buffer using 'yeetube-player'."
    (interactive)
    (let ((url (org-element-property
                :raw-link (org-element-context))))
      (if (string-match "mpv" yeetube-player)
          (shell-command (format "pkill -9 -f mpv"))
        (shell-command (format "pkill -9 -f %s" (shell-quote-argument yeetube-player))))
      (when (string-prefix-p "http" url)
        (call-process-shell-command
         (format "%s '%s'" yeetube-player url) nil 0)
        (message "Opening with %s" yeetube-player)))
    (yeetube--get-title)))

;;==============================================================================
;; elfeed
;;
;; https://github.com/skeeto/elfeed
;; https://github.com/karthink/elfeed-tube
;;==============================================================================

(use-package elfeed
  :config
  (setq elfeed-feeds
        '(;; Blog
          "https://darkpgmr.tistory.com/rss"
          "http://nullprogram.com/feed/"
          ("https://planet.emacslife.com/atom.xml" emacs)
          "http://arxiv.org/rss/cs.CV"
          ;; YouTube
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCsJ6RuBiTVWRX156FVbeaGg"
          ))

  (setq elfeed-show-mode-hook
        (lambda ()
          (cond
           ((string-equal system-type "gnu/linux")
            (set-face-attribute 'variable-pitch (selected-frame) :font (font-spec :family "JetBrains Mono" :size 25)))
           ((string-equal system-type "darwin")
            (set-face-attribute 'variable-pitch (selected-frame) :font (font-spec :family "JetBrains Mono" :size 12))))
          (setq fill-column 120)
          (setq elfeed-show-entry-switch #'daftemacs-show-elfeed)))

  (defun daftemacs-show-elfeed (buffer)
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (fill-individual-paragraphs (point) (point-max))
      (setq buffer-read-only t))
    (switch-to-buffer buffer)))

(use-package elfeed-tube
  :after elfeed
  :demand t
  :config
  ;;(setq elfeed-tube-auto-save-p nil) ; default value
  ;;(setq elfeed-tube-auto-fetch-p t)  ; default value
  (elfeed-tube-setup)
  :bind (:map elfeed-show-mode-map
              ("F" . elfeed-tube-fetch)
              ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
              ("F" . elfeed-tube-fetch)
              ([remap save-buffer] . elfeed-tube-save)))

(use-package elfeed-tube-mpv
  :config
  (elfeed-tube-mpv-follow-mode)
  :bind (:map elfeed-show-mode-map
              ("C-c C-f" . elfeed-tube-mpv-follow-mode)
              ("C-c C-c" . elfeed-tube-mpv)
              ("C-c C-w" . elfeed-tube-mpv-where)))

;;==============================================================================
;; md4rd - Mode for reddit
;;
;; https://github.com/ahungry/md4rd
;;==============================================================================

(use-package md4rd
  :config
  (add-hook 'md4rd-mode-hook 'md4rd-indent-all-the-lines)
  (setq md4rd--oauth-client-id "daftcoder")
  (setq md4rd--oauth-redirect-uri "http://localhost:12345")
  ;;(setq md4rd--oauth-access-token "44428323-ci7Q1lW1XZadFiMIYPdQu2Xdj-asyw")
  ;;(setq md4rd--oauth-refresh-token "44428323-vwOa5w774LmcjoMcy7IxTySny2aXBw")
  ;;(message "md4rd--oauth-access-token: %s" md4rd--oauth-access-token)
  ;;(message "md4rd--oauth-refresh-token: %s" md4rd--oauth-refresh-token)
  ;;(run-with-timer 0 3540 'md4rd-refresh-login)
  (setq md4rd-subs-active '(emacs lisp+Common_Lisp prolog clojure rust)))

;;==============================================================================
;; devdocs.io
;;
;; devdocs: https://github.com/astoff/devdocs.el
;; devdocs-browser: https://github.com/blahgeek/emacs-devdocs-browser
;;==============================================================================

(use-package devdocs)
(use-package devdocs-browser)

;;==============================================================================
;; arxiv-mode
;;
;; https://github.com/fizban007/arxiv-mode
;;==============================================================================

(use-package arxiv-mode)

;;==============================================================================
;; arxiv-citation
;;
;; https://gitlab.com/slotThe/arxiv-citation
;;==============================================================================

(use-package arxiv-citation
  :commands (arxiv-citation-elfeed arxiv-citation-gui)
  :custom
  (arxiv-citation-library "~/Documents")
  (arxiv-citation-bibtex-files '("~/Documents/bibliography.bib"))
  (arxiv-citation-open-pdf-function #'browse-url-emacs))

;;==============================================================================
;; Instant Stackoverflow Solutions
;;==============================================================================

(load-file "~/.emacs.d/stackoverflow.elc")
(require 'stackoverflow)

;;==============================================================================
;; stock-tracker
;;==============================================================================

(load-file "~/.emacs.d/stock-tracker.elc")
(require 'stock-tracker)

;; Refresh stock price every 5*10 secs
(customize-set-variable 'stock-tracker-refresh-interval 1)

;; Set up as green, down as red
(customize-set-variable 'stock-tracker-up-red-down-green nil)

;;==============================================================================
;; Wordel: Wordle in Emacs
;;
;; https://github.com/progfolio/wordel
;;==============================================================================

(use-package wordel)

;;==============================================================================
;; Global Keys
;;==============================================================================

(global-set-key (kbd "S-SPC") 'toggle-input-method)
(global-set-key (kbd "M-SPC") 'toggle-input-method) ;; 'Shift + Space' is recognized as 'M-SPC' on MacOS.

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x C-b") 'bufler-sidebar)
(global-set-key (kbd "C-x C-S-b") 'persp-ibuffer)
(global-set-key (kbd "C-x b") 'persp-counsel-switch-buffer)
(global-set-key (kbd "C-x k") 'persp-kill-buffer*)
(global-set-key (kbd "C-x U") 'vundo)
(global-set-key (kbd "C-x _") 'whitespace-mode)

(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "M-O") 'ace-select-previous-window)
(global-set-key (kbd "C-M-o") 'ace-swap-window)
(global-set-key (kbd "C-c o") 'ff-find-other-file)
(global-set-key (kbd "M-m") 'lsp-ui-imenu)
(global-set-key (kbd "M-M") 'imenu-list)
(global-set-key (kbd "M-0") 'treemacs-or-neotree-select-window)
(global-set-key (kbd "M-q") 'filldent-dwim)

(global-set-key (kbd "C-S-<up>") 'buf-move-up)
(global-set-key (kbd "C-S-<down>") 'buf-move-down)
(global-set-key (kbd "C-S-<left>") 'buf-move-left)
(global-set-key (kbd "C-S-<right>") 'buf-move-right)

(global-set-key (kbd "C-c m") 'magit)
(global-set-key (kbd "C-c 9") #'(lambda ()
                                  (interactive)
                                  (neotree-hide)
                                  (treemacs-select-window)))
(global-set-key (kbd "C-c 0") 'neotree-select-window)
(global-set-key (kbd "C-c s") 'swiper)
(global-set-key (kbd "C-c C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-c C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-c S") 'find-file-in-project)

(global-set-key (kbd "C-c b v") 'visual-line-mode)
(global-set-key (kbd "C-c b f") 'focus-mode)

(global-set-key (kbd "C-c p l") 'persp-state-load-default)

(global-set-key (kbd "C-c w c") 'centered-window-mode)
(global-set-key (kbd "C-c w l") 'centered-cursor-mode)
(global-set-key (kbd "C-c w s") 'resize-window)
(global-set-key (kbd "C-c w k") 'kill-buffer-and-window)

(global-set-key (kbd "C-c c r") 'counsel-register)
(global-set-key (kbd "C-c c e") 'counsel-recentf)
(global-set-key (kbd "C-c c s g") 'counsel-grep)
(global-set-key (kbd "C-c c s r") 'counsel-rg)
(global-set-key (kbd "C-c c s s") 'counsel-ag)

(defun open-dedicated-terminal ()
  (interactive)
  (multi-vterm-dedicated-toggle))

(defun open-terminal ()
  (interactive)
  (multi-vterm)
  ;;(eshell)
  ;;(ansi-term explicit-shell-file-name)
  )

(defun open-calculator ()
  (interactive)
  (calculator))

(add-hook 'calculator-mode-hook #'(lambda ()
                                    (let ((window (selected-window)))
                                      (if (< (window-height window) 3)
                                          (window-resize window 1)))))

(global-set-key (kbd "C-c u m") 'minimap-mode)
(global-set-key (kbd "C-c u t") 'open-dedicated-terminal)
(global-set-key (kbd "C-c u T") 'open-terminal)
(global-set-key (kbd "C-c u d") 'dirvish)
(global-set-key (kbd "C-c u D") 'dir-treeview-open)
(global-set-key (kbd "C-c u k") 'docker)
(global-set-key (kbd "C-c u c") 'open-calculator)
(global-set-key (kbd "C-c u l") 'insecure-lock-enter)
(when (string-equal system-type "gnu/linux")
  (global-set-key (kbd "C-c u p") 'neato-graph-bar))
(global-set-key (kbd "C-c u P") 'proced)
(global-set-key (kbd "C-c u u") 'disk-usage)
(global-set-key (kbd "C-c u g") 'gts-do-translate)
(global-set-key (kbd "C-c u f") 'elfeed)
(global-set-key (kbd "C-c u r") 'md4rd)
(global-set-key (kbd "C-c u a") 'arxiv-complex-search)
(global-set-key (kbd "C-c u o") 'stackoverflow-lookup)
(global-set-key (kbd "C-c u y") 'yeetube-search)
(global-set-key (kbd "C-c u s") 'stock-tracker-start)

(defun kill-side-windows ()
  (interactive)
  (treemacs-kill)
  (neotree-kill)
  (bufler-sidebar-close)
  (when (get-buffer-window "*Ilist*")
    (delete-window (get-buffer-window "*Ilist*")))
  (when (get-buffer "*lsp-ui-imenu*")
    (kill-buffer "*lsp-ui-imenu*"))
  (when (get-buffer minimap-buffer-name)
    (kill-buffer minimap-buffer-name))
  (when (get-buffer " *command-log*")
    (kill-buffer " *command-log*"))
  (when (multi-vterm-dedicated-exist-p)
    (multi-vterm-dedicated-close)))

(global-set-key (kbd "ESC ESC") 'kill-side-windows)

(add-hook 'vterm-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c ESC ESC") 'kill-side-windows)))

;; helpful
;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

(global-set-key (kbd "C-h D") 'devdocs-browser-open)

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

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c d") #'shortdoc-display-group)
            (local-set-key (kbd "C-c C-d") #'helpful-at-point)))

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map "\C-ca" 'py-autopep8)
            (define-key python-mode-map "\C-ce" 'pyvenv-workon)
            (define-key python-mode-map "\C-m" 'newline-and-indent)))

;;==============================================================================
;; command-log-mode
;;
;; https://github.com/lewang/command-log-mode
;;==============================================================================

(use-package command-log-mode
  :init
  (setq command-log-mode-key-binding-open-log nil))
