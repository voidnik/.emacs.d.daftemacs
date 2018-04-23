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
(message "user-init-file: %s (last modified date: 2017-11-06)" user-init-file)
(message "user-emacs-directory: %s" user-emacs-directory)

;;==============================================================================
;; Packages
;;==============================================================================

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(message "package-archives: %s" package-archives)

;;==============================================================================
;; Custom Variables
;;==============================================================================

(defvar my-initial-buffer nil)
;(defvar my-initial-buffer (concat user-emacs-directory "/init.el"))
(defvar my-default-directory (concat (getenv "HOME") "/Workspace/"))

;;==============================================================================
;; Basic Customization
;;==============================================================================

(setq visible-bell t
      make-backup-files nil
      column-number-mode t
      split-height-threshold nil ; not to split this way.
      gdb-many-windows t)
(setq-default truncate-lines t)
(put 'erase-buffer 'disabled nil)
(show-paren-mode t)
(delete-selection-mode t)
(require 'cl) ; Common Lisp

(defun open-user-init-file ()
  (interactive)
  (find-file user-init-file))

(defun init-doom-theme ()
  (add-to-list 'load-path "~/.emacs.d/doom-themes-20180328.1556-mod/")
  (require 'doom-themes)
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  ;(load-theme 'doom-one t)
  ;(load-theme 'doom-city-lights t)
  ;(load-theme 'doom-molokai t)
  ;(load-theme 'doom-nord t)
  ;(load-theme 'doom-nova t)
  ;(load-theme 'doom-peacock t)
  ;(load-theme 'doom-solarized-light t)
  ;(load-theme 'doom-spacegrey t)
  (load-theme 'doom-tomorrow-night t)
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

(defun init-custom-theme ()
  (doom-themes-neotree-config)
  (init-zerodark-theme))

(defun init-themes ()
  ;(load-theme 'base16-default-dark t)
  (init-doom-theme)
  ;(init-zerodark-theme)
  ;(init-color-theme)
  ;(init-custom-theme)
  )

(defun init-font ()
  ;(print (font-family-list))
  (cond
   ((string-equal system-type "darwin")
    (progn
      (set-face-attribute 'default nil :height 115 :family "monospace")))
   ((string-equal system-type "gnu/linux")
    (progn
      ;(set-face-attribute 'default nil :height 95 :family "FreeMono")
      ;(set-face-attribute 'default nil :height 90 :family "monospace")
      (set-face-attribute 'default nil :height 98 :family "Ubuntu Mono")))))

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

(init-themes)
(if (display-graphic-p)
    (startup-on-gui)
  (startup-on-cui))

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
    ("bce3ae31774e626dce97ed6d7781b4c147c990e48a35baedf67e185ebc544a56" default)))
 '(inhibit-startup-screen t)
 '(initial-buffer-choice my-initial-buffer)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (neotree zerodark-theme company flycheck magit vlf base16-theme flx-isearch flx-ido flx projectile dark-souls haskell-mode pdf-tools))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;==============================================================================
;; exec-path
;;==============================================================================

(cond
 ((string-equal system-type "darwin")
  (progn
    (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
    (setq exec-path (append exec-path '("/usr/local/bin")))
    (setenv "PATH" (concat (getenv "PATH") ":/Users/daftcoder/Library/Android/sdk/platform-tools"))
    (setq exec-path (append exec-path '("/Users/daftcoder/Library/Android/sdk/platform-tools")))
    (setenv "PATH" (concat (getenv "PATH") ":/Users/daftcoder/Workspace/trtc/depot_tools"))
    (setq exec-path (append exec-path '("/Users/daftcoder/Workspace/trtc/depot_tools")))))
 ((string-equal system-type "gnu/linux")
  (progn
    (setenv "PATH" (concat (getenv "PATH") ":/home/daftcoder/Workspace/depot_tools"))
    (setq exec-path (append exec-path '("/home/daftcoder/Workspace/depot_tools"))))))

;;==============================================================================
;; projectile
;;==============================================================================

(projectile-global-mode)
(setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)

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
;; https://github.com/m00natic/vlfi
;;==============================================================================

(require 'vlf-setup)

;;==============================================================================
;; flycheck
;;==============================================================================

(add-hook 'after-init-hook #'global-flycheck-mode)

;;==============================================================================
;; company
;;==============================================================================

(add-hook 'after-init-hook 'global-company-mode)

;;==============================================================================
;; neotree
;;==============================================================================

(setq neo-window-width window-min-width)
;(setq neo-window-position 'right)
(setq neo-window-fixed-size nil)
(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

;;==============================================================================
;; pdf-tools
;;==============================================================================

(pdf-tools-install)

;;==============================================================================
;; ediff
;;==============================================================================

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

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
  :group 'linum
  )
(defcustom linum-disable-starred-buffers 't
  "* Disable buffers that have stars in them like *Gnu Emacs*"
  :type 'boolean
  :group 'linum)

(defun linum-on ()
  "* When linum is running globally, disable line number in modes defined in `linum-disabled-modes-list'. Changed by linum-off. Also turns off numbering in starred modes like *scratch*"

  (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)
	      (and linum-disable-starred-buffers (string-match "*" (buffer-name)))
	      )
    (linum-mode 1)))

;;==============================================================================
;; buffer-move
;;==============================================================================

(load-file "~/.emacs.d/buffer-move.elc")

;;==============================================================================
;; Killing Buffers
;;
;; Link - https://www.emacswiki.org/emacs/KillingBuffers
;;==============================================================================

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;;==============================================================================
;; Indentation
;;==============================================================================

(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(add-hook 'c-mode-hook
          (lambda()
            (setq c-basic-offset 2)
            (c-set-offset 'substatement-open 0)))
(add-hook 'c++-mode-hook
          (lambda()
            (setq c-basic-offset 2)
            (c-set-offset 'substatement-open 0)))
(add-hook 'java-mode-hook
          (lambda()
            (setq c-basic-offset 2)
            (c-set-offset 'substatement-open 0)))

;;==============================================================================
;; Dim for #if 0 ... #endif
;;==============================================================================

(defun cpp-highlight-if-0/1 ()
  "Modify the face of text in between #if 0 ... #endif."
  (setq cpp-known-face '(background-color . "gray15"))
  (setq cpp-unknown-face 'default)
  (setq cpp-face-type 'dark)
  (setq cpp-known-writable 't)
  (setq cpp-unknown-writable 't)
  (setq cpp-edit-list
        '((#("1" 0 1
             (fontified nil))
           nil
           (background-color . "gray15")
           both nil)
          (#("0" 0 1
             (fontified nil))
           (background-color . "gray15")
           nil
           both nil)))
  (cpp-highlight-buffer t))
(defun jpk/c-mode-hook ()
  (cpp-highlight-if-0/1)
  (add-hook 'after-save-hook 'cpp-highlight-if-0/1 'append 'local))
(add-hook 'c-mode-common-hook 'jpk/c-mode-hook)

;;==============================================================================
;; GNU GLOBAL for source tags
;;==============================================================================

;(setq load-path (cons "~/.emacs.d/global" load-path)) ; "/usr/share/emacs/site-lisp/global/" load-path))
;
;(autoload 'gtags-mode "gtags"
;  "Minor mode for browsing source code using GLOBAL" t)
;(add-hook 'c-mode-common-hook
;          (lambda ()
;            (gtags-mode 1)))
;
;;(defun gtags-create-or-update ()
;;  "create or update the gnu global tag file"
;;  (interactive)
;;  (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
;;      (let ((olddir default-directory)
;;            (topdir (read-directory-name
;;                     "gtags: top of source tree:" default-directory)))
;;        (cd topdir)
;;        (shell-command "gtags && echo 'created tagfile'")
;;        (cd olddir)) ; restore
;;    ;;  tagfile already exists; update it
;;    (shell-command "global -u && echo 'updated tagfile'")))
;;
;;(add-hook 'c-mode-common-hook
;;          (lambda ()
;;            (gtags-create-or-update)))
;
;(defun gtags-update-single (filename)
;  "Update Gtags database for changes in a single file"
;  (interactive)
;  (start-process "update-gtags" "update-gtags" "bash" "-c" (concat "cd " (gtags-root-dir) " ; gtags --single-update " filename )))
;
;(defun gtags-update-current-file()
;  (interactive)
;  (defvar filename)
;  (setq filename (replace-regexp-in-string (gtags-root-dir) "." (buffer-file-name (current-buffer))))
;  (gtags-update-single filename)
;  (message "Gtags updated for %s" filename))
;
;(defun gtags-update-hook()
;  "Update GTAGS file incrementally upon saving a file"
;  (when gtags-mode
;    (when (gtags-root-dir)
;      (gtags-update-current-file))))
;
;(add-hook 'after-save-hook 'gtags-update-hook)
;
;(add-hook 'gtags-mode-hook
;          (lambda ()
;            (local-set-key (kbd "M-.") 'gtags-find-tag)
;            (local-set-key (kbd "M-,") 'gtags-find-rtag)))
;
;;(defun my-gtags-keys ()
;;  (define-key c-mode-base-map (kbd "C-.") 'gtags-find-tag)
;;  (define-key c-mode-base-map (kbd "C-?") 'gtags-find-rtag))
;;(add-hook 'c-mode-common-hook 'my-gtags-keys)

;;==============================================================================
;; Asynchronous Shell Command Excution
;;
;; Link - http://stackoverflow.com/questions/16815598/
;;        run-commands-in-emacs-asynchronously-but-display-output-incrementally
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
;; etags
;;==============================================================================

(defun create-tags (dir-name)
  "Create a TAGS file."
  (interactive "DDirectory: ")
  (setq c++-headers-path "/usr/include/c++")
  (with-current-buffer (get-buffer-create "*etags-output*") (erase-buffer))
  (execute-commands "*etags-output*"
                    (format "find -H %s -name \"*\" | xargs etags -o %sTAGS" c++-headers-path dir-name)
                    (format "find -H %s -type f \\( \
-name \"*.[csSh]\" \
-o \
-name \"*.cc\" \
-o \
-name \"*.cpp\"\
-o \
-name \"*.m\" \
-o \
-name \"*.java\" \
-o \
-name \"*.py\" \
-o \
-name \"*.pl\" \
\\) | \
xargs etags -a -o %sTAGS" dir-name dir-name)))

;;==============================================================================
;; find-file-in-tags
;;==============================================================================

(load-file "~/.emacs.d/find-file-in-tags.el")

;;;;==============================================================================
;;;; my-semantic-jump
;;;;==============================================================================
;;
;;(defvar my-semantic-jump-ring
;;  (make-ring 20)
;;  "ring buffer that used to store code jump information")
;;
;;(defun my-make-ring-item ()
;;  (let* ((buffer (current-buffer))
;;         (filename (buffer-file-name buffer))
;;         (offset (point)))
;;    (cons filename offset)))
;;
;;(defun my-semantic-jump-to ()
;;  (interactive)
;;  (let* ((item (my-make-ring-item)))
;;    (if (buffer-file-name (current-buffer))
;;        ;; if it's not an internal buffer
;;        (progn
;;          ;; insert the info before jump
;;          (ring-insert my-semantic-jump-ring item)
;;          (semantic-ia-fast-jump (point))))))
;;
;;(defun my-semantic-jump-prev ()
;;  (interactive)
;;  (let* (item)
;;    (if (eq 0 (ring-length my-semantic-jump-ring))
;;        (error "No history jump info")
;;      (setq item (ring-ref my-semantic-jump-ring 0))
;;      (find-file (car item))
;;      (goto-char (cdr item))
;;      (pulse-momentary-highlight-one-line (point))
;;      (ring-remove my-semantic-jump-ring 0))))

;;;==============================================================================
;;; Jabber
;;;==============================================================================
;
;(require 'jabber-autoloads)

;;==============================================================================
;; Window Resize
;; https://www.emacswiki.org/emacs/WindowResize
;;==============================================================================

(defun resize-window (&optional arg)    ; Hirose Yuuji and Bob Wiener
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
;; Dos To Unix
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
;; Setting Startup Buffer and Directory
;;==============================================================================

(switch-to-buffer "*Messages*")

(setq default-directory my-default-directory) ; this line must be excuted after excuting '(switch-to-buffer "*Messages*")'.
(message "default-directory: %s\n" default-directory)

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

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x p") 'other-window-reverse)

(global-set-key (kbd "C-c d") 'desktop-read)
(global-set-key (kbd "C-c \\") 'neotree-toggle)
(global-set-key (kbd "C-c |") 'visit-tags-table)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c r") 'resize-window)
(global-set-key (kbd "C-c f") 'find-file-in-tags)

(global-set-key (kbd "C-c 1") 'eshell)
;(global-set-key (kbd "C-c 1") 'shell)
;(global-set-key (kbd "C-c 1") 'term)
(global-set-key (kbd "C-c -") 'whitespace-mode)
(global-set-key (kbd "C-c =") 'hexl-mode-toggle)

(global-set-key (kbd "C-c C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-c C-r") 'isearch-backward-regexp)

(global-set-key (kbd "C-c <up>") 'buf-move-up)
(global-set-key (kbd "C-c <down>") 'buf-move-down)
(global-set-key (kbd "C-c <left>") 'buf-move-left)
(global-set-key (kbd "C-c <right>") 'buf-move-right)

(global-set-key (kbd "M-o") 'ff-find-other-file)

;(global-set-key (kbd "C-c RET") 'semantic-ia-complete-symbol-menu)
;(global-set-key (kbd "C-c SPC") 'semantic-ia-show-variants)
;;(global-set-key (kbd "C-c j") 'semantic-ia-fast-jump)
;(global-set-key (kbd "C-c ]") 'my-semantic-jump-to)
;(global-set-key (kbd "C-c [") 'my-semantic-jump-prev)

;; GUD
;(global-set-key [f5] '(lambda ()
;                        (interactive)
;                        (call-interactively 'gud-tbreak)
;                        (call-interactively 'gud-cont)))
;(global-set-key [f6] 'gud-next)
;(global-set-key [f7] 'gud-step)
;(global-set-key [f8] 'gud-finish)
;(global-set-key [f9] 'gud-break)
