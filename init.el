;; Editor: Richard Jaeho Hur
;; Emacs Version: 25.3.1


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(message "emacs-version: %s %d %d" emacs-version emacs-major-version emacs-minor-version)
(message "system-type: %s" system-type)
(message "system-name: %s" system-name)
(message "user-login-name: %s" user-login-name)
(message "user-init-file: %s (last modified date: 2017-10-03)" user-init-file)
(message "user-emacs-directory: %s" user-emacs-directory)

;;================
;; Basic Settings
;;================

(setq make-backup-files nil)
(setq-default truncate-lines t)
(show-paren-mode t)
(delete-selection-mode t)
(setq gdb-many-windows t)
;(setq split-width-threshold nil)
(setq split-height-threshold nil)

(if (display-graphic-p)
    (progn
      ;;======================
      ;; GUI Settings
      ;;======================
      (tool-bar-mode -1) ; hide tool bar

      (set-frame-position (selected-frame) 0 0)
      ;(set-frame-width (selected-frame) 150)
      ;(set-frame-height (selected-frame) 100)
      ;(print (font-family-list))

      (cond
       ((string-equal system-type "darwin")
        (progn
          (set-face-attribute 'default nil :height 115 :family "monospace")))
       ((string-equal system-type "gnu/linux")
        (progn
          (set-face-attribute 'default nil :height 100 :family "FreeMono")))
       )
      )
  ;;======================
  ;; CUI Settings
  ;;======================
  (setq window-min-width 20)
  )

;;======================
;; Custom Set Variables
;;======================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages (quote (magit pdf-tools))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;=============
;; Common Lisp
;;=============

(require 'cl)

;;=============
;; Color Theme
;;=============

(add-to-list 'load-path "~/.emacs.d/emacs-goodies-el/")
;(add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-goodies-el/")

(require 'color-theme)
(load-file "~/.emacs.d/color-theme/color-theme-sunburst.el")
(load-file "~/.emacs.d/color-theme/color-theme-tangotango.el")
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-tm)))

;;=======
;; CEDET
;;=======

(require 'cedet)

;;==========
;; semantic
;;==========

(semantic-mode 1)

(semantic-add-system-include "/usr/include" 'c-mode)
;(semantic-add-system-include "/usr/include" 'c++-mode)
(semantic-add-system-include "/usr/local/include" 'c-mode)
;(semantic-add-system-include "/usr/local/include" 'c++-mode)

;;=========
;; eassist
;;=========

(load-file "~/.emacs.d/eassist-mod.el")

(setq eassist-header-switches
      '(("h" . ("cpp" "cc" "c" "m"))
        ("hpp" . ("cpp" "cc"))
        ("cpp" . ("h" "hpp"))
        ("c" . ("h"))
        ("m" . ("h"))
        ("C" . ("H"))
        ("H" . ("C" "CPP" "CC"))
        ("cc" . ("h" "hpp")))
      )

(defun my-eassist-keys ()
  (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
  (define-key c-mode-base-map (kbd "M-m") 'eassist-list-methods))
(add-hook 'c-mode-common-hook 'my-eassist-keys)

;;=======
;; ediff
;;=======

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;;=============
;;; sr-speedbar
;;;=============
;
;(load-file "~/.emacs.d/sr-speedbar-mod.el")

;;=========
;; neotree
;;=========

;(load-file "~/.emacs.d/emacs-neotree-mod/neotree.el")
(load-file "~/.emacs.d/emacs-neotree/neotree.el")
(setq neo-window-width 30)
;(setq neo-window-position 'right)

;;=======
;; popup
;;=======
(load-file "~/.emacs.d/popup-el/popup.el")

;;===============
;; auto-complete
;;===============

(load-file "~/.emacs.d/auto-complete/auto-complete.el")

;;==========
;; ac-etags
;;==========

;(load-file "~/.emacs.d/emacs-ac-etags/ac-etags.el")
;(custom-set-variables '(ac-etags-requires 1))
;
;(eval-after-load "etags"
;  '(progn
;     (ac-etags-setup)))
;
;(add-hook 'c-mode-common-hook 'ac-etags-ac-setup)
;(add-hook 'ruby-mode-common-hook 'ac-etags-ac-setup)

;;=======
;; linum
;;=======

(load-file "~/.emacs.d/linum.el")
(setq linum-format "%5d \u2502")
(global-linum-mode)

(defcustom linum-disabled-modes-list '(eshell-mode wl-summary-mode compilation-mode org-mode text-mode dired-mode doc-view-mode)
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

;;=============
;; buffer-move
;;=============

(load-file "~/.emacs.d/buffer-move.el")

;;================================================
;; Killing Buffers
;; https://www.emacswiki.org/emacs/KillingBuffers
;;================================================

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;;=============
;; Indentation
;;=============

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

;;==========================
;; Dim for #if 0 ... #endif
;;==========================

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

;;;============================
;;; GNU GLOBAL for source tags
;;;============================
;
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

;;=====================================
;; Asynchronous Shell Command Excution
;;=====================================

;; Reference - http://stackoverflow.com/questions/16815598/run-commands-in-emacs-asynchronously-but-display-output-incrementally

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

;;=======
;; etags
;;=======

(defun create-tags-c (dir-name)
  "Create TAGS file for C/C++."
  (interactive "DDirectory: ")
  (setq prefix "/usr")
  (setq prefix-local "/usr/local")
  (cond
   ((string-equal system-type "darwin")
    (progn
      (setq prefix "/usr")
      (setq prefix-local "/opt/local")))
   )
  (with-current-buffer (get-buffer-create "*etags-output*") (erase-buffer))
  (execute-commands "*etags-output*"
                    (format "find -H %s/include -name \"*.h\" | xargs etags -o %sTAGS" prefix dir-name)
                    (format "find -H %s/include -name \"*.h\" | xargs etags -a -o %sTAGS" prefix-local dir-name)
                    (format "find -H %s -type f \\( -name \"*.[csSh]\" -o -name \"*.cc\" -o -name \"*.cpp\" -o -name \"*.m\" \\) | xargs etags -a -o %sTAGS" dir-name dir-name))
  )

;;===================
;; find-file-in-tags
;;===================

(load-file "~/.emacs.d/find-file-in-tags.el")

;;==================
;; my-semantic-jump
;;==================

(defvar my-semantic-jump-ring
  (make-ring 20)
  "ring buffer that used to store code jump information")

(defun my-make-ring-item ()
  (let* ((buffer (current-buffer))
         (filename (buffer-file-name buffer))
         (offset (point)))
    (cons filename offset)))

(defun my-semantic-jump-to ()
  (interactive)
  (let* ((item (my-make-ring-item)))
    (if (buffer-file-name (current-buffer)) 
        ;; if it's not an internal buffer
        (progn         
          ;; insert the info before jump
          (ring-insert my-semantic-jump-ring item)
          (semantic-ia-fast-jump (point))))))

(defun my-semantic-jump-prev ()
  (interactive)
  (let* (item)
    (if (eq 0 (ring-length my-semantic-jump-ring))
        (error "No history jump info")
      (setq item (ring-ref my-semantic-jump-ring 0))
      (find-file (car item))
      (goto-char (cdr item))
      (pulse-momentary-highlight-one-line (point))
      (ring-remove my-semantic-jump-ring 0))))

;;===========
;; exec-path
;;===========

(setenv "PATH" (concat (getenv "PATH") ":/Users/daftcoder/Library/Android/sdk/platform-tools"))
(setq exec-path (append exec-path '("/Users/daftcoder/Library/Android/sdk/platform-tools")))

(setenv "PATH" (concat (getenv "PATH") ":/Users/daftcoder/Workspace/trtc/depot_tools"))
(setq exec-path (append exec-path '("/Users/daftcoder/Workspace/trtc/depot_tools")))

;;========
;; Jabber
;;========

;(require 'jabber-autoloads)

;;====================
;; Custom Key Mapping
;;====================

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

(global-set-key (kbd "C-x p") 'other-window-reverse)

(global-set-key (kbd "C-c \\") 'neotree-toggle)

(global-set-key (kbd "C-c |") 'visit-tags-table)

(global-set-key (kbd "C-c 1") 'shell)
(global-set-key (kbd "C-c 2") 'eshell)
(global-set-key (kbd "C-c -") 'whitespace-mode)
(global-set-key (kbd "C-c =") 'hexl-mode-toggle)

(global-set-key (kbd "C-c <up>") 'buf-move-up)
(global-set-key (kbd "C-c <down>") 'buf-move-down)
(global-set-key (kbd "C-c <left>") 'buf-move-left)
(global-set-key (kbd "C-c <right>") 'buf-move-right)

(global-set-key (kbd "C-c f") 'find-file-in-tags)

(global-set-key (kbd "C-c RET") 'semantic-ia-complete-symbol-menu)
(global-set-key (kbd "C-c SPC") 'semantic-ia-show-variants)
;(global-set-key (kbd "C-c j") 'semantic-ia-fast-jump)
(global-set-key (kbd "C-c ]") 'my-semantic-jump-to)
(global-set-key (kbd "C-c [") 'my-semantic-jump-prev)

;; GUD
;(global-set-key [f5] '(lambda ()
;                        (interactive)
;                        (call-interactively 'gud-tbreak)
;                        (call-interactively 'gud-cont)))
;(global-set-key [f6] 'gud-next)
;(global-set-key [f7] 'gud-step)
;(global-set-key [f8] 'gud-finish)
;(global-set-key [f9] 'gud-break)
(put 'erase-buffer 'disabled nil)
