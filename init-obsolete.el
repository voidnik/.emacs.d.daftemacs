;;==============================================================================
;; exec-path (OBSOLETE by exec-path-from-shell)
;;==============================================================================

(cond
 ((string-equal system-type "darwin")
  (progn
    (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
    (setq exec-path (append exec-path '("/usr/local/bin")))
    (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
    (setq exec-path (append exec-path '("/Library/TeX/texbin")))
    (setenv "PATH" (concat (concat (getenv "PATH") ":") (expand-file-name "~/Library/Android/sdk/platform-tools")))
    (setq exec-path (append exec-path (list (expand-file-name "~/Library/Android/sdk/platform-tools"))))))
 ((string-equal system-type "gnu/linux")
  (progn
    (setenv "PATH" (concat (getenv "PATH") ":/usr/bin"))
    (setq exec-path (append exec-path '("/usr/bin"))))))

(setenv "PATH" (concat (concat (getenv "PATH") ":") (expand-file-name "~/Workspace/depot_tools")))
(setq exec-path (append exec-path (list (expand-file-name "~/Workspace/depot_tools"))))

;;==============================================================================
;; fcitx (OBSOLETE)
;;
;; https://github.com/cute-jumper/fcitx.el
;;==============================================================================

(if (string-equal system-type "gnu/linux")
    (use-package fcitx
      :ensure t
      :config
      (fcitx-aggressive-setup)
      (setq fcitx-use-dbus t)))

;;==============================================================================
;; gtags (OBSOLETE)
;;==============================================================================

;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; Set key bindings
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))

;;==============================================================================
;; etags (OBSOLETE)
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
;; find-file-in-tags (OBSOLETE)
;;==============================================================================

(load-file "~/.emacs.d/find-file-in-tags.el")
(global-set-key (kbd "C-c f") 'find-file-in-tags)

;;==============================================================================
;; Dim for #if 0 ... #endif (OBSOLETE)
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
