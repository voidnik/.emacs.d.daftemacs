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

;;==============================================================================
;; highlight-indent-guides
;;
;; https://github.com/DarthFennec/highlight-indent-guides
;;==============================================================================

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-responsive 'top)

;;==============================================================================
;; cc-search-directories
;;==============================================================================

(setq cc-search-directories '("." "../include" "/usr/include" "/usr/local/include/*"
                              "/System/Library/Frameworks" "/Library/Frameworks"))

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
(setq lsp-file-watch-threshold 2000)

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

(setq lsp-file-watch-ignored
      (append lsp-file-watch-ignored
              '("[/\\\\]\\.ccls-cache$"
                "[/\\\\]\\.deps$"
                "[/\\\\]\\.libs$")))

(add-to-list 'projectile-globally-ignored-directories ".ccls-cache")

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
    :ensure t
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
  :ensure t
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

(use-package py-autopep8
  :ensure t
  :config
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

(use-package importmagic
  :ensure t
  :config
  (add-hook 'python-mode-hook 'importmagic-mode))

;;==============================================================================
;; qml-mode
;;==============================================================================

(autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

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
;(load-file "~/.emacs.d/find-file-in-tags.el"
;(global-set-key (kbd "C-c f") 'find-file-in-tags) ;; OBSOLETE
;(global-set-key (kbd "M-o") 'ff-find-other-file) ;; OBSOLETE

;;;==============================================================================
;;; Dim for #if 0 ... #endif (OBSOLETE)
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


(provide 'init-programming)
