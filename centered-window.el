;;; centered-window.el --- Center the text when there's only one window  -*- lexical-binding: t; -*-
;;
;; Author: Richard Jaeho Hur <daftcoder@gmail.com>
;;
;; Original Author: Anler Hernández Peral <inbox+emacs@anler.me>
;; Version: 1.4.0
;; Package-Version: 20200426.1053
;; Package-Commit: f50859941ab5c7cbeaee410f2d38716252b552ac
;; Contributors:
;;    Mickael Kerjean <https://github.com/mickael-kerjean>
;;    Pierre Lecocq   <https://github.com/pierre-lecocq>
;;    Syohei YOSHIDA  <https://github.com/syohex>
;;    Lars Tveito     <https://github.com/larstvei>
;;    Tianxiang Xiong <https://github.com/xiongtx>
;; Keywords: faces windows
;; URL: https://github.com/anler/centered-window-mode
;; Package-Requires: ((emacs "24.4"))
;; Compatibility: GNU Emacs 24.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Enable centered-window-mode and your text is going to be centered when there's
;; only one window in the frame.
;;
;; Customizable options are:
;;  cwm-lighter
;;  cwm-centered-window-width
;;  cwm-ignore-buffer-predicates
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'face-remap)
(require 'subr-x)
(require 'mac-win nil t)
(require 'mwheel nil t)

(defgroup centered-window nil
  "Center text in windows."
  :group 'windows
  :prefix "cwm-")

(defcustom cwm-lighter
  " #"
  "Mode's lighter used in the mode line."
  :group 'centered-window
  :type 'string)

(defcustom cwm-centered-window-width
  110
  "Minimum line length required to apply the margins."
  :group 'centered-window
  :initialize #'custom-initialize-default
  :set #'cwm--set-and-recenter-windows
  :type 'integer)

(defcustom cwm-ignore-buffer-predicates
  (list #'cwm-special-buffer-p)
  "List of predicate functions.
Each is run with current buffer and if it returns 't the
mode won't activate in that buffer."
  :group 'centered-window
  :type '(list function))

(define-obsolete-variable-alias
  'centered-window-mode-hooks
  'cwm-hooks "1.3.0")
(defcustom cwm-hooks
  nil
  "Hooks to run every time window is centered (be careful)."
  :group 'centered-window
  :type 'hook)

(defun cwm--set-and-recenter-windows (var val)
  "Set customizable variable VAR to VAL and recenter windows.

All windows in all frames are recentered.

This is intended for use as the `setfunction' of a
`defcustom'. See Info node `(elisp) Variable Definitions'."
  (set-default var val)
  (dolist (frame (frame-list))
    (with-selected-frame frame
      (cwm-center-windows))))

(defadvice load-theme (after cwm-set-faces-on-load-theme activate)
  "Change the default fringe background whenever the theme changes."
  (cwm-update-fringe-background))

(defun cwm-ignore-window-p (window)
  "Check if BUFF should be ignored when activating the mode."
  (not
   (null
    (delq nil
          (mapcar (lambda (predicate)
                    (funcall predicate (window-buffer window)))
                  cwm-ignore-buffer-predicates)))))

(defun cwm-special-buffer-p (buffer)
  "Return 't if BUFF buffer name is special (starts with an *).

The *scratch* buffer although special, is treated as not special
by this function."
  (let ((buffname (string-trim (buffer-name buffer))))
    (and buffname
         (or (string-prefix-p "*" buffname)
             (string-match "magit-?[0-9a-zA-Z]*?: " buffname)
             (string= "pdf-view-mode" (with-current-buffer buffer major-mode)))
         (not (string= "*Messages*" buffname))
         (not (string-prefix-p "*scratch" buffname))
         (not (string= "*Packages*" buffname))
         (not (string-prefix-p "*Bufler" buffname))
         (not (string= "*Ibuffer*" buffname))
         (not (string-prefix-p "*xref" buffname))
         (not (string-prefix-p "*eshell" buffname))
         (not (string-prefix-p "*ansi-term" buffname))
         (not (string-prefix-p "*ein" buffname)))))

(defun cwm-update-fringe-background ()
  (custom-set-faces
   `(fringe ((t (:background ,(face-attribute 'default :background)))))))

(defun cwm-turn-on ()
  (add-hook 'window-configuration-change-hook #'cwm-center-windows)
  (add-hook 'window-size-change-functions #'cwm-center-windows-frame)
  (cwm-center-windows))

(defun cwm-turn-off ()
  (remove-hook 'window-configuration-change-hook #'cwm-center-windows)
  (remove-hook 'window-size-change-functions #'cwm-center-windows-frame)
  (cwm-center-windows))

(defun cwm-center-windows-frame (frame)
  (when (frame-size-changed-p frame)
    (cwm-center-windows)))

(defun cwm-center-windows ()
  (let ((windows (window-list nil :exclude-minibuffer)))
    (mapc #'cwm-center-window-instructions
          (mapcar #'cwm-centering-instructions
                  (cl-remove-if #'cwm-ignore-window-p windows)))
    (run-hooks 'centered-window-mode-hooks)))

(cl-defstruct cwm-centering-instructions
  window
  left-width
  right-width)

(defun cwm-center-window-instructions (instructions)
  (let* ((window (cwm-centering-instructions-window instructions)))
    (set-window-margins window
                        (cwm-centering-instructions-left-width instructions)
                        (cwm-centering-instructions-right-width instructions))))

(defun cwm-centering-instructions (window)
  (let ((widths (cwm-calculate-appropriate-margin-widths window)))
    (make-cwm-centering-instructions
     :window window
     :left-width (car widths)
     :right-width (cdr widths))))

(defun cwm-calculate-appropriate-margin-widths (window)
  (let* ((mode-active-p (with-current-buffer (window-buffer window) centered-window-mode))
         (window-width (window-total-width window))
         (margin (if mode-active-p
                     (max (/ (- window-width cwm-centered-window-width) 2) 0)
                   0))
         (left-width margin)
         (right-width margin))
    `(,left-width . ,right-width)))

;;;###autoload
(defun centered-window-mode-toggle ()
  (if centered-window-mode
      (centered-window-mode -1)
    (centered-window-mode +1)))

;;;###autoload
(define-minor-mode centered-window-mode
  "Minor mode to center text on the current buffer"
  :init-value nil
  :global t
  :lighter cwm-lighter
  (if centered-window-mode (cwm-turn-on) (cwm-turn-off)))

(provide 'centered-window-mode)
(provide 'centered-window)
;;; centered-window.el ends here
