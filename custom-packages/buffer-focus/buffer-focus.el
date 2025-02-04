;;; buffer-focus.el --- Change the focus of a buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Richard Jaeho Hur

;; Author: Richard Jaeho Hur <daftcoder@gmail.com>
;; Package-Version: 1.0
;; Package-Requires: ((emacs "26.3"))
;; Keywords: convenience

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Code:

(require 'windmove)

;;;###autoload
(defun buf-focus-up ()
  "Change the focus of the current buffer to the buffer above the split.
If there is no split, ie now window above the current one, an
error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'up)))
    (if (null other-win)
        (error "No window above this one")
      (select-window other-win))))

;;;###autoload
(defun buf-focus-down ()
  "Change the focus of the current buffer to the buffer under the split.
If there is no split, ie now window under the current one, an
error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'down)))
    (if (null other-win)
        (error "No window under this one")
      (select-window other-win))))

;;;###autoload
(defun buf-focus-left ()
  "Change the focus of the current buffer to the buffer on the left of the split.
If there is no split, ie now window on the left of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'left)))
    (if (null other-win)
        (error "No left split")
      (select-window other-win))))

;;;###autoload
(defun buf-focus-right ()
  "Change the focus of the current buffer to the buffer on the right of the split.
If there is no split, ie now window on the right of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'right)))
    (if (null other-win)
        (error "No right split")
      (select-window other-win))))

(provide 'buffer-focus)
;;; buffer-focus.el ends here
