;;; dotemacs.el --- My personal GNU/Emacs customizations  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  David Vazquez

;; Author: David Vazquez <davazp@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;;   ,           ,
;;  /             \
;; ((__-^^-,-^^-__))
;;  `-_---' `---_-'
;;   `--|o` 'o|--'
;;      \  `  /
;;       ): :(
;;       :o_o:
;;        "-"

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)


;;;; General settings

;;; General customizations to the UI. We want to maximize the space
;;; available for the text.

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(display-time)

;;; Do not generate backup files, we have git nowadays.
(setq make-backup-files nil)


;;; Use ido to find buffers and files. In addition of that, we define
;;; the `switch-to-other-buffer' function to explote the locality of
;;; references: I work most of the time with one or two buffers at the
;;; same time.

(ido-mode 1)
(setq ido-use-filename-at-point 'guess)
(setq read-file-name-completion-ignore-case t)

(defun switch-to-other-buffer ()
  "Switch to the most recently visited buffer. Calling this
command repeatly will switch between the last two most recent
buffers."
  (interactive)
  (switch-to-buffer (other-buffer)))

(define-key global-map (kbd "C-;") 'switch-to-other-buffer)




(provide 'dotemacs)
;;; dotemacs.el ends here
