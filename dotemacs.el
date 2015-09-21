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

;; General customizations to the UI. We want to maximize the space
;; available for the text.

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(display-time)

;; Do not generate backup files, we have git nowadays.
(setq make-backup-files nil)



;; Use ido to find buffers and files. In addition of that, we define
;; the `switch-to-other-buffer' function to explote the locality of
;; references: I work most of the time with one or two buffers at the
;; same time.

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



;;;; -----------------------------------------------------------------
;;;; Convenience
;;;; -----------------------------------------------------------------

;;; Zooming

(unless (package-installed-p 'zoom-frm)
  (package-install 'zoom-frm))


;;; Multiple Cursors and expand-region

(unless (package-installed-p 'multiple-cursors)
  (package-install 'multiple-cursors))

(unless (package-installed-p 'expand-region)
  (package-install 'expand-region))

(define-key global-map (kbd "C-#") 'er/expand-region)
(define-key global-map (kbd "C-$") 'mc/mark-next-like-this)


;;;; -----------------------------------------------------------------
;;;; Tools
;;;; -----------------------------------------------------------------

;;; Dired

(require 'dired-x)
(require 'dired-aux)

(setq dired-listing-switches "-lh")

(setq dired-omit-files "^\\.?#\\|^\\.")
(add-hook 'dired-mode-hook 'dired-omit-mode)

(unless (package-installed-p 'wgrep)
  (package-install 'wgrep))


;;; MaGIT -- Git integration with GNU/Emacs

(unless (package-installed-p 'magit)
  (package-install 'magit))

(setq magit-popup-show-common-commands nil)
(setq magit-last-seen-setup-instructions "1.4.0")
(define-key global-map (kbd "<f12>") 'magit-status)



;;;; -----------------------------------------------------------------
;;;; Programming Languages
;;;; -----------------------------------------------------------------


;;;; Lisp
;;;
;;; Use paredit as a close to structural editor


;;; Nice slime-like navigation for Emacs lisp with M-. and M-,

(unless (package-installed-p 'elisp-slime-nav)
  (package-install 'elisp-slime-nav))

(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))


(unless (package-installed-p 'paredit)
  (package-install 'paredit))

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)


(provide 'dotemacs)
;;; dotemacs.el ends here

