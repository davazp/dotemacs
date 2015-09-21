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


;;;; -----------------------------------------------------------------
;;;; General settings
;;;; -----------------------------------------------------------------

;; General customizations to the UI. We want to maximize the space
;; available for the text.

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(display-time)

;; Do not generate backup files, we have git nowadays.
(setq make-backup-files nil)

;;; dabbrev expands preserving the orignial case
(setq dabbrev-case-replace nil)

;;; Don't use tabs
(setq-default indent-tabs-mode nil)


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

;;; Diff

;; Do not open the diff control buffer in a different frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Use a vertical setting by default
(setq ediff-split-window-function 'split-window-horizontally)


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
;;;; Programming Languages and Markup
;;;; -----------------------------------------------------------------

;; Install paredit for a pleasent Lisp coding experience
(unless (package-installed-p 'paredit)
  (package-install 'paredit))



;;; Support SASS

(unless (package-installed-p 'scss-mode)
  (package-install 'scss-mode))

(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))


;;; Support for Markdown, YAML and JSON.

(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

(unless (package-installed-p 'yaml-mode)
  (package-install 'yaml-mode))

(unless (package-installed-p 'json-mode)
  (package-install 'json-mode)
  (setq json-mode))



;;;; Emacs Lisp

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

;; Nice slime-like navigation for Emacs lisp with M-. and M-,
(unless (package-installed-p 'elisp-slime-nav)
  (package-install 'elisp-slime-nav))

(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

;; Show parameter information in the minibuffer
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(unless (package-installed-p 'dash)
  (package-install 'dash))

(eval-after-load "dash" '(dash-enable-font-lock))


;;;; Scheme
(add-hook 'scheme-mode-hook 'enable-paredit-mode)


;;;; Common Lisp

(add-hook 'lisp-mode-hook 'enable-paredit-mode)

(unless (package-installed-p 'slime)
  (package-install 'slime))

(setq inferior-lisp-program (locate-file "sbcl" exec-path))
(slime-setup '(slime-fancy))



;;;; Javascript

(unless (package-installed-p 'js2-mode)
  (package-install 'js2-mode))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)


;;; Add support in ffap for finding files loaded from node_modules.
(require 'ffap)
(defun ffap-nodejs-module (name)
  (unless (or (string-prefix-p "/" name)
              (string-prefix-p "./" name)
              (string-prefix-p "../" name))
    (let ((base (locate-dominating-file
                 default-directory
                 (lambda (dir)
                   (let ((filename (concat dir "node_modules/" name)))
                     (and (file-exists-p filename)
                          filename))))))
      (and base (concat base "node_modules/" name)))))

(add-to-list 'ffap-alist '(js-mode . ffap-nodejs-module) t)
(add-to-list 'ffap-alist '(js2-mode . ffap-nodejs-module) t)



(provide 'dotemacs)

;;; Local Variables:
;;; mode: emacs-lisp
;;; End:

;;; dotemacs.el ends here

