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

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


;;;; -----------------------------------------------------------------
;;;; General settings
;;;; -----------------------------------------------------------------

;; General customizations to the UI. We want to maximize the space
;; available for the text.

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(display-time)

(setq inhibit-splash-screen t)

(setq confirm-kill-emacs #'yes-or-no-p)
(fset 'yes-or-no-p #'y-or-n-p)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Save History
(savehist-mode)
(setq history-delete-duplicates t)

;; Save place
(require 'saveplace)
(setq-default save-place t)

;; Buffers and window configuration persistence
(desktop-save-mode 1)

;; Enable semantic minor mode
(semantic-mode)

;; dabbrev expands preserving the orignial case
(setq dabbrev-case-replace nil)
;; but use hippie-expand instead of raw dabbrev-expand!
(bind-key "M-/" 'hippie-expand)

(setq browse-url-browser-function 'browse-url-chromium)


(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
                           try-complete-file-name-partially try-complete-file-name
                           try-expand-all-abbrevs try-expand-list try-expand-line
                           try-complete-lisp-symbol-partially try-complete-lisp-symbol))

;;; Don't use tabs
(setq-default indent-tabs-mode nil)
;; but if some mode use them, then set it to 4 spaces
(setq-default tab-width 4)

(defun davazp/switch-to-other-buffer ()
  "Switch to the most recently visited buffer. Calling this
command repeatly will switch between the last two most recent
buffers."
  (interactive)
  (switch-to-buffer (other-buffer)))

(bind-key "C-;" 'davazp/switch-to-other-buffer)


;; Smarter Modeline
(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))


;; Cycle between different spacing styles, instead of just-one-space
(bind-key "M-SPC" 'cycle-spacing)


;;;; -----------------------------------------------------------------
;;;; Convenience
;;;; -----------------------------------------------------------------

;;; Diff

;; Do not open the diff control buffer in a different frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Use a vertical setting by default
(setq ediff-split-window-function 'split-window-horizontally)


;;; Zooming

(use-package zoom-frm)


;;; Multiple Cursors and expand-region

(use-package multiple-cursors
  :bind ("C-$" . mc/mark-next-like-this))

(use-package expand-region
  :bind ("C-#" . er/expand-region))


;;; String utilities

(use-package s)


;;; Helm

(use-package helm
  :diminish helm-mode
  :bind ("M-x" . helm-M-x)
  :bind ("C-x b" . helm-mini)
  :bind ("C-x C-b" . helm-buffers-list)
  :bind ("C-x C-f" . helm-find-files)
  :bind ("C-x C-d" . helm-browse-project)
  :bind ("C-h a" . helm-apropos)
  ;; :bind ("M-y" . helm-show-kill-ring)
  :config
  (require 'helm-config)
  (helm-mode)
  (bind-key "<tab>" 'helm-execute-persistent-action helm-map) 
  (bind-key "C-i" 'helm-execute-persistent-action helm-map)
  (bind-key "C-z" 'helm-select-action helm-map) ; list actions using C-z
  (bind-key "o" 'helm-occur helm-command-map)

  (setq helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-recentf-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-M-x-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-lisp-fuzzy-completion t))


(use-package helm-ls-git)


(use-package helm-descbinds
  :config
  (helm-descbinds-mode))

(use-package helm-swoop
  :config
  (bind-key "C-r" 'helm-previous-line helm-swoop-map)
  (bind-key "C-s" 'helm-next-line helm-swoop-map)
  (bind-key "C-r" 'helm-previous-line helm-multi-swoop-map)
  (bind-key "C-s" 'helm-next-line helm-multi-swoop-map))

;;; Smart parents

(use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (smartparens-global-strict-mode)
  (sp-use-paredit-bindings)
  (bind-key "C-j" 'sp-newline sp-keymap)
  (sp-pair "(" ")" :wrap "M-(")
  (setq sp-highlight-pair-overlay nil))



;;;; -----------------------------------------------------------------
;;;; Tools
;;;; -----------------------------------------------------------------


;;; Org-mode
(setq org-startup-indented t)
(setq org-src-fontify-natively t)
(setq org-return-follows-link t)
(bind-key "C-c a" 'org-agenda)

;;; Eshell/Shell
(defun davazp/eshell-hook ()
  (bind-key "M-r" 'helm-eshell-history eshell-mode-map))

(add-hook 'eshell-mode-hook 'davazp/eshell-hook)

(bind-key "C-c C-l" 'helm-comint-input-ring shell-mode-map)

;;; Dired

(require 'dired-x)
(require 'dired-aux)

(setq dired-listing-switches "-lh")

(setq dired-omit-files "^\\.?#\\|^\\.")
(add-hook 'dired-mode-hook 'dired-omit-mode)

(use-package wgrep)


;;; MaGIT -- Git integration with GNU/Emacs

(use-package magit
  :bind ("<f12>" . magit-status)
  :config
  (setq magit-popup-show-common-commands nil)
  (setq magit-last-seen-setup-instructions "1.4.0"))



;;; Grep and others

;; Use case-insensitive grep by default
(require 'grep)
(grep-apply-setting 'grep-find-command '("find . -type f -exec grep -nH -i -e  {} +" . 37))


;;; Yasnippet

(let ((base (file-name-directory (or load-file-name (buffer-file-name)))))
  (use-package yasnippet
    :diminish yas-minor-mode
    :config 
    (setq yas-snippet-dirs (list (concat base "snippets/")))
    (yas-global-mode)
    (setq yas-prompt-functions '(yas-completing-prompt))))


;;; Projectile

(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name)))))

(use-package helm-projectile
  :config
  (helm-projectile-on)
  (setq projectile-switch-project-action 'projectile-vc))



;;;; -----------------------------------------------------------------
;;;; Programming Languages and Markup
;;;; -----------------------------------------------------------------

(use-package paren-face
  :config 
  (global-paren-face-mode 1))


;;; Support SASS

(use-package helm-css-scss
  :config
  (require 'css-mode)
  (bind-key "M-." 'helm-css-scss css-mode-map))

(use-package scss-mode
  :mode ("\\.scss\\'" . scss-mode)
  :config
  (bind-key "M-." 'helm-css-scss scss-mode-map))


;;; Support for Markdown, YAML and JSON.
(use-package markdown-mode)
(use-package yaml-mode)
(use-package json-mode)


;;; Haskell

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))


;;;; Emacs Lisp

;; Nice slime-like navigation for Emacs lisp with M-. and M-,
(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))


;; Show parameter information in the minibuffer
(require 'eldoc)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(diminish 'eldoc-mode)


(use-package dash
  :config 
  (dash-enable-font-lock))


;;;; Common Lisp

(use-package slime
  :config
  (setq inferior-lisp-program (locate-file "sbcl" exec-path))
  (slime-setup '(slime-fancy)))


;;;; Javascript

(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :config
  (setq-default js2-basic-offset 2)
  (setq-default js-indent-level 2)
  (setq js2-include-browser-externs t
        js2-include-node-externs t))

;;; Add support in ffap for finding files loaded from node_modules.
(require 'ffap)
(defun davazp/ffap-nodejs-module (name)
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

(add-to-list 'ffap-alist '(js-mode . davazp/ffap-nodejs-module) t)
(add-to-list 'ffap-alist '(js2-mode . davazp/ffap-nodejs-module) t)


(use-package nodejs-repl)

(defun js-send-to-nodejs-repl ()
  (interactive)
  (let ((string (buffer-string)))
    (nodejs-repl)
    (nodejs-repl--send-string string)))




;;;; -----------------------------------------------------------------
;;;;  Load machine-specific configuration if available
;;;; -----------------------------------------------------------------

(let ((machine-elisp-file (expand-file-name (concat system-name ".el"))))
  (when (file-exists-p machine-elisp-file)
    (load machine-elisp-file)))


(provide 'dotemacs)

;;; Local Variables:
;;; mode: emacs-lisp
;;; End:

;;; dotemacs.el ends here
