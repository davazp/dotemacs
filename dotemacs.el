;;; dotemacs.el --- My personal GNU/Emacs customizations  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2017  David Vazquez

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

(require 'cl-lib)


;;;; -----------------------------------------------------------------
;;;; General settings
;;;; -----------------------------------------------------------------

;; General customizations to the UI. We want to maximize the space
;; available for the text.

(setq visible-bell nil)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))


(menu-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(tool-bar-mode -1)

(display-time)

(setq inhibit-splash-screen t)

(when (boundp 'mac-command-modifier)
  (setq mac-command-modifier 'meta))

(setq default-input-method 'spanish-prefix)

(setq confirm-kill-emacs #'yes-or-no-p)
(fset 'yes-or-no-p #'y-or-n-p)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
;;; I have almost lost too many changes already. Use numbered backups!
(setq version-control t)

;;; Start eamcs in daemon mode, so emacsclient can connect to it
(server-start)

;; Save History
(savehist-mode)
(setq history-delete-duplicates t)

;; Save place
(require 'saveplace)
(setq-default save-place t)

;; Buffers and window configuration persistence
(desktop-save-mode 1)
(setq desktop-load-locked-desktop t)

;; Enable semantic minor mode
(semantic-mode)

;; dabbrev expands preserving the orignial case
(setq dabbrev-case-replace nil)
;; but use hippie-expand instead of raw dabbrev-expand!
(bind-key "M-/" 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
                           try-complete-file-name-partially try-complete-file-name
                           try-expand-all-abbrevs try-expand-list try-expand-line
                           try-complete-lisp-symbol-partially try-complete-lisp-symbol))

;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)


(setq browse-url-browser-function 'browse-url-chromium)

;;; Don't use tabs
(setq-default indent-tabs-mode nil)
;; but if some mode use them, then set it to 4 spaces
(setq-default tab-width 4)


(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))


;;; M-x regexp-builder uses query-replace-regexp syntax by default
(setq reb-re-syntax 'string)

(use-package visual-regexp
  :config
  (bind-key "C-M-%" 'vr/query-replace))



(defun davazp/switch-to-other-buffer ()
  "Switch to the most recently visited buffer. Calling this
command repeatly will switch between the last two most recent
buffers."
  (interactive)
  (switch-to-buffer (other-buffer)))

(bind-key "C-;" 'davazp/switch-to-other-buffer)


;; Cycle between different spacing styles, instead of just-one-space
(bind-key "M-SPC" 'cycle-spacing)


;;;; -----------------------------------------------------------------
;;;; Convenience
;;;; -----------------------------------------------------------------


(defun davazp/set-macro-counter-padding (n)
  (interactive "nPadding characters: ")
  (kmacro-set-format (concat "%0" (number-to-string n) "d")))


;;; Diff

;; Do not open the diff control buffer in a different frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Use a vertical setting by default
(setq ediff-split-window-function 'split-window-horizontally)


;;; Zooming

(use-package zoom-frm
  :bind ("C-x C-+" . zoom-in/out)
  :bind ("C-x C--" . zoom-in/out)
  :bind ("C-x C-=" . zoom-in/out)
  :bind ("C-x C-0" . zoom-in/out))


;;; Multiple Cursors and expand-region

(use-package multiple-cursors
  :bind ("C-$" . mc/mark-next-like-this))

;; (let ((dotemacs-path (file-name-directory (or load-file-name (buffer-file-name)))))
;;   (add-to-list 'load-path (expand-file-name "phi-search" dotemacs-path)))

;; (require 'phi-search)


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
  :bind ("C-x r b" . helm-bookmarks)
  :bind ("C-h a" . helm-apropos)
  ;; :bind ("M-y" . helm-show-kill-ring)
  :config
  (require 'helm-config)
  (helm-mode)
  (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
  (bind-key "C-i" 'helm-execute-persistent-action helm-map)
  (bind-key "C-z" 'helm-select-action helm-map) ; list actions using C-z
  (bind-key "o" 'helm-occur helm-command-map)

  (setq recentf-max-saved-items 200)

  (setq helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-ff-guess-ffap-filenames t
        helm-recentf-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-M-x-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-lisp-fuzzy-completion t))


(use-package helm-ls-git)
(use-package dash)

(when load-file-name
  (load
   (concat (file-name-directory (or load-file-name default-directory))
           "helm-figre")))

(use-package helm-descbinds
  :config
  (helm-descbinds-mode))

(use-package helm-swoop
  :config
  (bind-key "C-r" 'helm-previous-line helm-swoop-map)
  (bind-key "C-s" 'helm-next-line helm-swoop-map)
  (bind-key "C-r" 'helm-previous-line helm-multi-swoop-map)
  (bind-key "C-s" 'helm-next-line helm-multi-swoop-map))


(use-package helm-dash)


;;; Smart parents

(use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (setq sp-ignore-modes-list '(lisp-mode scheme-mode emacs-lisp-mode elixir-mode))
  (smartparens-global-strict-mode)
  (sp-use-paredit-bindings)
  ;; Fix weird behaviour in js2-mode when we kill an empty line right
  ;; before an expression. Reported upstream at:
  ;;   https://github.com/Fuco1/smartparens/issues/696
  (setq sp-no-reindent-after-kill-modes (remq 'js2-mode sp-no-reindent-after-kill-modes)))


;;; Paredit

(use-package paredit
  :diminish paredit-mode
  :config
  (add-hook 'scheme-mode-hook 'enable-paredit-mode
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)))


;;; Go Mode

(use-package go-mode
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (setq gofmt-command "goimports")
  (bind-key "M-." 'godef-jump go-mode-map))

(use-package go-eldoc
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))
  

;;; Elixir

(use-package elixir-mode
  )

(use-package alchemist
  :config
  (add-hook 'elixir-mode 'alchemist-mode))


;;;; -----------------------------------------------------------------
;;;; Tools
;;;; -----------------------------------------------------------------


;;; Org-mode
(setq org-startup-indented t)
(setq org-src-fontify-natively t)
(setq org-return-follows-link t)
(setq org-refile-targets (list `(nil :maxlevel . 9)))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

(setq org-default-notes-file "~/org/NOTES.org")

(bind-key "C-c l" 'org-store-link)
(bind-key "C-c c" 'org-capture)
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c b" 'org-iswitchb)

(setq org-todo-keyword-faces
      '(("STARTED" . "yellow")
        ("CANCELED" . (:foreground "gray" :weight bold))))

(setq org-enforce-todo-dependencies t)

(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-ctrl-k-protect-subtree t)

(setq calendar-week-start-day 1)

(add-hook 'before-save-hook 'org-update-all-dblocks)

(use-package org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

(defun davazp/gtd ()
  (interactive)
  (find-file org-default-notes-file))

(bind-key "C-c g" 'davazp/gtd)

(setq org-capture-templates
      '(("t" "Task" entry
         (file+headline "~/org/NOTES.org" "Inbox")
         "* %?
%T [[%F][%f]]
%i")))

(setq org-stuck-projects '("+PROJECT" ("TODO") nil nil))
(setq org-tags-exclude-from-inheritance '("PROJECT"))


;;; ERC
(defun chat ()
  (interactive)
  (setq erc-autojoin-channels-alist '(("freenode.net" "#emacs-es" "#lisp-es")))
  (erc :server "irc.freenode.org" :nick "davazp"))


;;; Eshell/Shell

;;; Case-insensitive completion in eshell
(setq eshell-cmpl-ignore-case t)

(defun davazp/eshell-hook ()
  (bind-key "M-r" 'helm-eshell-history eshell-mode-map)
  ;; Too many programs have a line-oriented behaviour, let's truncate
  ;; lines instead of do folding in the terminal.
  (toggle-truncate-lines t))

(add-hook 'eshell-mode-hook 'davazp/eshell-hook)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(bind-key "C-c C-l" 'helm-comint-input-ring comint-mode-map)
(setq comint-prompt-read-only t)

(setq eshell-visual-commands
      '("screen" "top" "less" "htop"))

(setenv "PAGER" "cat")


;;; Dired

(require 'dired-x)
(require 'dired-aux)

(setq dired-listing-switches "-lha")

(setq dired-omit-files "^\\.?#\\|^\\.")
(add-hook 'dired-mode-hook 'dired-omit-mode)


;;; MaGIT -- Git integration with GNU/Emacs

(use-package magit
  :bind ("<f12>" . magit-status)
  :config
  (setq magit-popup-show-common-commands nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (global-magit-file-mode)
  (bind-key "w" 'davazp/magit-cleanup-hunk-whitespace magit-hunk-section-map))


(defun davazp/magit-cleanup-hunk-whitespace ()
  "Cleanup the whitespaces in the diff hunk under the cursor."
  (interactive)
  (let ((current (magit-current-section)))
    (when (eq 'hunk (magit-section-type current))
      (let ((file (magit-file-at-point))
            (context (nth 2 (magit-section-value current))))
        (cl-destructuring-bind (first-line count)
            (mapcar #'string-to-number (split-string context ","))
          (save-excursion
            (with-current-buffer (find-file-noselect file)
              (let (start end)
                (goto-char (point-min))
                (forward-line (1- first-line))
                (setq start (point))
                (forward-line (1- count))
                (setq end (point))
                (whitespace-cleanup-region start end)
                (magit-refresh)))))))))


(defvar magit-show-remote-sections
  '("origin")
  "Magit remotes to show by default in the ref manager. Other
remotes are folded automatically.")

(defun magit-hide-other-origin-sections (section)
  "Hide remote sections if they are not listed in
`magit-show-remote-sections'."
  (if (and (eq 'remote (magit-section-type section))
           (not (member (magit-section-value section)
                        magit-show-remote-sections)))
      'hide
    nil))

(add-hook 'magit-section-set-visibility-hook 'magit-hide-other-origin-sections)

(autoload 'helm-gitlab "gitlab" nil t)




;;; Grep and others

;; Use case-insensitive grep by default
(require 'grep)
(grep-apply-setting 'grep-find-command '("find . -type f -exec grep -nH -i -e  {} +" . 37))

(dolist (dir '("node_modules" "dist"))
  (add-to-list 'grep-find-ignored-directories dir))

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))




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
  :diminish projectile-mode
  :config
  (projectile-global-mode))

(use-package helm-projectile
  :config
  (helm-projectile-on)
  (setq projectile-switch-project-action 'helm-ls-git-ls))

(use-package helm-git-grep
  :config
  (bind-key "s g" 'helm-git-grep projectile-command-map))



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


;;; Lua

(use-package lua-mode)


;;; Haskell

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))


;;; Docker
(use-package dockerfile-mode)


;;;; Emacs Lisp

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
  (slime-setup '(slime-fancy))
  (add-hook 'slime-repl-mode-hook 'paredit-mode))


;;; Clojure

(use-package cider
  :config
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'eldoc-mode)
  (setq cider-repl-display-help-banner nil)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-cljs-lein-repl
        "(do (user/run)
           (user/browser-repl))"))




;;;; SQL
(add-hook 'sql-mode-hook 'subword-mode)


;;;; Javascript

(add-to-list 'load-path "~/.tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)

(use-package js2-mode
  ;; :mode ("\\.js$" . js2-mode)
  :config
  (bind-key "C-M-h" 'js2-mark-defun js2-mode-map)
  (bind-key "RET" 'js2-line-break js2-mode-map)

  (add-hook 'js2-mode-hook 'subword-mode)
  (add-hook 'js2-mode-hook 'tern-mode)

  (setq js2-highlight-level 3)
  (setq js2-include-browser-externs t
        js2-include-node-externs t
        js2-strict-missing-semi-warning nil
        js2-strict-inconsistent-return-warning nil)

   (add-to-list 'interpreter-mode-alist '("node" . js2-mode))

   (setq js2-global-externs '("angular" "describe" "it" "beforeEach" "afterEach"))
  ;; (add-hook 'js2-mode-hook 'js2-highlight-unused-variables-mode)
   )

(use-package editorconfig
  :diminish 'editorconfig-mode)


(use-package rjsx-mode
  :mode ("\\.jsx?$" . rjsx-mode)
  :config
  nil)

(use-package typescript-mode
  :config
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'tide-mode))

(add-to-list 'auto-mode-alist '("\\.tsx?$" . typescript-mode))


(use-package tide)

(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'js-mode-hook 'prettier-js-mode)


(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
(add-hook 'typescript-mode-hook #'setup-tide-mode)


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
                     (file-exists-p filename))))))
      (cond
       (base
        (let ((file (concat base "node_modules/" name)))
          (if (and (file-directory-p file) (not (string-match-p "/$" file)))
              (concat file "/")
            file)))
       ((string-match-p "\\.jsx?$" name)
        ;; End here
        nil)
       (t
        (davazp/ffap-nodejs-module (concat name ".js")))))))


(dolist (mode '(rjsx-mode js-mode js2-mode))
  (add-to-list 'ffap-string-at-point-mode-alist
               ;; Taken from the default entry `file' in the variable
               ;; `ffap-string-at-point-mode-alist', but changed NOT
               ;; to remove @ fromthe beginning, as they are used for
               ;; package scopes.
               `(,mode "--:\\\\${}+<>@-Z_[:alpha:]~*?" "<" "@>;.,!:")))

(add-to-list 'ffap-alist '(js-mode . davazp/ffap-nodejs-module) t)
(add-to-list 'ffap-alist '(js2-mode . davazp/ffap-nodejs-module) t)
(add-to-list 'ffap-alist '(rjsx-mode . davazp/ffap-nodejs-module) t)


(use-package mocha
  :config
  (bind-key "<f6>" 'davazp/mocha-dwim js2-mode-map))

(defun davazp/mocha-dwim ()
  (interactive)
  (cond
   ((mocha-find-current-test)
    (mocha-run (buffer-file-name) (mocha-find-current-test)))
   ((and (string-match-p "test" (buffer-file-name)) (eq major-mode 'js2-mode))
    (mocha-run (buffer-file-name)))
   (t
    (mocha-run))))


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
