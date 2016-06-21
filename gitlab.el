;;; gitlab.el --- A helm based interface to gitlab   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  David Vazquez Pua

;; Author: David Vazquez Pua <davazp@gmail.com>
;; Keywords: tools

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


(require 'eieio)
(require 'request)

(defvar gitlab-token)
(defvar gitlab-projects)
(defvar gitlab-clone-default-directory)


;;;; Cache

(defvar gitlab-cache-file
  "~/.emacs.d/gitlab.eieio")

(defclass gitlab-cache (eieio-persistent)
  ((projects
    :initarg :projects
    :accessor gitlab-cache-projects)))

(defun gitlab-read-cache ()
  (ignore-errors
    (let ((cache (eieio-persistent-read gitlab-cache-file 'gitlab-cache)))
      (setq gitlab-projects (gitlab-cache-projects cache)))))
(gitlab-read-cache)


(defun gitlab-refresh-projects ()
  (interactive)
  (request "http://git.ctrl/api/v3/projects"
           :params `(("per_page" . "100")
                     ("private_token" . ,gitlab-token))
           :parser (lambda ()
                     (let ((json-array-type 'list))
                       (json-read)))
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
                       (setq gitlab-projects data)
                       (eieio-persistent-save
                        (make-instance 'gitlab-cache
                                       :file gitlab-cache-file
                                       :projects gitlab-projects))
                       (message "List of gitlab projects updated.")))))


(defun gitlab-browse (project &optional relative-path)
  (let ((url (assoc-default 'web_url project)))
    (browse-url (concat url (or relative-path "")))))

(defun gitlab-clone (project)
  (let* ((url (assoc-default 'ssh_url_to_repo project))
         (directory (read-directory-name
                     "Clone to: " gitlab-clone-default-directory nil nil
                     (and (string-match "\\([^./]+\\)\\(\\.git\\)?$" url)
                          (match-string 1 url)))))
    (magit-clone url directory)))


;;; Helm integration

(defface helm-gitlab-namespace-face
  '((t :foreground "grey50"))
  "Face to display the namespace of a project.")

(defface helm-gitlab-project-face
  '((t :weight extra-bold))
  "Face to display the project.")


(defvar helm-gitlab-project-source
  (helm-build-sync-source "Gitlab Projects"
    :action `(("Visit"          . (lambda (c) (gitlab-browse c)))
              ("Files"          . (lambda (c) (gitlab-browse c "/tree/master")))
              ("Issues"         . (lambda (c) (gitlab-browse c "/issues")))
              ("Merge Requests" . (lambda (c) (gitlab-browse c "/merge_requests")))
              ("Clone"          . (lambda (c) (gitlab-clone c))))

    :candidates (lambda ()
                  (sort (mapcar (lambda (project)
                                  (let* ((id (assoc-default 'id project))
                                         (ns (assoc-default 'namespace project))
                                         (namespace (assoc-default 'name ns))
                                         (name (assoc-default 'name project))
                                         (path (assoc-default 'path project)))

                                    (cons (format "%15s / %-50s   %s"
                                                  (propertize namespace 'face 'helm-gitlab-namespace-face)
                                                  (propertize name 'face 'helm-gitlab-project-face)
                                                  (if (string-equal (upcase path) (upcase name))
                                                      ""
                                                    path))

                                          project)))
                                gitlab-projects)
                        (lambda (c1 c2)
                          (string-lessp (car c1) (car c2)))))))



(defvar helm-gitlab-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-r") 'gitlab-refresh-projects)
    map))

(defun helm-gitlab ()
  (interactive)
  (unless gitlab-projects
    (gitlab-refresh-projects))
  (helm :buffer "*helm gitlab*"
        :keymap helm-gitlab-map
        :sources '(helm-gitlab-project-source)))


(provide 'gitlab)
;;; gitlab.el ends here


