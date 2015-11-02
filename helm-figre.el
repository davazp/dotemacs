;;; helm-figre.el --- A Helm helper for common find-grep patterns  -*- lexical-binding: t; -*-

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

(require 'cl-lib)

(defun helm-figre-find (regexp fn &optional name exclude dir)
  "Find all the lines matching REGEXP in the files in a directory and
execute a function of the result.

For every match, execute FN on the matched string a list of
elements of the form

   (RESULT FILENAME LINE)

where RESULT is the returned value of FN on the match."
  (with-temp-buffer
    (let* ((name-args (if name `("-name" ,name)))
           (exclude-args (if exclude `("-not" "-path" ,exclude)))
           (args `(,(expand-file-name (or dir default-directory))
                   "-maxdepth" "10"
                   "-type" "f"
                   ,@exclude-args
                   ,@name-args
                   "-exec" "grep" "-HIno" "-e" ,regexp "{}" "+")))
      ;; Find the tests with the grepn utility
      (apply #'call-process "find" nil t nil args))
    ;; Parse the result into a list
    (let ((result nil))
      (cl-block nil
        (goto-char (point-min))
        (while t
          (when (looking-at "^\\(.*\\):\\([[:digit:]]+\\):\\(.*\\)$")
            (let ((file (match-string 1))
                  (lineno (string-to-number (match-string 2)))
                  (line (match-string 3)))
              (when (string-match regexp line)
                (let ((item (funcall fn line)))
                  (when item
                    (push (list item file lineno) result))))))
          (when (/= 0 (forward-line))
            (cl-return))))
      result)))


(cl-defun helm-figre-source (title &key candidates printer)
  "Build a source named TITLE for CANDIDATES, where CANDIDATES is
a subset of the return value of `helm-figre-find'."
  (helm-build-sync-source title
    :candidates candidates
    :candidate-transformer
    (lambda (candidates)
      (mapcar (lambda (candidate)
                (cons (concat (propertize
                               (concat (file-relative-name (cadr candidate)) ":")
                               'face 'helm-grep-file)
                              " "
                              (funcall printer (car candidate)))
                      (cdr candidate)))
              candidates))
    :action (helm-make-actions
             "Jump" (lambda (candidate)
                      (find-file (car candidate))
                      (goto-line (cadr candidate))
                      (helm-highlight-current-line nil nil nil nil 'pulse)))))


(provide 'helm-figre)
;;; helm-figre.el ends here
