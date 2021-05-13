;;; org-statistics-cookie-helpers.el --- WIP-*-lexical-binding:t-*-

;; Copyright (C) 2021, Zweihänder <zweidev@zweihander.me>
;;
;; Author: Zweihänder
;; Keywords: org-mode
;; Homepage: https://github.com/Zweihander-Main/org-statistics-cookie-helpers
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; WIP
;;
;;; Code:

(require 'org)
(require 'org-element)

(defgroup org-statistics-cookie-helpers nil
  "Customization for 'org-statistics-cookie-helpers' package."
  :group 'org
  :prefix "org-statistics-cookie-helpers-")

(defun org-statistics-cookie-helpers-insert-cookies (&optional type)
  "Insert statistics cookie of optional TYPE % (default) or /."
  (save-excursion
    (let ((cur-tags-string (org-get-tags-string)))
      (if (not (eq cur-tags-string ""))
          (when (org-back-to-heading t)
            (re-search-forward org-tag-line-re)
            (goto-char (- (match-beginning 1) 1)))
        (end-of-line))
      (insert (concat " " (if (eq type '/) "[/]" "[%]")))
      (org-update-statistics-cookies nil))))

(defun org-statistics-cookie-helpers-find-cookies ()
  "Find statistics cookies on line and return as plist."
  (save-excursion
    (beginning-of-line)
    (let ((end-point (save-excursion (end-of-line) (point)))
          (search-point (point))
          (cookie nil))
      (while (and (not cookie) search-point)
        (setq search-point (re-search-forward "\\[" end-point t))
        (when search-point
          (forward-char -1)
          (setq cookie (cadr (org-element-statistics-cookie-parser)))
          (forward-char 1)))
      (if cookie
          (plist-put cookie :type (if (eq (string-match-p "%" (plist-get cookie :value)) nil) '/ '%))
        cookie))))

(defun org-statistics-cookie-helpers-delete-cookies ()
  "Delete statistics cookies on line."
  (let ((cookie (zwei/org-find-statistics-cookies)))
    (when cookie
      (delete-region (plist-get cookie :begin) (plist-get cookie :end))
      (save-excursion
        (end-of-line)
        (when (eq (char-before) ? )
          (delete-char -1))))))

;;;###autoload
(defun org-statistics-cookie-helpers-toggle-cookies ()
  "Toggle between [/] and [%] type statistics cookies on line."
  (interactive)
  (let ((type (plist-get (zwei/org-find-statistics-cookies) :type)))
    (zwei/org-delete-statistics-cookies)
    (cond ((eq type '%) (zwei/org-insert-statistics-cookies '/))
          ((eq type '/) (zwei/org-insert-statistics-cookies '%)))))

(provide 'org-statistics-cookie-helpers)

;; Local Variables:
;; coding: utf-8
;; End:

;;; org-statistics-cookie-helpers.el ends here
