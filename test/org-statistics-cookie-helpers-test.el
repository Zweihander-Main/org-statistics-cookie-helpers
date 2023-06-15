;;; org-statistics-cookie-helpers-test.el --- Tests for org-statistics-cookie-helpers-*-lexical-binding:t-*-

;; Copyright (C) 2021, Zweihänder <zweidev@zweihander.me>
;;
;; Author: Zweihänder
;; Keywords: outlines
;; Homepage: https://github.com/Zweihander-Main/org-statistics-cookie-helpers
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))

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

;; Tests for org-statistics-cookie-helpers

;;; Code:

(require 'buttercup)
(require 'org-statistics-cookie-helpers)

(defvar org-statistics-cookie-helpers-test-temp-buffer nil)

(describe "org-statistics-cookie-helpers"
  (before-each
    (setq org-statistics-cookie-helpers-test-temp-buffer
          (generate-new-buffer "temp-buffer"))
    (with-current-buffer org-statistics-cookie-helpers-test-temp-buffer
      (org-mode)
      (insert "* Heading 1")
      (goto-char (point-min))
      (org-statistics-cookie-helpers-insert-cookies)))

  (after-each
    (kill-buffer org-statistics-cookie-helpers-test-temp-buffer))

  (it "should insert [%] cookie at the end of the line"
    (with-current-buffer org-statistics-cookie-helpers-test-temp-buffer
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "* Heading 1 [100%]")))

  (it "should find the statistics cookie on the line and return it as a plist"
    (with-current-buffer org-statistics-cookie-helpers-test-temp-buffer
      (let ((cookie (org-statistics-cookie-helpers-find-cookies)))
        (expect cookie :to-equal
                '(:begin 13 :end 19 :value "[100%]" :post-blank 0 :type %)))))

  (it "should toggle between [/] and [%] type statistics cookies on the line"
    (with-current-buffer org-statistics-cookie-helpers-test-temp-buffer
      (org-statistics-cookie-helpers-toggle-cookies)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "* Heading 1 [0/0]")
      (org-statistics-cookie-helpers-toggle-cookies)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "* Heading 1 [100%]")))

  (it "should delete the statistics cookie on the line"
    (with-current-buffer org-statistics-cookie-helpers-test-temp-buffer
      (org-statistics-cookie-helpers-delete-cookies)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "* Heading 1"))))


(provide 'org-statistics-cookie-helpers-test)

;; Local Variables:
;; coding: utf-8
;; End:

;;; org-statistics-cookie-helpers-test.el ends here
