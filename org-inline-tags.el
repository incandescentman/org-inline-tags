;;; org-inline-tags.el --- Insert and search for inline tags in org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2023 Jay Dixit

;; Author: Jay Dixit <jaydixit.work@gmail.com>
;; URL: https://github.com/incandescentman/org-inline-tags
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (org "9.1"))
;; Keywords: org, inline, tags, plain lists

;; This file is not part of GNU Emacs.

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

;; This package provides a simple way to insert and search for inline tags in org-mode.

;;; Code:

(require 'org)

(defface org-inline-tags-face
  '((t (:foreground "orange" :weight bold)))
  "Face for custom inline tags in plain list items.")

(font-lock-add-keywords 'org-mode
  '(("#\\(\\w+\\(-\\w+\\)*\\)" 0 'org-inline-tags-face)))


;;;###autoload
(defun org-inline-tags-search (tag)
  "Search for TAG in the current org file."
  (org-search-view nil (concat "#" tag)))

;;;###autoload
(defun org-inline-tags-search-buffer (tag)
  "Search for TAG in the current buffer."
  (consult-line (concat "#" tag)))

;;;###autoload
(defun org-inline-tags-search-project-wide ()
  "Search for inline TAG project-wide using counsel-projectile-ag if available, otherwise use occur."
  (interactive)
  (message "Enter tag to search for (Please include the # sign at the beginning): ")
  (if (fboundp 'counsel-projectile-ag)
      (counsel-projectile-ag)
    (occur)))

;;;###autoload
(defun org-inline-tags-insert ()
  "Prompt the user to choose a tag and insert it at the current cursor position."
  (interactive)
  (let* ((tag-alist '((?r . "review")
                      (?b . "book")
                      (?t . "todo")
                      (?u . "urgent")
                      (?p . "tweet")
                      (?i . "insight")
                      (?c . "cook-ideas-over-time")))
         (selected-key (read-char "Choose a tag:\n
r: review
b: book
t: todo
u: urgent
p: tweet
i: insight
c: cook-ideas-over-time\n")))
    (setq selected-tag (cdr (assoc selected-key tag-alist)))
    (if selected-tag
        (insert (format " #%s" selected-tag))
      (error "Invalid tag selection"))))

(provide 'org-inline-tags)

;;; org-inline-tags.el ends here
