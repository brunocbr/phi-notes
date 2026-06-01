;;; phi-omnifocus.el --- Integration with OmniFocus for Org and Markdown

;; Author: Bruno Conte <bruno@brunoc.com.br>
;; Version: 0.1
;; Created: 2023-10-01
;; Keywords: org, markdown, omnifocus
;; URL: https://github.com/brunocbr/phi-notes/phi-omnifocus.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; This package allows you to send task headers from Org or
;; Markdown files to OmniFocus using the x-callback URL scheme.

;;; Code:

(defgroup phi-omnifocus nil
  "Integration with OmniFocus."
  :prefix "phi-omnifocus-"
  :group 'external)

(defcustom phi-omnifocus-x-callback-url "omnifocus://x-callback-url/"
  "Base URL for x-callback calls to OmniFocus."
  :type 'string
  :group 'phi-omnifocus)

(defun phi-omnifocus-send-task ()
  "Sends the task under the cursor to OmniFocus, supporting Org mode and Markdown."
  (interactive)
  (let ((task-text (phi-omnifocus-get-task-text))
        (current-url (when (and (boundp 'phi-mode) phi-mode)
                       (phi-get-current-note-url))))
    (if task-text
        (let* ((note (when current-url (concat "Source: " current-url)))
               (url (format "%sadd?name=%s&note=%s"
                            phi-omnifocus-x-callback-url
                            (url-encode-url task-text)
                            (url-encode-url note))))
          (browse-url url))
      (message "No task found on the current line."))))

(defun phi-omnifocus-sanitize-task-text (line)
  "Sanitize LINE by removing Markdown list markers and tags like #todo #abcd."
  (let ((sanitized-line line))
    ;; Remove Markdown list markers
    (setq sanitized-line (replace-regexp-in-string "^\\s-*[-+*]\\s-*" "" sanitized-line))
    (setq sanitized-line (replace-regexp-in-string "^\\s-*\\[\\s-*\\]\\s-*" "" sanitized-line)) ; Remove unmarked tasks
    (setq sanitized-line (replace-regexp-in-string "^\\s-*\\[X]\\s-*" "" sanitized-line)) ; Remove marked tasks
    ;; Remove tags
    (setq sanitized-line (replace-regexp-in-string "#[A-Za-z0-9_]+" "" sanitized-line))
    ;; Trim whitespace
    (string-trim sanitized-line)))

(defun phi-omnifocus-get-task-text ()
  "Gets the task text based on the current mode (Org or Text/Markdown)."
  (if (equal major-mode 'org-mode)
      (if (org-at-heading-p)
          (org-get-heading t t)
        nil)
    (let ((line (thing-at-point 'line t)))
      (phi-omnifocus-sanitize-task-text line))))

(global-set-key (kbd "C-c o") 'phi-omnifocus-send-task)

(provide 'phi-omnifocus)

;;; phi-omnifocus.el ends here
