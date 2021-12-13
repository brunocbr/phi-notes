;;; phi-notes.el --- Zettelkasten note management -*- lexical-binding:t -*-

;; Copyright (C) 2021  Bruno Conte

;; Author: Bruno Conte <bruno@brunoc.com.br>
;; URL: https://github.com/brunocbr/phi-notes/
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: zettelkasten, writing

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; define a global key somewhere, e. g.:
;; 	(spacemacs/set-leader-keys "Co" 'phi-new-originating-note)

;;; Code:

;;; Core Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Options

(defgroup phi ()
  "Tools for phi notes"
  :group 'tools)


(defcustom phi-header-pre
  "\
---
title:		'%s'  
id:			Φ%s  
"
  "First part of YAML header"
  :type 'string
  :group 'phi)

(defcustom phi-header-post
  "\
tags:	 	%s
...

"
  "Final part of YAML header"
  :type 'string
  :group 'phi)


(defcustom phi-notes-path "~/phi"
  "Path for note files"
  :type 'string
  :group 'phi)

(defcustom phi-counter-file ".phi_counter"
  "Path for counter file"
  :type 'string
  :group 'phi)

(defcustom phi-master-note-id "0000"
  "Default note for the sidebar when no ancestor is found"
  :type 'string
  :group 'phi)

(defcustom phi-originating-symbol "○"
  "Symbol for a originating note's breadcrumb"
  :type 'string
  :group 'phi)

(defcustom phi-parent-symbol "△"
  "Symbol for parent note in breadcrumb"
  :type 'string
  :group 'phi)

(defcustom phi-child-symbol "▽"
  "Symbol for children notes breadcrumb"
  :type 'string
  :group 'phi)

(defcustom phi-tags-field "tags"
  "Field in YAML header for tags"
  :type 'string
  :group 'phi)

(defcustom phi-loc-field "loc"
  "Field in YAML header for location"
  :type 'string
  :group 'phi)

(defcustom phi-citekey-field "citekey"
  "Field in YAML header for cite key"
  :type 'string
  :group 'phi)


(defcustom phi-breadcrumb t
  "Create breadcrumbs"
  :type 'boolean
  :group 'phi)

(defcustom phi-link-left-bracket-symbol "[["
  "Symbol for left bracket"
  :type 'string
  :group 'phi)


(defcustom phi-link-right-bracket-symbol "]]"
  "Symbol for right bracket"
  :type 'string
  :group 'phi)

(defcustom phi-link-left-bracket-symbol-re "\\[\\["
  "RegEx for left bracket"
  :type 'string
  :group 'phi)


(defcustom phi-link-right-bracket-symbol-re "\\]\\]"
  "RegEx for right bracket"
  :type 'string
  :group 'phi)



(defcustom phi-id-regex "[0-9]\\{4,\\}"
  "Regular expression for valid note id"
  :type 'string
  :group 'phi)

(defcustom phi-id-format "%04d"
  "Zero-padded format for note id"
  :type 'string
  :group 'phi)

(defcustom phi-tag-symbol "#"
  "Symbol to prepend tags"
  :type 'string
  :group 'phi)


(defcustom phi-default-file-extension "markdown"
  "Default file extension for notes"
  :type 'string
  :safe 'stringp
  :group 'phi)


(defcustom phi-annotation-tag "ƒ"
  "Tag for the identification of annotation notes"
  :type 'string
  :group 'phi)


(defcustom phi-mode-lighter "Φ"
  "Mode-line indicator for `phi-mode'."
  :type '(choice (const :tag "No lighter" "") string)
  :safe 'stringp
  :group 'binder)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun phi-get-counter ()
  "Increment and return current counter"
  (let ((counter)
        (phi-counter-path (concat phi-notes-path "/" phi-counter-file)))
    (with-temp-buffer
      (insert-file-contents phi-counter-path)
      (setq counter (format phi-id-format (1+ (string-to-number (buffer-string))))))
    (with-temp-file phi-counter-path
      (insert counter))
    counter
    ))

(defun phi-construct-breadcrumb (&optional parent)
  "Construct the breadcrumb for a new note"
  (if (and parent (not (equal parent "")))
      (concat phi-parent-symbol phi-link-left-bracket-symbol
              parent phi-link-right-bracket-symbol)
    (concat phi-originating-symbol)))

(defun phi-get-current-note-id ()
  "Get the current note id"
  (interactive)
  (let ((filename (file-name-nondirectory buffer-file-name)))
    (string-match (concat "^" phi-id-regex) filename)
    (match-string 0 filename)))

(defun phi-matching-file-name (id)
  "Return the first match of a file name starting with id"
  (nth 0 (file-name-all-completions id ""))) ;; blank for current dir instead of phi-notes-path

(defun phi-get-parent-note-id ()
  "Return the id for the parent note"
  (save-excursion
    (goto-char (point-min))
    (if (and (re-search-forward (concat "^" phi-parent-symbol) nil t)
             (looking-at (concat phi-link-left-bracket-symbol-re
                                 "\\(" phi-id-regex "\\)" phi-link-right-bracket-symbol-re)))
        (match-string-no-properties 1))))

(defun phi-visit-parent-note ()
  "Visit the parent note"
  (interactive)
  (let ((id (phi-get-parent-note-id)))
    (if id
        (switch-to-buffer (find-file-noselect (phi-matching-file-name (phi-get-parent-note-id))))
      (error "The current note has no parent"))))

(defun phi-get-next-link-at-point ()
  "Search forward for wikilink and return id"
  (save-excursion
    (if (re-search-forward
         (concat phi-link-left-bracket-symbol-re
                 "\\(" phi-id-regex "\\)" phi-link-right-bracket-symbol-re))
        (match-string-no-properties 1))))

(defun phi-visit-next-link ()
  "Visit the next linked note. `C-u' to visit note in other window."
  (interactive)
  (let ((buffer (find-file-noselect (phi-matching-file-name (phi-get-next-link-at-point)))))
    (if (and (not (equal (current-buffer) phi-sidebar-buffer))
             (equal current-prefix-arg nil)) ; no C-u
        (switch-to-buffer buffer)
      (progn (pop-to-buffer buffer)
             (if (and phi-sidebar-persistent-window
                      (equal current-prefix-arg nil)) ; no C-u
                      (delete-other-windows)))
    (phi-mode))))

(defun phi-get-note-field-contents (field)
  "Return the specified field contents for the current note"
  (save-excursion
    (goto-char (point-min))
    (if  (and (re-search-forward (concat "^" field ":\\s-*") nil t)
              (looking-at (concat ".*$")))
        (replace-regexp-in-string "\s+$" "" (match-string-no-properties 0)))))

(defun phi-create-common-note (id title &optional parent tags citekey loc body)
  "Create a common note buffer"
  (interactive)
  (with-current-buffer (generate-new-buffer "New PHI Note")
    (insert (format phi-header-pre
                    title id))
    (when citekey (insert (concat phi-citekey-field ":\t" (replace-regexp-in-string "\s+$" "" citekey) "  \n")))
    (when loc (insert (concat phi-loc-field ":\t\t" (replace-regexp-in-string "\s+$" "" loc) "  \n")))
    (insert (format phi-header-post tags))
    (insert (phi-construct-breadcrumb parent))
    (insert "\


")
    (when body (insert body))
    (write-file (concat phi-notes-path "/" id " " title "." phi-default-file-extension))
    (phi-mode)
    (current-buffer)))

(defun phi-extract-title-from-body (body)
  "Extract title from the first line of `body'"
  (with-temp-buffer
    (when body (insert body))
    (goto-char (point-min))
    (if (looking-at "#*\s*\\(.\\{1,72\\}\\)")
        (replace-regexp-in-string "\s*#*\s*$" "" (match-string-no-properties 1)))))

(defun phi-new-common-note (&optional body parent)
  "Generate a new common note. `C-u' to create note in other window."
  (interactive)
  (let ((title (read-string "title: " (phi-extract-title-from-body body)))
        (tags (read-string "tags: " (phi-get-note-field-contents phi-tags-field)))
        (citekey (read-string "citekey: " (phi-get-note-field-contents phi-citekey-field)))
        (loc (read-string "loc: " (phi-get-note-field-contents phi-loc-field)))
        (id (phi-get-counter))
        (buffer nil)
        (w nil))
    (unless parent (setq parent (phi-get-current-note-id)))
    (setq buffer (phi-create-common-note id title parent tags
                                         (unless (string= citekey "") citekey) (unless (string= loc "") loc)
                                         body))
    (unless (equal "" parent)
      (insert (concat title " " phi-link-left-bracket-symbol id phi-link-right-bracket-symbol)))
      (setq w (selected-window))
      (if (and (equal current-prefix-arg nil) ; no C-u
               (not (equal (current-buffer) phi-sidebar-buffer)))
          (switch-to-buffer buffer)
        (pop-to-buffer buffer)
        (select-window w))))

(defun phi-new-originating-note ()
  "Create an originating note. `C-u' to create note in other window."
  (interactive)
  (phi-new-common-note nil "")
  )

(defun phi-new-descendant-note ()
  "Create a child linked note. `C-u' to create note in other window."
  (interactive)
  (phi-new-common-note))

(defun phi-kill-to-new-note (start end)
  "Kill region to linked note. `C-u' to create note in other window."
  (interactive "r")
  (let ((body (buffer-substring-no-properties start end))
        (buffer (current-buffer)))
    (phi-new-common-note body)
    (with-current-buffer buffer
      (kill-region start end))))

(defun phi-yank-to-new-note ()
  "Yank to linked note. `C-u' to create note in other window."
  (interactive)
  (let ((body (substring-no-properties (car kill-ring))))
    (phi-new-common-note body)))


;; Sidebar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup phi-sidebar ()
  "Options for `phi-sidebar-mode'."
  :group 'phi)

(defcustom phi-sidebar-resize-window-step
  4
  "Integer of columns by which to resize window.
Used by `phi-sidebar-shrink-window' and `phir-sidebar-enlarge-window'."
  :type 'integer
  :safe 'integerp
  :group 'phi-sidebar)

(defcustom phi-sidebar-text-scale-set
  0
  "Text scale for sidebar buffer"
  :type 'integer
  :group 'phi-sidebar)

(defcustom phi-sidebar-display-alist
  '((side . left)
    (window-width . 45)
    (slot . -1))
  "Association list used to display PHI sidebar buffer.
See `display-buffer-in-side-window' for example options."
  :type 'alist
  :safe (lambda (value)
          (and (listp value)
               (seq-every-p 'consp value)))
  :group 'phi-sidebar)

(defcustom phi-sidebar-persistent-window
  t
  "When non-nil, sidebar will persist when calling `delete-other-windows'.
This marks `no-delete-other-windows' window parameter as non-nil.
Use `phi-toggle-sidebar' or `quit-window' to close the sidebar."
  :type 'boolean
  :safe 'booleanp
  :group 'phi-sidebar)

(defcustom phi-sidebar-olivetti-width
  38
  "olivetti-mode column width for sidebar buffer"
  :type 'integer
  :group 'phi-sidebar)

(defvar phi-sidebar-buffer nil)

(defun phi-sidebar-adjust-buffer (buffer)
  (with-current-buffer buffer
    (phi-mode)
    (if phi-sidebar-olivetti-width
        (olivetti-set-width phi-sidebar-olivetti-width))
    (text-scale-set phi-sidebar-text-scale-set)
    (phi-buttonize-buffer))
  buffer)

(defun phi-sidebar-create-buffer (id)
  (let ((file (phi-matching-file-name id)))
        (if file
            (setq phi-sidebar-buffer (phi-sidebar-adjust-buffer
                                      (find-file-noselect file)))
          (error "No matching file for note id %s" id))))

(defun phi-sidebar-create-window (id)
  (display-buffer-in-side-window (phi-sidebar-create-buffer id)
                                 (append phi-sidebar-display-alist
                                         (when phi-sidebar-persistent-window
                                           (list '(window-parameters (no-delete-other-windows . t)))))))

(defun phi-sidebar-with-parent ()
  "Open PHI Sidebar with parent note"
  (interactive)
  (phi-sidebar-create-window (or (phi-get-parent-note-id) phi-master-note-id)))

(defun phi-toggle-sidebar ()
  "Toggle visibility of PHI Sidebar"
  (interactive)
  (if (and phi-sidebar-buffer (window-live-p (get-buffer-window phi-sidebar-buffer)))
      (progn (delete-window (get-buffer-window phi-sidebar-buffer))
           (setq phi-sidebar-buffer nil))
    (phi-sidebar-with-parent)))

(defun phi-focus-sidebar ()
  "Focus on PHI Sidebar"
  (interactive)
  (select-window (phi-sidebar-with-parent)))

(define-button-type 'phi-linked-note
  'follow-link t
  'action #'phi-find-file-button)

(defun phi-find-file-button (button)
  (let ((buffer (find-file-noselect (phi-matching-file-name
                                     (buffer-substring (button-start button) (button-end button))))))
    (pop-to-buffer buffer)
    (phi-mode)
    (if phi-sidebar-persistent-window (delete-other-windows))))

(defun phi-buttonize-buffer ()
  "Turn all wiki links into buttons"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (concat phi-link-left-bracket-symbol-re
                    "\\(" phi-id-regex "\\)" phi-link-right-bracket-symbol-re) nil t)
      (make-button (match-beginning 1) (match-end 1) :type 'phi-linked-note))))

(defvar phi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c ;") #'phi-toggle-sidebar)
    (define-key map (kbd "C-c :") #'phi-focus-sidebar)
    (define-key map (kbd "C-c n d") #'phi-new-descendant-note)
    (define-key map (kbd "C-c n k") #'phi-kill-to-new-note)
    (define-key map (kbd "C-c n y") #'phi-yank-to-new-note)
    (define-key map (kbd "C-c u") #'phi-visit-parent-note)
    (define-key map (kbd "C-c j") #'phi-visit-next-link)
    map)
  "Main mode map for `phi-mode'.")

(define-minor-mode phi-mode
  "PHI note minor mode"
  :lighter phi-mode-lighter
  :keymap phi-mode-map
  )

(provide 'phi-notes)
