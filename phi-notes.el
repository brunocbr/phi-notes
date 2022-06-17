;;; phi-notes.el --- Zettelkasten note management -*- lexical-binding:t -*-

;; Copyright (C) 2021-2022  Bruno Conte

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
;;
;; for helm-bibtex support:
;;  (helm-bibtex-helmify-action bibtex-completion-create-phi-note helm-bibtex-create-phi-note)
;;
;;  (helm-add-action-to-source
;;   "Create PHI bibliographical annotation" 'helm-bibtex-create-phi-note
;;   helm-source-bibtex 1)


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


(defcustom phi-repository-alist
  nil
  "Note repositories (\"NAME\" \"PATH\" \"MASTERID\") ..."
  :type '(alist :key-type (symbol :tag "Name")
                :value-type (list (string :tag "Path")
                                  (string :tag "Master note id")))
  :group 'phi)

(defcustom phi-counter-file ".counter"
  "Path for counter file"
  :type 'string
  :group 'phi)

(defcustom phi-default-master-note-id "0000"
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

(defcustom phi-project-field "proj"
  "Field in YAML header for project note id"
  :type 'string
  :group 'phi)

(defcustom phi-origin-field "origin"
  "Field in YAML header for the URL callback of for an externally maintained source."
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

(defcustom phi-id-timestamp-format "%Y%m%d%H%M"
  "Format for timestamp IDs"
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

(defcustom phi-tag-regex "#[0-9a-zA-Z\\./-_]\\+"
  "RegEx to identify a valid tag"
  :type 'string
  :group 'phi)

(defcustom phi-project-tag "proj"
  "Tag identification for projects"
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

(defcustom phi-url-protocol "x-phi"
  "Custom URL protocol for notes."
  :type 'stringp
  :safe 'stringp
  :group 'phi)

(defgroup phi-tlg ()
  "TLG (Thesaurus Linguae Graecae) support"
  :group 'phi)

(defcustom phi-tlg-ref-field
  "ref_tlg"
  "Field for TLG reference of the sort 0059:031"
  :type 'string
  :group 'phi-tlg)

(defcustom phi-tlg-section-field
  "section"
  "Field for TLG section"
  :type 'string
  :group 'phi-tlg)

(defcustom phi-tlg-line-field
  "line"
  "Field for TLG line"
  :type 'string
  :group 'phi-tlg)


(defcustom phi-mode-lighter "Φ"
  "Mode-line indicator for `phi-mode'."
  :type '(choice (const :tag "No lighter" "") string)
  :safe 'stringp
  :group 'binder)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun phi-initialize-counter (&optional dir value)
  "Prompts for directory and value, and create a counter"
  (interactive)
  (let* ((default-directory (or dir (read-directory-name "Select the directory where to create or reset the counter: ")))
         (counter (read-string "Set a counter value or type \"TIMESTAMP\": " (or value "0000"))))
    (make-directory default-directory t)
    (with-temp-file phi-counter-file
      (insert counter))))

(defun phi--get-current-counter-from-file (path)
  (if (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (if (string= (buffer-string) "TIMESTAMP")
            "TIMESTAMP"
          (format phi-id-format (string-to-number (buffer-string)))))))

;;;###autoload
(defun phi-add-repository ()
  "Helper function for intializing a repository and customizing `phi-repository-alist'"
  (interactive)
  (let* ((repo-name (read-string "Repository name: "))
         (existing-repo-dir (car (cdr (assoc repo-name phi-repository-alist))))
         (repo-dir (read-directory-name "Select the directory for the note repository"
                                        (or existing-repo-dir default-directory)))
         (counter-path (concat repo-dir "/" phi-counter-file))
         (counter-value (phi--get-current-counter-from-file counter-path))
         (entry (assoc repo-name phi-repository-alist)))
    (phi-initialize-counter repo-dir counter-value)
    (if entry (setcdr entry (cons repo-dir (cons phi-default-master-note-id nil)))
      (progn
        (setq params (cons repo-name (cons repo-dir (cons phi-default-master-note-id nil))))
        (if phi-repository-alist
            (push params (cdr (last phi-repository-alist)))
          (setq phi-repository-alist (cons params nil)))))
    (customize-variable 'phi-repository-alist)))

(defun phi--prompt-for-notes-path ()
  (unless phi-repository-alist (error "No repository set! Use `phi-add-repository'."))
  (if (equal (length phi-repository-alist) 1)
      (cadr (car phi-repository-alist))
    (cadr (assoc (completing-read "Select a note repository: "
                                  phi-repository-alist) phi-repository-alist))))

(defun phi--enforce-directory ()
  "Try to make sure we are at the right directory"
  (setq default-directory
        (file-name-directory buffer-file-name)))

(defun phi-notes-path (&optional usecontext)
  "Get the path for notes (usually the default directory).

If optional USECONTEXT is not nil, enforce setting the default directory to the current note's directory"
  (if usecontext ;; (and usecontext phi-mode)
      (phi--enforce-directory)) ;; enforce directory when visiting a PHI note
  (if (file-exists-p phi-counter-file)
      default-directory
    (setq default-directory (phi--prompt-for-notes-path))))

(defun phi--get-counter-path ()
  "Get the full path for the counter file"
  (concat (phi-notes-path) "/" phi-counter-file))

(defun phi-get-counter ()
  "Increment and return current counter"
  (let* ((phi-counter-path (phi--get-counter-path))
         (current-counter (phi--get-current-counter-from-file phi-counter-path)))
    (if (string= current-counter "TIMESTAMP")
        (format-time-string phi-id-timestamp-format (current-time))
      (let ((counter))
        (setq counter (format phi-id-format
                              (1+ (string-to-number (phi--get-current-counter-from-file phi-counter-path)))))
        (with-temp-file phi-counter-path
          (insert counter))
        counter))))

(defun phi-construct-breadcrumb (&optional parent)
  "Construct the breadcrumb for a new note"
  (if (and parent (not (equal parent "")))
      (concat phi-parent-symbol phi-link-left-bracket-symbol
              parent phi-link-right-bracket-symbol)
    (concat phi-originating-symbol)))

(defun phi-id-to-wikilink (id)
  "Return a wikilink for the given `id'"
  (concat phi-link-left-bracket-symbol id phi-link-right-bracket-symbol))

(defun phi--get-note-id-from-file-name (filename)
  (string-match (concat "^" phi-id-regex) filename)
  (match-string 0 filename))

(defun phi-get-current-note-id ()
  "Get the current note id"
  (interactive)
  (let ((filename (file-name-nondirectory buffer-file-name)))
    (phi--get-note-id-from-file-name filename)))

(defun phi-get-current-note-title ()
  "Get current note title from its filename"
  (let ((filename (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
    (if (string-match (concat "^\\(" phi-id-regex "\\)\s+\\(.*\\)$") filename)
        (match-string-no-properties 2 filename))))

(defun phi-get-current-note-tlg-fields ()
  "Get the TLG fields for the current note in as plist"
  (let ((data  (list :tlg-ref (phi-get-note-field-contents phi-tlg-ref-field)
             :tlg-section (phi-get-note-field-contents phi-tlg-section-field)
             :tlg-line (phi-get-note-field-contents phi-tlg-line-field))))
    data))

;;;###autoload
(defun phi-buffer-repository (&optional buf)
  "Get the repository name for buffer `BUF', or the current buffer if `nil'."
  (let ((filename (buffer-file-name buf)))
    (if filename
        (cdr (assoc (directory-file-name (file-name-directory filename))
                    (mapcar (lambda (x) (cons
                                         (directory-file-name (expand-file-name (cadr x))) (first x))) phi-repository-alist))))))

(defun phi-matching-file-name (id &optional usecontext)
  "Return the first match of a file name starting with ID.

If USECONTEXT is not nil, enforce setting the current directory to the note's directory."
  (nth 0 (file-name-all-completions id (phi-notes-path usecontext)))) ;; blank for current dir instead of phi-notes-path

(defun phi-wiki-link-re ()
  (concat phi-link-left-bracket-symbol-re
          "\\(" phi-id-regex "\\)" phi-link-right-bracket-symbol-re))

(defun phi-get-parent-note-id ()
  "Return the id for the parent note"
  (save-excursion
    (goto-char (point-min))
    (if (and (re-search-forward (concat "^" phi-parent-symbol) nil t)
             (looking-at (concat phi-link-left-bracket-symbol-re
                                 "\\(" phi-id-regex "\\)" phi-link-right-bracket-symbol-re)))
        (match-string-no-properties 1))))

(defun phi--is-project-p (id)
  (string-match-p (concat phi-tag-symbol phi-project-tag) (or (phi--get-tags-from-note-as-str id) "")))

(defun phi-get-ancestor-project-id (id)
  (and id
       (or (and (phi--is-project-p id) id)
           (with-current-buffer (find-file-noselect (phi-matching-file-name id))
             (or (phi-get-linked-project-note-id)
                 (phi-get-ancestor-project-id (phi-get-parent-note-id)))))))

(defun phi-get-linked-project-note-id ()
  "Return the id for the project this note is linked to"
  (let ((project (phi-get-note-field-contents phi-project-field)))
    (if (and project (string-match (concat phi-link-left-bracket-symbol-re "\\(" phi-id-regex "\\)" phi-link-right-bracket-symbol-re) project))
        (match-string-no-properties 1 project)
      (or project nil))))

;;;###autoload
(defun phi-find-note (id repo)
  "Visit note `ID' in repository `REPO'"
  (setq default-directory (cadr (assoc repo phi-repository-alist)))
  (let ((filename (phi-matching-file-name id)))
    (if filename
        (switch-to-buffer (find-file-noselect filename))
      (error (format "Invalid note ID %s" id)))))

;;;###autoload
(defun phi-visit-parent-note ()
  "Visit the parent note"
  (interactive)
  (let ((id (phi-get-parent-note-id)))
    (if id
        (switch-to-buffer (find-file-noselect (phi-matching-file-name (phi-get-parent-note-id) t)))
      (error "The current note has no parent"))))

(defun phi-get-next-link-at-point ()
  "Search forward for wikilink and return id"
  (save-excursion
    (if (re-search-forward
         (concat phi-link-left-bracket-symbol-re
                 "\\(" phi-id-regex "\\)" phi-link-right-bracket-symbol-re))
        (match-string-no-properties 1))))

;;;###autoload
(defun phi-visit-next-link ()
  "Visit the next linked note. `C-u' to visit note in other window."
  (interactive)
  (let ((buffer (find-file-noselect (phi-matching-file-name (phi-get-next-link-at-point) t))))
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
    (if (looking-at-p "---") (forward-line 1))
    (let ((target-pos (point))
          (yaml-end-pos (search-forward-regexp "^\\(\\.\\.\\.\\|---\\)" nil t)))
      (goto-char target-pos)
      (if  (and (re-search-forward (concat "^" field ":\\s-*") yaml-end-pos t)
                (not (looking-at "^\\(\\.\\.\\.\\|---\\)"))
                (looking-at (concat ".*$")))
          (replace-regexp-in-string "\s+$" "" (match-string-no-properties 0))))))

(defun phi--get-tags-from-note-as-str (id)
  "Get a string of the tags from a given note `ID'"
  (let ((file (concat (phi-notes-path) "/" (phi-matching-file-name id)))
        contents)
    (with-current-buffer (get-buffer-create "*PHI temp*")
      (insert-file-contents file nil nil nil t)
      (setq contents (phi-get-note-field-contents phi-tags-field)))))

(defun phi--get-tags-from-file-as-str (file)
  "Get a string of the tags from `FILE'"
  (let (contents)
    (with-current-buffer (get-buffer-create "*PHI temp*")
      (insert-file-contents file nil nil nil t)
      (setq contents (phi-get-note-field-contents phi-tags-field)))
    (kill-buffer "*PHI temp*")
    contents))

(defun phi-has-tag (tag)
  (let ((tags (phi-get-note-field-contents phi-tags-field)))
    (not (null (string-match-p (concat phi-tag-symbol tag "\\b") tags)))))

(defun phi-set-note-field-contents (field value)
  "Insert or update a field in the note's YAML frontmatter."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; Find the YAML frontmatter block boundaries, or otherwise throw an error.
    (if (looking-at-p "---") (forward-line 1) (error "No YAML frontmatter found"))
    (let ((target-pos (point))
          (yaml-end-pos (search-forward-regexp "^\\(\\.\\.\\.\\|---\\)" nil t)))
      ;; Return after search-forward moved point.
      (goto-char target-pos)
      (if (not yaml-end-pos)
          (error "YAML block end boundary not found")
        ;; If YAML block is found, update or insert the date entry
        (progn
          (if (search-forward-regexp (concat "^" field ":") yaml-end-pos t)
              (delete-region (line-beginning-position) (line-end-position))
            (progn
              (goto-char yaml-end-pos)
              (beginning-of-line)
              (newline)
              (backward-char)))
          (insert (format "%s:\t\t%s" field value)))))))

(defun phi--without-quotes (s)
  "Lame function to remove quotes"
  (string-match "^'\\(.*\\)'$" s)
  (match-string-no-properties 1 s))

(defun phi-rename-current-note (&optional title)
  "Rename the current note to `TITLE' or ask for one."
  (interactive)
  (let ((new-title (or title
                       (read-string "New title: " (phi-get-current-note-title))))
        (dir (file-name-directory (expand-file-name (buffer-file-name)))))
    (when new-title
      (phi-set-note-field-contents "title" (format "'%s'" new-title))
      (let ((new-file-name (concat dir (phi-get-current-note-id) " " new-title
                                   "." (file-name-extension (buffer-file-name)))))
        (rename-file (buffer-file-name) new-file-name)
        (set-visited-file-name new-file-name)))))

(defun phi-create-common-note (&rest args)
  "Create a common note buffer"
  (interactive)
  (let ((id (plist-get args :id))
        (title (plist-get args :title))
        (parent (plist-get args :parent))
        (tags (plist-get args :tags))
        (citekey (plist-get args :citekey))
        (loc (plist-get args :loc))
        (body (plist-get args :body))
        (tlg-fields (plist-get args :tlg-fields)))
    (with-current-buffer (generate-new-buffer "New PHI Note")
      (insert (format phi-header-pre
                      title id))
      (when citekey (insert (concat phi-citekey-field ":\t" (replace-regexp-in-string "\s+$" "" citekey) "  \n")))
      (when loc (insert (concat phi-loc-field ":\t\t" (replace-regexp-in-string "\s+$" "" loc) "  \n")))
      (when tlg-fields
        (let ((tlg-ref (plist-get tlg-fields :tlg-ref))
              (tlg-section (or (plist-get tlg-fields :tlg-section) ""))
              (tlg-line (or (plist-get tlg-fields :tlg-line) "")))
          (insert (concat phi-tlg-ref-field ":\t" (replace-regexp-in-string "\s+$" "" tlg-ref) "  \n"))
          (insert (concat phi-tlg-section-field ":\t" (replace-regexp-in-string "\s+$" "" tlg-section) "  \n"))
          (insert (concat phi-tlg-line-field ":\t\t" (replace-regexp-in-string "\s+$" "" tlg-line) "  \n"))))
      (insert (format phi-header-post tags))
      (insert (phi-construct-breadcrumb parent))
      (insert "\


")
      (when body (insert body))
      (write-file (concat (phi-notes-path) "/" id " " title "." phi-default-file-extension))
      (phi-mode)
      (current-buffer))))

(defun phi-extract-title-from-body (body)
  "Extract title from the first line of `body'"
  (with-temp-buffer
    (when body (insert body))
    (goto-char (point-min))
    (if (looking-at "#*\s*\\(.\\{1,72\\}\\)")
        (replace-regexp-in-string "\s*#*\s*$" "" (match-string-no-properties 1)))))

(defun phi-new-common-note (&optional body parent insert-title)
  "Generate a new common note. `C-u' to create note in other window."
  (interactive)
  (let* ((title (read-string "title: " (phi-extract-title-from-body body)))
        (tags (read-string "tags: " (phi-get-note-field-contents phi-tags-field)))
        (citekey (read-string "citekey: " (phi-get-note-field-contents phi-citekey-field)))
        (loc (read-string "loc: " (phi-get-note-field-contents phi-loc-field)))
        (tlg-fields (phi-get-current-note-tlg-fields))
        (id (phi-get-counter))
        (buffer nil)
        (w nil))
    (unless parent (setq parent (phi-get-current-note-id)))
    (let ((buffer (phi-create-common-note :id id :title title :parent parent :tags tags
                                          :citekey (unless (string= citekey "") citekey)
                                          :loc (unless (string= loc "") loc)
                                          :body body
                                          :tlg-fields (when (cadr tlg-fields) tlg-fields))))
      (unless (string-equal "" parent)
        (insert (concat (when insert-title (concat title " "))
                        phi-link-left-bracket-symbol id phi-link-right-bracket-symbol)))
      (setq w (selected-window))
      (if (and (equal current-prefix-arg nil) ; no C-u
               (not (equal (current-buffer) phi-sidebar-buffer)))
          (switch-to-buffer buffer)
        (pop-to-buffer buffer)
        (select-window w)))))

;;;###autoload
(defun phi-new-originating-note (&optional body)
  "Create an originating note. `C-u' to create note in other window."
  (interactive)
  (setq default-directory (phi--prompt-for-notes-path))
  (phi-new-common-note body ""))

;;;###autoload
(defun phi-new-descendant-note ()
  "Create a child linked note. `C-u' to create note in other window."
  (interactive)
  (phi--enforce-directory)
  (phi-new-common-note nil nil t))

;;;###autoload
(defun phi-kill-to-new-note (start end)
  "Kill region to linked note. `C-u' to create note in other window."
  (interactive "r")
  (let ((body (buffer-substring-no-properties start end))
        (buffer (current-buffer)))
    (phi--enforce-directory)
    (phi-new-common-note body (phi-get-current-note-id) t)
    (with-current-buffer buffer
      (kill-region start end))
    (pop-to-buffer buffer)))

;;;###autoload
(defun phi-yank-to-new-note ()
  "Yank to linked note. `C-u' to create note in other window."
  (interactive)
  (let ((body (substring-no-properties (car kill-ring))))
    (phi--enforce-directory)
    (phi-new-common-note body nil nil)))

(defun phi--search-forward-pp ()
  (re-search-forward "(\\(p\\{1,2\\}\.\\) +\\([0-9a-zA-Z\-,;\. ]+\\))" nil t))

(defun phi--pandoc-cite (citekey &optional loc)
  (if (and (not (null loc)) (not (string= loc "0")) (not (string= loc "")))
      (format "[@%s, %s]" citekey loc)
  (format "[@%s]" citekey)))

(defun phi-pp-to-pandoc-cite (str citekey &optional loc)
  (let ((found nil))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (save-excursion
        (while (phi--search-forward-pp)
          (setq found t)
          (replace-match (format "[@%s, %s %s]" citekey (match-string 1)
                                 (match-string 2)))))
      (unless found
        (goto-char (point-max))
        (re-search-backward "\\." nil t) ;; best place do insert a citation should be before the last period
        (insert (concat " " (phi--pandoc-cite citekey loc))))
      (buffer-string))))

(defun phi-smart-copy-region (start end)
  "Copy region to kill ring formatted for later quoting."
  (interactive "r")
  (let ((citekey (phi-get-note-field-contents phi-citekey-field))
        (loc (phi-get-note-field-contents phi-loc-field))
        (id (phi-get-current-note-id))
        (region (filter-buffer-substring start end)))
    (let ((str (format "[[%s]] %s"
                      id (if citekey (phi-pp-to-pandoc-cite region citekey loc) region))))
    ;; reproduce copy-region-as-kill
      (if (eq last-command 'phi-smart-copy-region)
          (kill-append str (< end beg))
        (kill-new str))))
  (setq deactivate-mark t)
  nil)

(defun phi-smart-copy-ref-at-point ()
  "Copy the next page reference at point."
  (interactive)
  (phi--search-forward-pp)
  (phi-smart-copy-region (- (match-beginning 1) 1) (+ (match-end 2) 1)))

(defun phi-copy-wikilink ()
  "Copy a wikilink to the current note to the kill buffer. C-u to copy URL with `phi-url-protocol'."
  (interactive)
  (if (equal current-prefix-arg nil)
      (kill-new (format "[[%s]]" (phi-get-current-note-id)))
    (kill-new (format "%s://%s" phi-url-protocol (phi-get-current-note-id)))))

(defun phi-remove-frontmatter (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (when (looking-at-p "---") ;; remove YAML frontmatter
      (forward-line 1)
      (let  ((start-pos (point-min))
             (yaml-end-pos (search-forward-regexp "^\\(\\.\\.\\.\\|---\\)" nil t)))
        (delete-region start-pos yaml-end-pos)))
    (goto-char (point-min))
    (when (search-forward-regexp (concat "^[" phi-originating-symbol
                                         phi-parent-symbol "]") nil) ;; remove breadcrumb
      (move-end-of-line nil)
      (delete-region (point-min) (point)))
    (buffer-string)))

(defun phi-get-wiki-linked-ids (file)
  "Return a sequence of unique ids referenced in `FILE'."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (cl-remove-duplicates
       (cl-loop while (re-search-forward (phi-wiki-link-re) nil t) collect
                (save-excursion
                  (re-search-backward (phi-wiki-link-re))
                  (phi-get-next-link-at-point)))
       :test #'string-equal))))

;; ideas from Grant Rosson's zk package

(defun phi--grep-tag-list ()
  "Return list of tags from all notes in phi directory."
  (let* ((tags (shell-command-to-string (concat
                                          "grep -I -ohir -e "
                                          (shell-quote-argument
                                           phi-tag-regex)
                                          " "
                                          (phi-notes-path 'enforce-path) " 2>/dev/null")))
         (tag-list (split-string tags "\n" t)))
    (delete-dups tag-list)))

;;;###autoload
(defun phi-tag-insert ()
  "Insert TAG at point.
Select TAG, with completion, from list of all tags in phi notes."
  (interactive)
  (insert (completing-read "Tag: " (phi--grep-tag-list))))

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


(defgroup phi-sidebar-faces ()
  "Default faces for `phi-mode' sidebar."
  :group 'phi-sidebar-faces)

(defface phi-sidebar-face
  '((t (:height 0.9 :inherit (region))))
  "Default face for the sidebar."
  :group 'phi-sidebar-faces)


(defvar phi-sidebar-buffer nil)

(defun phi-sidebar-adjust-buffer (buffer)
  (with-current-buffer buffer
    (phi-mode)
    (if (and phi-sidebar-olivetti-width (bound-and-true-p olivetti-mode))
         (olivetti-set-width phi-sidebar-olivetti-width))
    (setq buffer-face-mode-face 'phi-sidebar-face)
    (buffer-face-mode)
    (phi-buttonize-buffer))
  buffer)

(defun phi-sidebar-create-buffer (id)
  (let ((file (phi-matching-file-name id t)))
        (if file
            (setq phi-sidebar-buffer (phi-sidebar-adjust-buffer
                                      (find-file-noselect file)))
          (error "No matching file for note id %s" id))))

(defun phi-sidebar-create-window (id)
  (display-buffer-in-side-window (phi-sidebar-create-buffer id)
                                 (append phi-sidebar-display-alist
                                         (when phi-sidebar-persistent-window
                                           (list '(window-parameters (no-delete-other-windows . t)))))))

(defun phi--master-note-id ()
  "Get the master note for the current repository"
  (let ((custom-master-note
         (cdr (assoc (expand-file-name (directory-file-name default-directory))
                     (mapcar (lambda (x) (cons (expand-file-name (directory-file-name (cadr x))) (nth 2 x))) phi-repository-alist)))))
    (if custom-master-note
        custom-master-note
      phi-default-master-note-id)))

(defun phi-sidebar-with-parent ()
  "Open PHI Sidebar with linked project, parent or master note"
  (interactive)
  (phi-sidebar-create-window (or (phi-get-linked-project-note-id) (phi-get-ancestor-project-id (phi-get-current-note-id)) (phi-get-parent-note-id) (phi--master-note-id))))

;;;###autoload
(defun phi-toggle-sidebar ()
  "Toggle visibility of PHI Sidebar"
  (interactive)
  (if (and phi-sidebar-buffer (window-live-p (get-buffer-window phi-sidebar-buffer)))
      (progn (delete-window (get-buffer-window phi-sidebar-buffer))
           (setq phi-sidebar-buffer nil))
    (phi-sidebar-with-parent)))

;;;###autoload
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

;; bibtex-completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bibtex-completion-create-phi-note (keys)
  "Create a PHI bibliographical annotation note with the first entry in KEYS."
  (phi--enforce-directory)
  (let* ((key (nth 0 keys))
         (entry (bibtex-completion-get-entry key))
         (year (or (bibtex-completion-get-value "year" entry)
                   (car (split-string (bibtex-completion-get-value "date" entry "") "-"))))
         (author (bibtex-completion-get-value "author" entry))
         (title (bibtex-completion-get-value "title" entry))
         (note-title (read-string "title: " (format "%s (%s) %s" author year title)))
         (tags (read-string "tags: " (concat phi-tag-symbol phi-annotation-tag)))
         (loc (read-string "loc: " "0"))
         (id (phi-get-counter))
         (buffer (phi-create-common-note :id id :title note-title :tags tags :citekey key :loc loc)))
    (if (equal current-prefix-arg nil) ; no C-u
        (switch-to-buffer buffer)
      (pop-to-buffer buffer))))

;; phi-cached ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;i;;;;;;;;;;

(defun phi-cache-initialize ()
  "Initialize hash tables for caching files."
  (setq phi-hash-contents (make-hash-table :test 'equal))
  (setq phi-hash-mtimes (make-hash-table :test 'equal)))

(defun phi-cache-newer-file (file mtime)
  "Update cached information for FILE with given MTIME."
  ;; Modification time
  (let ((contents (phi--get-tags-from-file-as-str file)))
    (puthash file mtime phi-hash-mtimes)
    (puthash file contents phi-hash-contents)))

(defun phi-cache-get-mtime (file)
  (gethash file phi-hash-mtimes))

(defun phi-cache-file (file)
  "Update file cache if FILE exists."
  (let ((mtime-cache (phi-cache-get-mtime (expand-file-name file)))
        (mtime-file (nth 6 (file-attributes (file-truename file)))))
    (if (or (not mtime-cache)
            (time-less-p mtime-cache mtime-file))
        (phi-cache-newer-file (expand-file-name file) mtime-file))))

(defun phi-cache-get-contents (file)
  "Get cached contents for corresponding `FILE'"
  (or (gethash (expand-file-name file) phi-hash-contents)
      (phi-cache-file file)))

(defun phi-cache-refresh-dir-maybe (dir)
  (let ((mtime-cache (gethash (expand-file-name dir) phi-hash-mtimes))
        (mtime-file (nth 5 (file-attributes (expand-file-name dir)))))
    (if (or (not mtime-cache)
            (time-less-p mtime-cache mtime-file))
        (progn
          (mapcar #'phi-cache-file
                (directory-files (expand-file-name dir) t
                                 (helm-phi--extract-id-from-cadidate-re)))
          (puthash (expand-file-name dir) mtime-file phi-hash-mtimes)))))

;; helm-phi ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-phi--extract-id-from-cadidate-re ()
  (concat "^\\(" phi-id-regex "\\)\s+\\(.*\\)\\.\\(markdown\\|txt\\|org\\|taskpaper\\|md\\)$"))

(defun helm-phi--get-file-name (candidate)
  (let* ((file-line (helm-grep-split-line candidate))
         (filename (if file-line (cl-first file-line) candidate)))
    filename))

(defun helm-ag-phi-insert-link-action (candidate)
  (let ((filename (file-name-nondirectory (helm-phi--get-file-name candidate))))
    (string-match (helm-phi--extract-id-from-cadidate-re) filename)
    (let* ((id (match-string-no-properties 1 filename))
           (wikilink (concat phi-link-left-bracket-symbol
                             id phi-link-right-bracket-symbol)))
      (with-current-buffer (current-buffer)
        (insert wikilink)))))

(defun helm-phi-insert-title-and-link-action (candidate)
  (let ((filename (file-name-nondirectory (helm-phi--get-file-name candidate))))
  (string-match (helm-phi--extract-id-from-cadidate-re) filename)
    (let* ((id (match-string-no-properties 1 filename))
           (title (match-string-no-properties 2 filename))
           (wikilink (concat phi-link-left-bracket-symbol
                             id phi-link-right-bracket-symbol)))
      (with-current-buffer (current-buffer)
        (insert (concat title " " wikilink))))))

(defun helm-phi-insert-titles-and-links-action (candidate)
  "helm action to insert multiple titles and links"
  (cl-loop for cand in (helm-marked-candidates)
        do
        (with-current-buffer (current-buffer)
          (insert "- ") (helm-phi-insert-title-and-link-action cand) (newline))))

(defun helm-phi-insert-and-assign-action (candidate)
  (string-match (helm-phi--extract-id-from-cadidate-re) candidate)
  (let* ((id (match-string-no-properties 1 candidate))
         (this-id (phi-get-current-note-id))
         (buffer (find-file-noselect (phi-matching-file-name id))))
    (with-current-buffer buffer
        (let* ((current-projects (phi-get-note-field-contents phi-project-field))
               (project-link (if current-projects ;; preppend to existing project list, if needed
                               (concat (phi-id-to-wikilink this-id) " " current-projects) (phi-id-to-wikilink this-id))))
          (phi-set-note-field-contents phi-project-field project-link))))
  (or (phi-has-tag phi-project-tag) ;; add project tag to current note, if needed
      (phi-set-note-field-contents phi-tags-field (concat phi-tag-symbol phi-project-tag
                                                          " " (phi-get-note-field-contents phi-tags-field))))
  (helm-phi-insert-title-and-link-action candidate))

(defun helm-phi-find-note-action (candidate)
  (find-file candidate))

(defun helm-phi-source-data-sorted (&optional path)
  (mapcar #'car
          (sort (directory-files-and-attributes (expand-file-name (or path (phi-notes-path)))
                                                t (concat "^" phi-id-regex "\s+\\(.+\\)\\.\\(markdown\\|txt\\|org\\|taskpaper\\|md\\)$") t)
                #'(lambda (x y) (time-less-p (file-attribute-modification-time (cdr y)) (file-attribute-modification-time (cdr x)))))))

(defun helm-phi-source-data-unsorted (&optional path)
  (mapcar #'car
       (directory-files-and-attributes (expand-file-name (or path (phi-notes-path)))
                                       t
                                       (concat "^" phi-id-regex "\s+\\(.+\\)\\.\\(markdown\\|txt\\|org\\|taskpaper\\|md\\)$") t)))

(defun helm-phi-source-data-with-tags (&optional path)
  (phi-cache-refresh-dir-maybe (or path (phi-notes-path)))
;;  (when path (setq default-directory path)) - desnecessário se source data trouxer caminhos completos
  (mapcar #'(lambda (x) (cons (format "%s::%s" x (or ;; (phi--get-tags-from-note-as-str (phi--get-note-id-from-file-name x))
                                                  (phi-cache-get-contents x)
                                                  "")) (expand-file-name x)))
          (helm-phi-source-data-sorted path)))

(defun helm-phi--build-sources ()
  (append
   (cl-loop
    for repo in phi-repository-alist
    collect (helm-build-sync-source (car repo)
              :candidates ((lambda (x) (helm-phi-source-data-with-tags (second x))) repo)
              :candidate-transformer 'helm-phi-candidates-transformer
;;              :filtered-candidate-transformer 'helm-phi-filtered-candidate-transformer
              :action (helm-make-actions "Open note"
                                         'helm-phi-find-note-action
                                         "Insert link to note"
                                         'helm-ag-phi-insert-link-action
                                         "Insert title(s) & link(s)"
                                         'helm-phi-insert-titles-and-links-action
                                         "Insert & assign to this project"
                                         'helm-phi-insert-and-assign-action)))
   (list 
    (helm-build-dummy-source "Create a new note"
      :action (helm-make-actions "Create a new note"
                                 'phi-new-originating-note)))))

(defun helm-phi-formatter (candidate)
  (when (string-match (concat "\\(" phi-id-regex "\\)\s+\\(.+\\)\\.\\(markdown\\|txt\\|org\\|taskpaper\\|md\\)::\\(.*\\)$")
                      (car candidate))
    (let ((width (round (/ (with-helm-window (1- (window-body-width))) 1.61)))
          (display (car candidate)))
      (cons
       (concat
        (truncate-string-to-width 
         (format "%s %s"
                 (propertize (match-string 1 display) 'face 'font-lock-function-name-face)
                 (propertize (match-string 2 display) 'face 'font-lock-builtin-face)) width nil ?\s t
                 #'helm-moccur-buffer)
        " "
        (truncate-string-to-width
         (propertize (match-string 4 display) 'face 'font-lock-keyword-face) (- (window-body-width) 3 width) nil ?\s t
         #'font-lock-keyword-face))
       (cdr candidate)))))

(defun helm-phi-candidates-transformer (candidates)
  "Format CANDIDATES for display in helm."
  (cl-loop
   for entry in candidates
   collect (helm-phi-formatter entry)))

(defun helm-phi-filtered-candidate-transformer (candidates source)
  "Not working -- why?"
  (sort (copy-seq candidates) (lambda (x y)
                     (time-less-p (phi-cache-get-mtime (cdr x)) (phi-cache-get-mtime (cdr y))))))

(defun helm-do-phi-ag (input)
  (require 'helm-ag)
  (helm-ag--do-ag-set-source (phi-notes-path t))
  (helm-add-action-to-source "Insert wikilink"
                             #'helm-ag-phi-insert-link-action
                             helm-source-do-ag)
  (helm :sources 'helm-source-do-ag :buffer "*helm-ag-phi*" :keymap helm-do-ag-map
        :input input
        :history 'helm-ag--helm-history))

;;;###autoload
(defun helm-ag-phi-find-backlinks ()
  (interactive)
  (helm-do-phi-ag (concat
                   phi-link-left-bracket-symbol-re (phi-get-current-note-id) phi-link-right-bracket-symbol-re)))

;;;###autoload
(defun helm-ag-phi-find-like-tags ()
  (interactive)
  (helm-do-phi-ag (phi-get-note-field-contents phi-tags-field)))

;;;###autoload
(defun helm-ag-phi-find ()
  (interactive)
  (helm-do-phi-ag nil))

;;;###autoload
(defun helm-phi-find (&optional input)
  (require 'helm-source)
  (interactive)
  (helm :sources
         (helm-phi--build-sources)
        :buffer "*helm phi notes*"
        :input input))

;;;###autoload
(defun helm-phi-find-like-tags ()
  (interactive)
  (helm-phi-find (phi-get-note-field-contents phi-tags-field)))


;;;###autoload
(defun helm-phi-insert ()
  "Calls `helm-phi-find' considering the current note's context (DEPRECATED)"
  (interactive)
  (if phi-mode
      (setq default-directory (file-name-directory buffer-file-name)))
  (helm-phi-find))

;;;###autoload
(defun helm-phi-open-repo ()
  "Prompt for a repository and call `helm-phi-find`."
  (interactive)
  (setq default-directory (phi--prompt-for-notes-path))
  (helm-phi-find))

;;; markdown ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun phi-markdown-convert-wiki-link-to-filename-adv (orig-func link)
  "Advise function `markdown-convert-wiki-link-to-filename' to get a complete filename when `link' is an ID for an existing note."
  (or (and (stringp link)
           (save-match-data (string-match (concat "^" phi-id-regex) link))
           (phi-matching-file-name link))
      (funcall orig-func link)))

(advice-add 'markdown-convert-wiki-link-to-filename :around #'phi-markdown-convert-wiki-link-to-filename-adv)

;;; phi-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar phi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c ;") #'phi-toggle-sidebar)
    (define-key map (kbd "C-c :") #'phi-focus-sidebar)
    (define-key map (kbd "C-c n d") #'phi-new-descendant-note)
    (define-key map (kbd "C-c n k") #'phi-kill-to-new-note)
    (define-key map (kbd "C-c n y") #'phi-yank-to-new-note)
    (define-key map (kbd "C-c u") #'phi-visit-parent-note)
    (define-key map (kbd "C-c j") #'phi-visit-next-link)
    (define-key map (kbd "C-c f b") #'helm-ag-phi-find-backlinks)
    (define-key map (kbd "C-c f t") #'helm-phi-find-like-tags)
    (define-key map (kbd "C-c f f") #'helm-ag-phi-find)
    (define-key map (kbd "C-c R") #'phi-rename-current-note)
    (define-key map (kbd "C-c w") #'phi-smart-copy-region)
    (define-key map (kbd "C-c M-w") #'phi-smart-copy-ref-at-point)
    (define-key map (kbd "C-c l") #'phi-copy-wikilink)
    (define-key map (kbd "C-c t") #'phi-tag-insert)
    map)
  "Main mode map for `phi-mode'.")

(defun phi-mode-check-external-origin ()
  "Verify if origin field is set and make buffer read only in this case."
  (when (phi-get-note-field-contents phi-origin-field)
    (setq buffer-read-only t)))

(define-minor-mode phi-mode
  "PHI note minor mode"
  :lighter phi-mode-lighter
  :keymap phi-mode-map
  )

(add-hook 'phi-mode-hook 'phi-mode-check-external-origin)

(phi-cache-initialize)

(provide 'phi-notes)
