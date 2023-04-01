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
;;; define a global key somewhere, e. g.:
;; 	(spacemacs/set-leader-keys "Co" 'phi-new-originating-note)
;;
;; for helm-bibtex support:
;;  (helm-bibtex-helmify-action bibtex-completion-create-phi-note helm-bibtex-create-phi-note)
;;
;;  (helm-add-action-to-source
;;   "Create PHI bibliographical annotation" 'helm-bibtex-create-phi-note
;;   helm-source-bibtex 1)


;;; Code:

(require 'helm-source)
(require 'pulse)
(require 'crm)

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

(defcustom phi-link-format "[[%s]]"
  "Format for wiki link"
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

(defcustom phi-hashtag-symbol "#"
  "Symbol to prepend tags"
  :type 'string
  :group 'phi)

(defcustom phi-tag-regex "[[:alnum:]\\./-_]+"
  "RegEx to identify a valid tag"
  :type 'string
  :group 'phi)

(defcustom phi-author-name nil
  "Author name for notes (overriding system user information)"
  :type 'string
  :group 'phi)

(defcustom phi-date-format "%F"
  "Date format for notes"
  :type 'string
  :group 'phi)

(defcustom phi-persistent-cache t
  "True if cache should be persistent (written on disk)"
  :type 'boolean
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

(defun phi-md-hashtags-str (tags)
  "Generate a string of hashtags out of the TAGS list."
  (mapconcat #'(lambda (t) (format "#%s" t)) tags " "))

(defun phi-md-hashtags-to-list (hashtags)
"Return a list of (valid) tags from a HASHTAGS string. Valid tag
names conform to `phi-tag-regex'."
(let ((start 0)
      (tags '())
      (hashtag-re (concat "#\\(" phi-tag-regex "\\)")))
  (when (stringp hashtags)
        (while (string-match hashtag-re hashtags start)
          (add-to-list 'tags (match-string 1 hashtags) t)
          (setq start (match-end 0))))
  tags))

(defun phi-journal-header (id title tags _parent-props _extra-fields)
  (format "\
\tDate: %s
\tTags: %s

%s

"
          (format-time-string "%e %B %Y %H:%M")
          (phi-md-hashtags-str tags)
          (or title (format-time-string "%A"))))

(defun phi-journal-get-fields (buffer)
  (save-excursion
    (goto-char (point-min))
    (let* ((endpos (save-excursion
                     (forward-line 6)
                     (point))) ;; limit the seek to the first lines
           (fields nil)
           (field-key-str nil))
      (while (search-forward-regexp "^\t\\([[:alnum:]]+\\):\\s-*" endpos t)
        (setq field-key-str (match-string-no-properties 1))
        (add-to-list 'fields
                     (cons (intern (downcase field-key-str))
                           (when (looking-at "\\(.*\\)$")
                             (string-trim-right (match-string-no-properties 1))))))
      fields)))

(defun phi--yaml-section-wrap (s)
  "Wrap S as a YAML section. All but the last fields will have
double space appended to the end of the line. This is in order
for text editors that don't identify YALM sections (e. g.
1Writer) to display the lines correctly."
  (let* ((lines (split-string s "\n" t " +$"))
         (breaklines (mapconcat #'(lambda (x) (format "%s  " x)) (butlast lines) "\n"))
         (section (concat "---\n" breaklines "\n" (car (last lines)) "\n...\n")))
    section))

(defun phi-md-frontmatter (id title &optional tags extra-fields)
  "Return the YAML frontmatter for a Markdown note."
  (let ((basic (format "\
title: %S
id:	%s\n" title id))
         (extra (phi--yaml-fields extra-fields))
         (hashtags (format "tags: %s" (or (phi-md-hashtags-str tags) ""))))
    (phi--yaml-section-wrap (concat basic extra hashtags))))

(defun phi-construct-breadcrumb (&optional parent)
  "Construct the breadcrumb for a new note"
  (if (and parent (not (equal parent "")))
      (concat phi-parent-symbol phi-link-left-bracket-symbol
              parent phi-link-right-bracket-symbol)
    (concat phi-originating-symbol)))

(defun phi--yaml-fields (props)
  "Return YAML fields for a alist PROPS."
  (cl-loop for (k . v) in props concat (format "%s: %s\n" k v)))

(defun phi-md-header (id title &optional tags parent-props extra-fields)
  "Return the header for a Markdown note.
PARENT-PROPS is a plist with parent note properties as keywords.
EXTRA-FIELDS is an alist of fields to include in the header after
the common fields."
  (let* ((frontmatter (phi-md-frontmatter id title tags extra-fields))
         (parent-id (alist-get 'id parent-props))
         (breadcrumb (when phi-breadcrumb
                       (concat "\n"
                               (phi-construct-breadcrumb parent-id)
                               "\n"))))
    (concat frontmatter breadcrumb "\n")))

(defun phi-md-tag-reader (fields)
  "Read the tag field from FIELDS, expecting a string with
hashtags, and return a list of tags."
  (phi-md-hashtags-to-list (alist-get 'tags fields)))

(defun phi-md-get-fields (buffer)
  "Return an alist with keys and values for fields in the YAML
frontmatter of BUFFER."
  (save-excursion
    (goto-char (point-min))
    ;; Find the YAML frontmatter block boundaries, or otherwise throw an error.
    (if (looking-at-p "---")
        (let* ((_ (forward-line 1))
              (target-pos (point))
              (yaml-end-pos (search-forward-regexp "^\\(\\.\\.\\.\\|---\\)" nil t))
              (field-key-str nil)
              (fields nil))

          ;; Return after search-forward moved point.
          (goto-char target-pos)

          (if yaml-end-pos
            ;; If YAML block is found, collect the fields
              (progn
                (while (search-forward-regexp "^\\([[:alnum:]-_]+\\):\\s-*" yaml-end-pos t)
                  (setq field-key-str (match-string-no-properties 1))
                  (add-to-list 'fields
                               (cons (intern field-key-str)
                                     (when (looking-at "\\(.*\\)$")
                                       (string-trim-right (match-string-no-properties 1)))) t))
                fields)
            nil))
      nil)))

(defun phi-md-insert-link (buffer link)
  "Function to insert a wikilink for Markdown notes in BUFFER. LINK
is a plist with appropriate metadata: `:description', `:id',
`:repository'"
  (let* ((description (or (plist-get link :description) ""))
         (target-id (plist-get link :id))
         (repository (plist-get link :repository))
         (link-pre (or (plist-get link :prepend) ""))
         (link-post (or (plist-get link :append) ""))
         (format-str (if (string= description "")
                         "%s%s[[%s]]%s"
                       "%s%s [[%s]]%s")))
  (with-current-buffer buffer
    (insert (format format-str link-pre description target-id link-post)))))

(defun phi-org-insert-link (buffer link)
  "Function to insert a link in an Org BUFFER. LINK is a plist with
the appropriate metadata : `:description', `:id', `:repository'."
  (require 'ol)
  (let* ((description (plist-get link :description))
         (target-id (plist-get link :id))
         (repository (or (plist-get link :repository)
                         (phi-buffer-repository)))
         (link-pre (plist-get link :prepend))
         (link-post (plist-get link :append)))
    (with-current-buffer buffer
      (insert
       (concat link-pre
               (org-link-make-string (concat repository ":" target-id)
                                     description)
               link-post)))))

(defun phi-org-header (id title &optional tags parent-props extra-fields)
  "Header for notes using Org format."
  (let* ((parent-id (alist-get 'id parent-props))
         (filetags (when tags
                     (concat ":"
                             (string-join tags ":")
                             ":")))
         (extras (mapcar #'(lambda (x)
                             (format "#+%s: %s\n"
                                     (upcase (symbol-name (car x)))
                                     (cdr x)))
                         extra-fields)))
    (concat (format "\
#+TITLE: %s
#+ID: %s\n" title id)
            (when parent-id
              (format "#+UPLINK: %s\n" parent-id))
            (when filetags
              (format "#+FILETAGS: %s\n" filetags))
            (when extras
              (apply #'concat extras))
            "\n")))

(defun phi-org-get-fields (buffer)
  (save-excursion
    (goto-char (point-min))
    (let* ((endpos (save-excursion
                     (forward-line 12)
                     (point)))
          (fields nil)
          (field-key-str nil))
      (while (search-forward-regexp "#\\+\\([[:alnum:]]+\\):\\s-*" endpos t)
        (setq field-key-str (match-string-no-properties 1))
        (add-to-list 'fields
                     (cons (intern (downcase field-key-str))
                           (when (looking-at "\\(.*\\)$")
                             (string-trim-right (match-string-no-properties 1))))))
      fields)))

(defun phi-org-tag-reader (fields)
  (let ((orgtags (alist-get 'filetags fields)))
    (when orgtags (split-string orgtags ":" t))))


(defun phi-author-date-fill (fields)
  "Return author and date fields filled with the user name and the
current date. System user name may be overriden by
`phi-author-name'. Date format may be set with `phi-date-format',
defaulting to ISO 8601 date."
(let* ((author-name (or phi-author-name
                        (user-full-name)
                        (user-login-name)))
       (date-format (or phi-date-format
                        "%F"))
       (date (format-time-string date-format)))
  (list (cons 'author author-name)
        (cons 'date date))))

;; TODO: 1) implement merge with custom types; 2) change everywhere where type
;; props are read to get them from a merged alist.
(defun phi--type-prop (prop type)
  "Return the property PROP for note TYPE"
  (let ((type-props (alist-get type phi-note-types)))
    (alist-get prop type-props)))

(defun phi-basic-type-check-p (buffer type &rest args)
  "Return `t' if the BUFFER conforms to basic verification checks
for a note of type TYPE. The conditions tested are the buffer file
name having valid extension and the note having all required
tags and all type-specific required extra-fields.

The extension may be supplied with `:extension'. Notably, it
should be supplied when working with temporary buffers with no
corresponding files."
  (let* ((type-props (alist-get type phi-note-types))
         (type-exts (alist-get 'file-extensions type-props))
         (file-ext (or (plist-get (flatten-list args) :extension)
                       (file-name-extension (buffer-file-name buffer))))
         (req-tags (alist-get 'required-tags type-props))
         (read-tags-fn (alist-get 'tag-reader-function type-props))
         (get-fields-fn (alist-get 'get-fields-function
                                    type-props))
         (fields (when (functionp get-fields-fn)
                   (funcall get-fields-fn buffer)))
         (field-keys (mapcar #'car fields))
         (tags (when (functionp read-tags-fn)
                 (funcall read-tags-fn fields)))
         (type-fields (alist-get 'extra-fields type-props)))
    (and (member file-ext type-exts)
         (cl-every #'(lambda (x) (memq x field-keys)) type-fields)
         (cl-every #'(lambda (x) (member x tags)) req-tags))))

(defun phi-is-type-p (buffer type &rest args)
  "Return `t' if BUFFER complies with the appropriate verification
checks for type TYPE."
  (let ((check-fn (phi--type-prop 'type-check-function type)))
    (when (functionp check-fn) (apply check-fn buffer type args))))

(defun phi-guess-type (buffer &rest args)
  "Return a guess of the note type for BUFFER. The function will
check the note types alist in reverse order (assuming this should
map from more to less specific types)."
  (let ((type-list (mapcar #'car (reverse phi-note-types))))
    (cl-loop for type in type-list
             thereis (when (apply #'phi-is-type-p buffer type args)
                       type))))

(defun phi-note-props (buffer)
  "Return alist of basic note properties for BUFFER."
  (let* ((fn (buffer-file-name buffer))
         (id (phi--get-note-id-from-file-name fn))
         (title (phi--get-note-title-from-file-name fn)))
    (list (cons 'id id)
          (cons 'title title))))

(defvar phi-note-types
  '((default . ((description . "Default")
                (file-extensions . ("markdown" "md"))
                (extra-fields . nil)
                (required-tags . nil)
                (header-function . phi-md-header)
                (tag-reader-function . phi-md-tag-reader)
                (tag-writer-function . nil)
                (get-fields-function . phi-md-get-fields)
                (insert-link-function . phi-md-insert-link)
                (type-check-function . phi-basic-type-check-p)))
    (bib-annotation . ((description . "Bibliographical annotation")
                       (file-extensions . ("markdown" "md"))
                       (extra-fields . (citekey loc))
                       (required-tags . ("ƒ"))
                       (header-function . phi-md-header)
                       (tag-reader-function . phi-md-tag-reader)
                       (tag-writer-function . nil)
                       (get-fields-function . phi-md-get-fields)
                       (insert-link-function . phi-md-insert-link)
                       (type-check-function . phi-basic-type-check-p)))
    (author-date . ((description . "Author & Date")
                    (file-extensions . ("markdown" "md"))
                    (extra-fields . (author date))
                    (required-tags . ())
                    (header-function . phi-md-header)
                    (tag-reader-function . phi-md-tag-reader)
                    (tag-writer-function . nil)
                    (get-fields-function . phi-md-get-fields)
                    (insert-link-function . phi-md-insert-link)
                    (transform-fields-function . phi-author-date-fill)
                    (type-check-function . phi-basic-type-check-p)))
    (tlg-text . ((description . "TLG Text")
                 (file-extensions . ("markdown"))
                 (extra-fields . (ref_tlg section line))
                 (header-function . phi-md-header)
                 (required-tags . ("π"))
                 (tag-reader-function . phi-md-tag-reader)
                 (tag-writer-function . nil)
                 (get-fields-function . phi-md-get-fields)
                 (insert-link-function . phi-md-insert-link)
                 (type-check-function . phi-basic-type-check-p)))
    (org-default . ((description . "Org file")
                    (file-extensions . ("org"))
                    (extra-fields . nil)
                    (required-tags . nil)
                    (header-function . phi-org-header)
                    (tag-reader-function . phi-org-tag-reader)
                    (get-fields-function . phi-org-get-fields)
                    (insert-link-function . phi-org-insert-link)
                    (type-check-function . phi-basic-type-check-p)))
    (journal . ((description . "Journal entry")
                (file-extensions . ("markdown"))
                (extra-fields . nil)
                (required-tags . ("diário"))
                (header-function . phi-journal-header)
                (tag-reader-function . phi-md-tag-reader)
                (tag-writer-function . nil)
                (get-fields-function . phi-journal-get-fields)
                (insert-link-function . phi-md-insert-link)
                (type-check-function . phi-basic-type-check-p)))))

;; TODO: experimenting...
(defvar phi-new-repositories
  '(("phi" . ((directory . "~/phi")))))

(defun phi-read-tags (&optional input-tags)
  "Interface for user input of tags. INPUT-TAGS is a default list
of tags."
  (completing-read-multiple "tags: " (phi--grep-tag-list) nil nil
                            (mapconcat 'identity input-tags ",")))

(defun phi-create-note (type repo-dir &rest args)
  "Non-interactive function to create a new note of the type TYPE
and write it to REPO-DIR. The user will be prompted for id, title
and (optional) extra fields, and the function will return the
newly created buffer.

Optional keyword arguments `:title', `:tags', `:fields' may be
passed to supply default information; `:parent-props' to pass an
alist of properties/fields of the parent note. This will all be
consumed by a call to the associated `header-function' registered
in `phi-note-types', in order to build the header for the note.

Use the optional keyword `:body' with a string to fill the note
with some contents."
  (let* ((type-props (alist-get type phi-note-types))
         (file-extension (first (alist-get 'file-extensions type-props)))
         (extra-fields (alist-get 'extra-fields type-props))
         (header-fn (alist-get 'header-function type-props))
         (parent-props (plist-get args :parent-props))
         (title (read-string "title: " (plist-get args :title)))

         ;; fields not matching extra-fields will be filtered:
         (input-fields (plist-get args :fields))
         (transform-fields-fn (alist-get 'transform-fields-function type-props))
         (transformed-fields (append
                              (when (functionp transform-fields-fn)
                                       (funcall transform-fields-fn input-fields))
                              input-fields))
         (fields (cl-loop for k in extra-fields
                          collect
                          (cons k
                                (read-string (format "%s: " k)
                                             (alist-get k transformed-fields)))))
         (tags (sort
                (phi-read-tags
                 (seq-uniq (append (plist-get args :tags)
                                   (alist-get 'required-tags type-props))
                           #'string=))
                'string<))
         (body (plist-get args :body))
         (id (phi-inc-counter repo-dir))
         (filename (concat repo-dir "/" id
                           (when (not (string= title ""))
                             (concat " " title)) "." file-extension))
         (header (funcall header-fn id title tags parent-props
                          fields))
         (new-buf (generate-new-buffer "*New PHI Note*")))
    (with-current-buffer new-buf
      (insert header)
      (when body (insert body))
      (write-file filename)
      (phi-mode))
    new-buf))

(defun phi-prompt-for-type ()
  "Prompt user to select a note type from its description, and return the type."
  (let* ((selection (mapcar #'(lambda (x)
                              (cons (alist-get 'description x) x))
                            phi-note-types))
         (choice (completing-read "Select a note type: "
                                  selection)))
    (cadr (assoc-string choice selection))))

;;;###autoload
(defun phi-new-note (&rest args)
  "Create a new note.

The note type is to be supplied with the `:type' keyword. If it
is `nil', the user will be prompted for a note type.

By default the user will be prompted for a note repository,
unless `:repository' is used with a repository name. Use 'current
to create the new note in the same repository as the current
buffer, without asking (except if the the current buffer file is
not in a valid repository path).

Other keyword arguments will be passed to the more specific
functions: `:title', `:tags', `:fields', `:body', `:parent-props'."
  (interactive)
  (let* ((repository-arg (plist-get args :repository))
         (def-repository (phi-repository-for-path
                          (buffer-file-name)))
         (repository
          (cond ((eq repository-arg 'current)
                 (or def-repository
                     (phi--prompt-for-repository def-repository)))
                ((eq repository-arg nil)
                 (phi--prompt-for-repository def-repository))
                (t repository-arg)))
         (repo-dir (cadr (assoc repository phi-repository-alist)))
         (type (or (plist-get args :type)
                   (phi-prompt-for-type)))
         (buf (apply #'phi-create-note type repo-dir args)))
    (phi--pop-to-buffer-maybe buf)
    buf)) ;; FIXME C-u not working

;; (phi-new-new-note :repository "Diário" :tags '(teste) :type 'journal :body "that's my body")

(defun phi-insert-link (buf link)
  "Insert a link in buffer BUF, using the appropriate format for
the note type. LINK is a plist."
  (let* ((type (phi-guess-type buf))
         (insert-link-fn (phi--type-prop 'insert-link-function type)))
    (when (functionp insert-link-fn)
      (funcall insert-link-fn buf link))))

(defun phi-get-tags (buffer &rest args)
  "Interface for getting tags for a buffer BUF which may contain
any kind of note. Return a list. Use `:type' to override
expensive type checks and `:fields' to avoid reading fields from
the buffer."
  (let* ((type (or (plist-get args :type)
                   (phi-guess-type buffer args)))
         (read-tags-fn (phi--type-prop 'tag-reader-function type))
         (get-fields-fn (phi--type-prop 'get-fields-function type)))
    (when (functionp read-tags-fn)
      (funcall read-tags-fn (or (plist-get args :fields)
                              (when (functionp get-fields-fn)
                                (funcall get-fields-fn
                                         buffer)))))))

(defun phi-get-fields (buffer &rest args)
  "Interface for getting fields for a buffer BUFFER which may contain
any kind of note. Return an alist."
  (let* ((type (or (plist-get args :type)
                   (phi-guess-type buffer args)))
         (get-fields-fn (phi--type-prop 'get-fields-function type)))
    (when (functionp get-fields-fn)
      (funcall get-fields-fn buffer))))

;;;###autoload
(defun phi-create-descendant (&rest args)
  "Create a descendant note from the current buffer. Use `:with-buffer' override.

Use `:link-prepend' and `:link-append' to insert text around the
link to the new note.

Keyword arguments may override `:repository', `:type',
`:parent-props', `:tags' and `:fields' for the new note."
  (interactive)
  (let* ((buf (or (plist-get args :with-buffer)
                  (current-buffer)))
         (type (phi-guess-type buf))
         (get-fields-fn (phi--type-prop 'get-fields-function type))
         (read-tags-fn (phi--type-prop 'tag-reader-function type))
         (cur-fields (when (functionp get-fields-fn)
                       (funcall get-fields-fn buf)))
         (cur-tags (when (functionp read-tags-fn)
                     (funcall read-tags-fn cur-fields)))
         ;; (extra-fields (seq-filter #'(lambda (x) (not (memq (car x) '(title id tags)))) cur-fields))
         (cur-props (phi-note-props buf))
         (new-buf
          (apply #'phi-new-note
           (append args
                   (list :repository 'current
                         :type type
                         :parent-props cur-props
                         :tags cur-tags
                         :fields cur-fields))))
         (new-props (phi-note-props new-buf))
         (new-id (alist-get 'id new-props))
         (new-title (alist-get 'title new-props))
         (link-pre (plist-get args :link-prepend))
         (link-post (plist-get args :link-append))
         (link-repo (phi-buffer-repository buf)))
    (phi-insert-link buf (list :id new-id :description new-title
                               :prepend link-pre :append link-post
                               :repository link-repo))
    new-buf))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAVIGATION

(defvar phi-navigation-history '()
  "Holds a list of file names of viewed notes, last to first.")

(defvar phi-navigation-previous nil
  "Local variable keeping the name of the last note viewed when the
current note was first visited.")

(defun phi-navigation-history-keeper ()
  "Keep a navigation history of notes visited. The function is
intended to serve as a hook for `window-configuration-change-hook'."
  (when phi-mode
    (let* ((buf (current-buffer))
           (file (expand-file-name (buffer-file-name buf)))
           (prev (first phi-navigation-history)))
      (setq phi-navigation-history
            (append (list file) (delete file phi-navigation-history)))
      (unless phi-navigation-previous
        (set (make-local-variable 'phi-navigation-previous) prev)))))

(defun phi-navigate-previous ()
  (interactive)
  (when phi-navigation-previous
    (phi--pop-to-buffer-maybe (find-file-noselect phi-navigation-previous))))

(add-hook 'window-configuration-change-hook 'phi-navigation-history-keeper)

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

(defun phi--prompt-for-repository (&optional def)
  (unless phi-repository-alist (error "No repository set! Use `phi-add-repository'."))
  (completing-read "Select a note repository: "
                   phi-repository-alist nil nil nil nil def))

(defun phi--prompt-for-notes-path ()
  (if (equal (length phi-repository-alist) 1)
      (cadr (car phi-repository-alist))
    (cadr (assoc (phi--prompt-for-repository) phi-repository-alist))))

;; TODO: should refactor all of this
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

(defun phi--get-counter-path (&optional dir)
  "Get the full path for the counter file"
  (concat (or dir (phi-notes-path)) "/" phi-counter-file))

(defun phi-inc-counter (&optional repo-dir)
  "Increment and return current counter"
  (let* ((phi-counter-path (phi--get-counter-path repo-dir))
         (current-counter (phi--get-current-counter-from-file phi-counter-path)))
    (if (string= current-counter "TIMESTAMP")
        (format-time-string phi-id-timestamp-format (current-time))
      (let ((counter))
        (setq counter (format phi-id-format
                              (1+ (string-to-number (phi--get-current-counter-from-file phi-counter-path)))))
        (with-temp-file phi-counter-path
          (insert counter))
        counter))))

(defun phi-id-to-wikilink (id)
  "Return a wikilink for the given `id'"
  (concat phi-link-left-bracket-symbol id phi-link-right-bracket-symbol))

(defun phi--get-note-id-from-file-name (filename)
  (let ((fn (file-name-sans-extension (file-name-nondirectory filename))))
    (when (string-match (concat "^" phi-id-regex) fn)
      (match-string 0 fn))))

(defun phi--get-note-title-from-file-name (filename)
  (let ((fn (file-name-sans-extension (file-name-nondirectory filename))))
    (if (string-match (concat "^\\(" phi-id-regex "\\)\s+\\(.*\\)$") fn)
        (match-string-no-properties 2 fn))))

(defun phi-get-current-note-id ()
  "Get the current note id"
  (interactive)
  (phi--get-note-id-from-file-name buffer-file-name))

(defun phi-get-current-note-title ()
  "Get current note title from its filename"
  (phi--get-note-title-from-file-name buffer-file-name))

(defun phi-get-current-note-tlg-fields ()
  "Get the TLG fields for the current note in as plist"
  (let ((data  (list :tlg-ref (phi-get-note-field-contents phi-tlg-ref-field)
             :tlg-section (phi-get-note-field-contents phi-tlg-section-field)
             :tlg-line (phi-get-note-field-contents phi-tlg-line-field))))
    data))

;;;###autoload
(defun phi-buffer-repository (&optional buf)
  "Get the repository name for buffer `BUF', or the current buffer if `nil'."
  (let ((filename (buffer-file-name (or buf
                                        (current-buffer))))
        (repo-dirs (mapcar (lambda (x)
                             (cons
                              (directory-file-name (expand-file-name (cadr x)))
                              (first x))) phi-repository-alist)))
    (if filename
        (cdr (assoc (directory-file-name (file-name-directory (expand-file-name filename)))
                    repo-dirs)))))

(defun phi-matching-file-name (id &optional usecontext path)
  "Return the first match of a file name starting with ID.

If USECONTEXT is not nil, enforce setting the current directory to the note's directory."
  (nth 0 (file-name-all-completions id (or path (phi-notes-path usecontext))))) ;; blank for current dir instead of phi-notes-path

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
  (string-match-p (concat phi-hashtag-symbol phi-project-tag) (or (phi--get-tags-from-note-as-str id) "")))

(defun phi-get-linked-project-note-id ()
  "Return the id for the project this note is linked to"
  (let ((project (phi-get-note-field-contents phi-project-field)))
    (if (and project (string-match (concat phi-link-left-bracket-symbol-re "\\(" phi-id-regex "\\)" phi-link-right-bracket-symbol-re) project))
        (match-string-no-properties 1 project)
      (or project nil))))

(defun phi--get-repository-path (repository)
  (cadr (assoc repository phi-repository-alist)))

(defun phi-in-repository-p (path repository)
  "Returns t if PATH is in the corresponding directory for
REPOSITORY"
  (let ((target-dir (file-name-directory (expand-file-name path)))
        (repo-dir (file-name-as-directory (expand-file-name (phi--get-repository-path repository)))))
    (string= target-dir repo-dir)))

(defun phi-repository-for-path (path)
  "Return the repository name corresponding to PATH, or `nil' if
there's no match"
  (some #'(lambda (r) (when (phi-in-repository-p path r) r))
        (mapcar 'car phi-repository-alist)))

(defun phi-filename (id &optional repo)
  "Return the filename for note ID. If optional parameter REPO is not provided,
assume the repository for the current buffer."
  (let* ((path (phi--get-repository-path (or repo
                                             (phi-buffer-repository))))
         (filename (phi-matching-file-name id nil path)))
    (concat path "/" filename)))

;;;###autoload
(defun phi-find-note (id repo)
  "Visit note `ID' in repository `REPO'"
  (let ((filename (expand-file-name (phi-filename id repo))))
    (if filename
        (phi--pop-to-buffer-maybe (find-file-noselect filename))
      (error (format "Invalid note ID %s" id)))))

;;;###autoload
(defun phi-visit-parent-note ()
  "Visit the parent note"
  (interactive)
  (let ((id (phi-get-parent-note-id)))
    (if id
        (switch-to-buffer (find-file-noselect (phi-matching-file-name (phi-get-parent-note-id)
        t)))
      (message "The current note has no parent!"))))

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

(defun phi-get-note-field-contents (field &optional buffer)
  "Return the specified field contents for BUFFER. If BUFFER is
`nil', work with the current buffer."
  (let* ((buf (or buffer
                 (current-buffer)))
        (fields (phi-get-fields buf)))
    (alist-get (intern field) fields))) ;; TODO: interim solution

    ;; (with-current-buffer buf
    ;;   (save-excursion
    ;;     (goto-char (point-min))
    ;;     (if (looking-at-p "---") (forward-line 1))
    ;;     (let ((target-pos (point))
    ;;           (yaml-end-pos (search-forward-regexp "^\\(\\.\\.\\.\\|---\\)" nil t)))
    ;;       (goto-char target-pos)
    ;;       (if  (and (re-search-forward (concat "^" field ":\\s-*") yaml-end-pos t)
    ;;                 (not (looking-at "^\\(\\.\\.\\.\\|---\\)"))
    ;;                 (looking-at (concat ".*$")))
    ;;           (replace-regexp-in-string "\s+$" "" (match-string-no-properties 0))))))))

(defun phi--get-tags-from-note-as-str (id)
  "Get a string of the tags from a given note `ID'"
  (let ((file (concat (phi-notes-path) "/" (phi-matching-file-name id)))
        contents)
    (with-current-buffer (get-buffer-create "*PHI temp*")
      (insert-file-contents file nil nil nil t)
      (setq contents (phi-get-note-field-contents phi-tags-field)))))

(defun phi--get-metadata-from-file (file)
  (with-current-buffer (get-buffer-create "*PHI temp*")
    (insert-file-contents file nil nil nil t)
    ;; TODO interim solution 2
    (let* ((file-ext (file-name-extension file))
           (type (phi-guess-type (current-buffer)
                                 :extension file-ext))
           (fields (and type
                        (phi-get-fields (current-buffer)
                                        :type type)))
           (tags (and type
                      (phi-get-tags (current-buffer)
                                    :type type
                                    :fields fields)))
           (contents (list :tags (phi-md-hashtags-str (sort tags 'string<))
                          ;; (phi-get-note-field-contents phi-tags-field)
                          :citekey (alist-get 'citekey fields))))
                          ;; (phi-get-note-field-contents phi-citekey-field))))
      contents)))

(defun phi--get-metadata-from-file-1 (file)
  (with-current-buffer (get-buffer-create "*PHI temp*")
    (insert-file-contents file nil nil nil t)
    ;; TODO interim solution
    (let* ((fields (phi-md-get-fields (current-buffer)))
           (contents (list :tags (alist-get 'tags fields)
                           ;; (phi-get-note-field-contents phi-tags-field)
                           :citekey (alist-get 'citekey fields))))
      ;; (phi-get-note-field-contents phi-citekey-field))))
      contents)))


(defun phi-has-tag (tag &optional buffer &rest args)
  "Return non-nil if the note has the specified TAG.

The note type may be specified with the keyword argument `:type',
or otherwise it will be guessed."
  (let* ((buf (or buffer (current-buffer)))
         (type (or (plist-get args :type)
                   (phi-guess-type buf args)))
         (fields (phi-get-fields buf :type type))
         (read-tags-fn (phi--type-prop 'tag-reader-function type))
         (tags (funcall read-tags-fn fields)))
    (member tag tags)))

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

(make-obsolete 'phi-create-common-note 'phi-create-note "2022-11-27")

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
        (id (phi-inc-counter))
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

(make-obsolete 'phi-new-common-note 'phi-new-note "2022-11-27")

;;;###autoload
(defun phi-new-originating-note (&optional body)
  "Create an originating note. `C-u' to create note in other window."
  (interactive)
  (phi-new-note :body body))

(make-obsolete 'phi-new-originating-note 'phi-new-note "2022-11-27")

;;;###autoload
(define-obsolete-function-alias 'phi-new-descendant-note 'phi-create-descendant "2022-11-27")

;;;###autoload
(defun phi-kill-to-new-note (start end)
  "Kill region to linked note. `C-u' to create note in other window."
  (interactive "r")
  (let ((body (buffer-substring-no-properties start end))
        (buffer (current-buffer)))
    (phi-create-descendant :body body
                           :title (phi-extract-title-from-body body))
    (with-current-buffer buffer
      (kill-region start end))
    (pop-to-buffer buffer)))

;;;###autoload
(defun phi-yank-to-new-note ()
  "Yank to linked note. `C-u' to create note in other window."
  (interactive)
  (let ((body (substring-no-properties (car kill-ring))))
    (phi-create-descendant :body body
                           :title (phi-extract-title-from-body body))))

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
                                           "#[[:alnum:]\\./-_]\\+" ;; TODO
                                           )
                                          " "
                                          (file-name-directory
                                           (buffer-file-name (current-buffer)))
                                          " | sed 's/^#//' 2>/dev/null"
                                          )))
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
    (slot . -1)
    (mode-line-format . none))
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
  '((t (:height 0.9)))
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
  (phi-sidebar-create-window (or
                              (phi-get-linked-project-note-id)
                              (phi--master-note-id))))

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
  "Create PHI bibliographical annotation note(s) for each entry in KEYS."
  ;; (phi--enforce-directory)
  (let ((cur-buffer (current-buffer)))
    (cl-loop for key in keys
             do
             (let* ((entry (bibtex-completion-get-entry key))
                    (year (or (bibtex-completion-get-value "year" entry)
                              (car (split-string (bibtex-completion-get-value "date" entry "") "-"))))
                    (author (bibtex-completion-get-value "author" entry))
                    (title (bibtex-completion-get-value "title" entry))
                    (note-title (format "%s (%s) %s" author year title))
                    (fields (list (cons 'citekey key) (cons 'loc "0"))))
               (with-current-buffer cur-buffer
                 (phi-create-descendant :title note-title
                                        :fields fields
                                        :type 'bib-annotation
                                        :link-prepend "- "
                                        :link-append (newline)))))))

;; phi-cached ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun phi-cache--write-hash-to-file (h outfile)
  "Asynchronously write a hashtable to a file."
  ;; (require 'async)
  (async-start `(lambda ()
                  (with-temp-file ,outfile
                    (prin1 ,h (current-buffer)))
                  nil)
               ;; `(lambda (result)
               ;;    (message (format "PHI cache saved to %s" ,outfile)))
               'ignore))

(defun phi-cache--persistent-filename (data)
  (let ((homedir (getenv "HOME")))
    (concat homedir "/.phi-cache-" data)))

(defun phi-persist-cache ()
  (when phi-persistent-cache
    (phi-cache--write-hash-to-file phi-hash-contents (phi-cache--persistent-filename "contents"))
    (phi-cache--write-hash-to-file phi-hash-mtimes (phi-cache--persistent-filename "mtimes"))))

(defun phi-cache-cleanup ()
  "Asynchronously check for orphaned entries and reset the cache if any is found."
  ;; TODO: implement cache cleanup
  )

(defun phi-cache--slurp (file)
  (with-temp-buffer
    (insert-file file)
    (read (current-buffer))))

(defun phi-load-persistent-cache ()
    (setq phi-hash-mtimes (phi-cache--slurp (phi-cache--persistent-filename "mtimes")))
    (setq phi-hash-contents (phi-cache--slurp (phi-cache--persistent-filename "contents"))))

(defun phi-cache-reset ()
  "Reset the notes cache."
  (interactive)
  (setq phi-hash-contents (make-hash-table :test 'equal))
  (setq phi-hash-mtimes (make-hash-table :test 'equal)))

(defun phi-cache-initialize ()
  "Initialize hash tables for caching files. Read from persistent layer if enabled."
  (if (and phi-persistent-cache
           (file-exists-p (phi-cache--persistent-filename "mtimes"))
           (file-exists-p (phi-cache--persistent-filename "contents")))
      (phi-load-persistent-cache)
    (phi-cache-reset)))

(defun phi-cache-newer-file (file mtime)
  "Update cached information for FILE with given MTIME."
  ;; Modification time
  (let ((contents (phi--get-metadata-from-file file)))
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

(defun phi-cache--get-contents (file)
  "Get cached contents for corresponding `FILE'"
  (or (gethash (expand-file-name file) phi-hash-contents)
      (phi-cache-file file)))

(defun phi-cache--get-tags (file)
  "Get cached contents for tags of corresponding `FILE'"
  (let ((contents (phi-cache--get-contents file)))
    (plist-get contents :tags)))

(defun phi-cache-refresh-dir-maybe (dir)
  (let ((mtime-cache (gethash (expand-file-name dir) phi-hash-mtimes))
        (mtime-file (nth 5 (file-attributes (expand-file-name dir)))))
    (if (or (not mtime-cache)
            (time-less-p mtime-cache mtime-file))
        (progn
          (mapcar #'phi-cache-file
                (directory-files (expand-file-name dir) t
                                 (helm-phi--extract-id-from-cadidate-re)))
          (puthash (expand-file-name dir) mtime-file phi-hash-mtimes)
          (phi-persist-cache)))))

;; helm-phi ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-phi--extract-id-from-cadidate-re ()
  (concat "^\\(" phi-id-regex "\\)\s*\\(.*\\)\\.\\(markdown\\|txt\\|org\\|taskpaper\\|md\\)$"))

(defun helm-phi--get-file-name (candidate)
  (let* ((file-line (helm-grep-split-line candidate))
         (filename (if file-line (cl-first file-line) candidate)))
    filename))

(defun helm-ag-phi-insert-link-action (candidate)
  (cl-loop for cand in (helm-marked-candidates)
           do
           (let* ((filename (helm-phi--get-file-name cand))
                  (id (phi--get-note-id-from-file-name filename)))
             (phi-insert-link (current-buffer) (list :id id
                                                     :append " ")))))

(defun helm-phi-insert-title-and-link-action (candidate)
  (let ((filename (helm-phi--get-file-name candidate))
        (id (phi--get-note-id-from-file-name filename))
        (title (phi--get-note-title-from-file-name filename)))
    (phi-insert-link (current-buffer) (list :id id :description title))))

(defun helm-phi-insert-titles-and-links-action (candidate)
  "helm action to insert multiple titles and links"
  (cl-loop for cand in (helm-marked-candidates)
        do
        (let* ((filename (helm-phi--get-file-name cand))
               (id (phi--get-note-id-from-file-name filename))
               (title (phi--get-note-title-from-file-name filename)))
          (phi-insert-link (current-buffer) (list :id id
                                                  :description title
                                                  :prepend "- "
                                                  :append (newline))))))

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
      (phi-set-note-field-contents phi-tags-field (concat phi-hashtag-symbol phi-project-tag
                                                          " " (phi-get-note-field-contents phi-tags-field))))
  (helm-phi-insert-title-and-link-action candidate))

(defun phi--pop-to-buffer-maybe (buffer)
  "Pop to buffer if interaction is modified with C-u"
  (if (equal current-prefix-arg nil) ; no C-u
      (switch-to-buffer buffer)
    (pop-to-buffer buffer)))

(defun helm-phi-find-note-action (candidate)
  (find-file candidate)
  (phi-mode))

(defun helm-phi-source-data-sorted (&optional path)
  (mapcar #'car
          (sort (directory-files-and-attributes (expand-file-name (or path (phi-notes-path)))
                                                t (concat "^" phi-id-regex "\s*\\(.*\\)\\.\\(markdown\\|txt\\|org\\|taskpaper\\|md\\)$") t)
                #'(lambda (x y) (time-less-p (file-attribute-modification-time (cdr y)) (file-attribute-modification-time (cdr x)))))))

(defun helm-phi-source-data-unsorted (&optional path)
  (mapcar #'car
       (directory-files-and-attributes (expand-file-name (or path (phi-notes-path)))
                                       t
                                       (concat "^" phi-id-regex "\s+\\(.+\\)\\.\\(markdown\\|txt\\|org\\|taskpaper\\|md\\)$") t)))
(defun helm-phi-source-data-item (file)
  (let* ((contents (phi-cache--get-contents file))
         (tags (plist-get contents :tags))
         (citekey (plist-get contents :citekey)))
    (cons
     (format "%s::%s::%s"
             file
             (or tags "")
             (or (when citekey
                   (concat "@" citekey))
                 ""))
     (expand-file-name file))))

(defun helm-phi--source-data-items (item-list)
  (mapcar #'helm-phi-source-data-item
          item-list))

(defun helm-phi-source-data-with-tags (&optional path)
  (phi-cache-refresh-dir-maybe (or path (phi-notes-path)))
;;  (when path (setq default-directory path)) - desnecessário se source data trouxer caminhos completos
  (mapcar #'helm-phi-source-data-item (helm-phi-source-data-sorted path)))

(defun helm-phi-wiki-linked-action (file)
  (require 'helm-source)
  (interactive)
  (helm :sources (append
                  (list
                   (helm-build-sync-source (format "Wiki links into %s" (file-name-base file))
                     :candidates (mapcar #'helm-phi-source-data-item
                                         (append
                                          (list file) ;; include the note itself
                                          (mapcar #'(lambda (x) (concat (file-name-directory file) "/"
                                                                        (phi-matching-file-name x))) (phi-get-wiki-linked-ids file))))
                     :candidate-transformer 'helm-phi-candidates-transformer
                     :action (helm-phi--build-actions)))
                  (helm-phi--build-sources))
        :buffer (format "*helm phi wiki links in %s*" (file-name-base file))))


(defun helm-phi--build-actions ()
  (helm-make-actions "Open note"
                     'helm-phi-find-note-action
                     "Insert link to note"
                     'helm-ag-phi-insert-link-action
                     "Insert title(s) & link(s)"
                     'helm-phi-insert-titles-and-links-action
                     "Insert & assign to this project"
                     'helm-phi-insert-and-assign-action
                     "Navigate wiki linked notes"
                     'helm-phi-wiki-linked-action))

(defun helm-phi-new-note (candidate)
  (phi-new-note :title candidate))

(defun helm-phi--build-sources ()
  (require 'helm-find)
  (append
   (cl-loop
    for repo in phi-repository-alist
    collect (helm-build-sync-source (car repo)
              :candidates  ((lambda (x) (helm-phi-source-data-with-tags (second x))) repo)
              :candidate-transformer 'helm-phi-candidates-transformer
;;              :filtered-candidate-transformer 'helm-phi-filtered-candidate-transformer
;;              :keymap helm-find-files-map ;; FIXME
              :action (helm-phi--build-actions)))
   (list
    (helm-build-dummy-source "Create a new note"
      :action (helm-make-actions "Create a new note"
                                 'helm-phi-new-note)))))

(defun helm-phi-formatter (candidate)
  (when (string-match (concat "\\(" phi-id-regex "\\)\s*\\(.*\\)\\.\\(markdown\\|txt\\|org\\|taskpaper\\|md\\)::\\(.*\\)::\\(.*\\)$")
                      (car candidate))
    (let* ((helm-window-body-width (with-helm-window (window-body-width)))
           (width-left (round (/ (with-helm-window (1- helm-window-body-width)) 1.61)))
           (width-title (1- width-left))
           (display (car candidate))
           (citekey (match-string 5 display)))
      (cons
       (concat
        (truncate-string-to-width
         (format "%s %s"
                 (propertize (match-string 1 display) 'face 'font-lock-function-name-face)
                 (propertize (match-string 2 display) 'face 'font-lock-builtin-face))
         width-title nil ?\s t #'helm-moccur-buffer) ;; id and title
        " "
        (truncate-string-to-width
         (propertize (concat (when (not (string= citekey "")) (concat citekey " ")) (match-string 4 display)) 'face 'font-lock-keyword-face)
         (- helm-window-body-width 3 width-left) nil ?\s t #'font-lock-keyword-face)) ;; tags
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

(defun phi--grep-file-list (str)
  "Return a list of files containing regexp STR."
  (let* ((files (shell-command-to-string (concat
                                          "grep -lir --include \\*."
                                          "{markdown,txt,org,taskpaper,md}" ;; should be a symbol
                                          " -e "
                                          (shell-quote-argument
                                           str)
                                          " "
                                          (phi-notes-path t)
                                          " 2>/dev/null"))))
    (split-string files "\n" t)))

(defun phi--backlinks-list (id)
  "Return a list of files with wiki links to note ID"
  (phi--grep-file-list (regexp-quote (format phi-link-format id))))

;;;###autoload
(defun phi-backlinks ()
  "Display a list of notes with links pointing to the current one."
  (interactive)
  (let* ((id (phi-get-current-note-id))
         (source-name (format "Backlinks to %s" (file-name-base buffer-file-name)))
         (candidates (phi--backlinks-list id))
         (backlink-source (helm-build-sync-source source-name
                            :candidates (helm-phi--source-data-items candidates)
                            :candidate-transformer 'helm-phi-candidates-transformer
                            :action (helm-phi--build-actions))))
    (helm :sources backlink-source
          :buffer "*helm phi backlinks*")))

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
  (helm-phi-find (phi-md-hashtags-str (phi-get-tags (current-buffer)))))

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
    (define-key map (kbd "C-c n d") #'phi-create-descendant)
    (define-key map (kbd "C-c n k") #'phi-kill-to-new-note)
    (define-key map (kbd "C-c n y") #'phi-yank-to-new-note)
    (define-key map (kbd "C-c u") #'phi-navigate-previous)
    (define-key map (kbd "C-c j") #'phi-visit-next-link)
    (define-key map (kbd "C-c f b") #'phi-backlinks)
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
