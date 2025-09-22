;;; phi-tinderbox.el -- phi-notes integration with Tinderbox -*- lexical-binding:t -*-
;;; 	by Bruno L. Conte, 2022

(require 'phi-notes)
(require 'el-tinderbox)

(defgroup phi-tinderbox ()
  "Integration with Tinderbox"
  :group 'phi)

(defcustom phi-tinderbox-zettel-attr
  "ZettelID"
  "Attribute for note IDs"
  :type 'string
  :group 'phi-tinderbox)

(defcustom phi-tinderbox-citekey-attr
  "citekey"
  "Attribute for citekeys"
  :type 'string
  :group 'phi-tinderbox)

(defcustom phi-tinderbox-default-container
  "PHI Notes"
  "Default container for exported notes: note name or `nil' for the currently selected note."
  :type 'string
  :group 'phi-tinderbox)

(defcustom phi-tinderbox-default-prototype
  "PHI Note"
  "Default prototype name for notes."
  :type 'string
  :group 'phi-tinderbox)

(defcustom phi-tinderbox-url-attr
  "URL"
  "Attribute for note URLs"
  :type 'string
  :group 'phi-tinderbox)

(defcustom phi-tinderbox-url-prefix
  "x-phi://"
  "Prefix used to set the URL of the note for the field defined by `phi-tinderbox-url-prefix'."
  :type 'string
  :group 'phi-tinderbox)

(defun phi-tinderbox--initialize-tlg-attributes ()
  (tinderbox-initialize-attribute phi-tlg-ref-field "String")
  (tinderbox-initialize-attribute phi-tlg-section-field "String")
  (tinderbox-initialize-attribute phi-tlg-line-field "Number"))

(defun phi-tinderbox--get-current-note-tlg-attributes ()
  (let ((tlg-fields (phi-get-current-note-tlg-fields)))
    (list (cons phi-tlg-ref-field (getf tlg-fields :tlg-ref))
          (cons phi-tlg-section-field (getf tlg-fields :tlg-section))
          (cons phi-tlg-line-field (getf tlg-fields :tlg-line)))))

;;;###autoload
(defun phi-tinderbox-export-current-note (&optional container extra-attr)
  "Export the current note to Tinderbox"
  (interactive)
  (let ((attributes (list
                     (cons phi-tinderbox-zettel-attr (phi-get-current-note-id))
                     (cons phi-tinderbox-url-attr (concat phi-tinderbox-url-prefix (phi-get-current-note-id)))
                     (cons "ReadOnly" "true")
                     (cons "Prototype" phi-tinderbox-default-prototype)
                     (cons phi-tinderbox-citekey-attr (or (phi-get-note-field-contents phi-citekey-field) ""))))
        (tlg-attr (phi-tinderbox--get-current-note-tlg-attributes))
        (name (phi-get-current-note-title))
        (text (phi-remove-frontmatter (substring-no-properties (buffer-string)))))
    (tinderbox-initialize-attribute phi-tinderbox-zettel-attr "String")
    (tinderbox-initialize-attribute phi-tinderbox-citekey-attr "String")
    (phi-tinderbox--initialize-tlg-attributes)
    (tinderbox-make-or-update-note name text (or container phi-tinderbox-default-container)
                                   (append attributes tlg-attr extra-attr))))

(provide 'phi-tinderbox)
