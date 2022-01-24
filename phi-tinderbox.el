;;; phi-tinderbox.el - phi-notes integration with Tinderbox
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

(defun phi-tinderbox-remove-frontmatter (str)
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

;;;###autoload
(defun phi-tinderbox-export-current-note (&optional container extra-attr)
  "Export the current note to Tinderbox"
  (interactive)
  (let ((attributes (list
                     (cons phi-tinderbox-zettel-attr (phi-get-current-note-id))
                     (cons phi-tinderbox-url-attr (concat phi-tinderbox-url-prefix (phi-get-current-note-id)))
                     (cons "ReadOnly" "true")
                     (cons "Prototype" phi-tinderbox-default-prototype)
                     (cons phi-tinderbox-citekey-attr (or (phi-get-note-field-contents phi-citekey-field) nil))))
        (name (phi-get-current-note-title))
        (text (phi-tinderbox-remove-frontmatter (substring-no-properties (buffer-string)))))
    ;; (when phi-tinderbox-default-prototype
    ;; (setq attributes (append attributes '(("Prototype" . phi-tinderbox-default-prototype)))))
    (tinderbox-initialize-attribute phi-tinderbox-zettel-attr "String")
    (tinderbox-initialize-attribute phi-tinderbox-citekey-attr "String")
    (tinderbox-make-or-update-note name text (or container phi-tinderbox-default-container)
                                   (append attributes extra-attr))))

(provide 'phi-tinderbox)
