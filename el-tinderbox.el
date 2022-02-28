;;; el-tinderbox.el
;;;   by Bruno L. Conte (concat "bruno" "@" "brunoc.com.br"), 2022


(defun tinderbox-applescript--escape-quotes (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (save-excursion
      (while (search-forward "\\" nil t)
        (replace-match "\\\\\\\\" "FIXEDCASE" "LITERAL")))
    (save-excursion
      (while (search-forward "\"" nil t)
        (replace-match "\\\"" "FIXEDCASE" "LITERAL")))
    (message (buffer-string))
    (buffer-string)))

(defun tinderbox-applescript--quoted-string (str)
  (if str
      (format "\"%s\"" (tinderbox-applescript--escape-quotes str))
    "missing value"))

(defun tinderbox-applescript-set-attributes (attr-list)
  (mapconcat (lambda (x) (concat "set value of attribute \"" (car x) "\" to "
                                 (tinderbox-applescript--quoted-string (cdr x))))
             attr-list
             "\n"))

;;;###autoload
(defun tinderbox-make-or-update-note (name &optional text container attributes)
  "Make a new note.

`NAME' is the note name.

`TEXT' is the textual content of the note.

`ATTRIBUTES' is a list of cons cells with the format (attribute . value).

`CONTAINER' is a string with the name of the location (a note) where to create the new note.
If nil, the note will be created in the currently selected note."
  (let ((script-command (format "osascript <<EOF
set containerName to %s
set theName to %s
set theText to %s

tell application \"Tinderbox 8\"
	tell front document
		if not (exists) then error \"No Tinderbox document open.\"

		if containerName is not missing value and not (exists note containerName) then
			set newContainerNote to make new note at before first note
			tell newContainerNote to set name to containerName
		end if

		if containerName is not missing value then
			set theContainer to note containerName
		else
			set theContainer to the selected note
		end if

		tell theContainer
			if not (exists note theName) then
				set newNote to make new note at theContainer
			else
				set newNote to note theName
			end if
			tell newNote
				set its text to theText
				set value of attribute \"Name\" to theName
				%s
			end tell
		end tell
	end tell
end tell
EOF
"
                                (tinderbox-applescript--quoted-string container)
                                (tinderbox-applescript--quoted-string name)
                                (tinderbox-applescript--quoted-string text)
                                (tinderbox-applescript-set-attributes attributes))))
    (shell-command script-command "*Tinderbox output*" "*Tinderbox errors*")))

;;;###autoload
(defun tinderbox-initialize-attribute (attribute attr-type &optional default-value)
  "Create an attribute if needed"
  (let ((script-command (format "osascript <<EOF
set theAttributeName to %s
set theAttributeType to %s
set theDefaultValue to %s

tell application \"Tinderbox 8\"
	try
		set theDocument to front document
		if not (exists attribute theAttributeName in theDocument) then
			set theAttribute to make attribute in theDocument
			set type of theAttribute to theAttributeType
			set name of theAttribute to theAttributeName
			if theDefaultValue is not missing value then
				set defaultValue of theAttribute to theDefaultValue
			end if
		end if
	on error error_message number error_number
		if the error_number is not -128 then display alert message error_message as warning
		return
	end try
end tell
EOF
"
                                (tinderbox-applescript--quoted-string attribute)
                                (tinderbox-applescript--quoted-string attr-type)
                                (tinderbox-applescript--quoted-string default-value))))
    (shell-command script-command "*Tinderbox output*" "*Tinderbox errors*")))

(provide 'el-tinderbox)
