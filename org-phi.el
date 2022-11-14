;; org-phi.el  -*- lexical-binding:t -*-

(require 'org)
(require 'bibtex-completion)
(require 'ol-bibtex)
(require 's)

(defvar org-phi-bib-entry-template
  "${author}. (${year}) ${title}.
:PROPERTIES:
:END:")

(defun phi-bibtex-org-insert-bib-action (keys)
  (cl-loop for key in keys
           do
           (let* ((entry (bibtex-completion-get-entry key))
	                (val (lambda (field) (cdr (assoc field entry))))
                  (heading (s-format org-phi-bib-entry-template
                                     val))
                  (org-special-properties nil) ; avoids errors with `org-entry-put'
	                (togtag (lambda (tag) (org-toggle-tag tag 'on))))
             (org-insert-heading)
             (insert heading)
             (org-bibtex-put org-bibtex-type-property-name
		                         (downcase (funcall val "=type=")))
             (dolist (pair entry)
               (pcase (car pair)
	               ("=key=" 		(org-bibtex-put
	                             org-bibtex-key-property (cdr pair)))
	               ("=type=" 		nil)
                 ("title" 		nil)
	               ("keywords" 	(if org-bibtex-tags-are-keywords
		                              (dolist (kw (split-string (cdr pair) ", *"))
			                              (funcall
			                               togtag
			                               (replace-regexp-in-string
			                                "[^[:alnum:]_@#%]" ""
			                                (replace-regexp-in-string "[ \t]+" "_" kw))))
		                            (org-bibtex-put (car pair) (cdr pair))))
	               (_ (org-bibtex-put (car pair) (cdr pair)))))
             (mapc togtag org-bibtex-tags))))

(provide 'org-phi)
