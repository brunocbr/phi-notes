;; org-phi.el  -*- lexical-binding:t -*-

(require 'org)
(require 'bibtex-completion)
(require 's)

(defvar org-phi-bib-entry-template
  "${author}. (${year}) ${title}.
:PROPERTIES:
:CUSTOM_ID:	${=key=}
:TITLE:			${title}
:AUTHOR:		${author}
:YEAR:			${year}
:END:
")

(defun phi-bibtex-org-insert-bib-action (keys)
  (cl-loop for key in keys
           do
           (let* ((entry (bibtex-completion-get-entry key))
                  (heading (s-format org-phi-bib-entry-template
                                     (lambda (x) (cdr (assoc x entry))))))
             (org-insert-heading)
             (insert heading))))

(provide 'org-phi)
