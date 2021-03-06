;;; phi-notes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "phi-notes" "phi-notes.el" (25061 30235 688307
;;;;;;  924000))
;;; Generated autoloads from phi-notes.el

(autoload 'phi-initialize-counter "phi-notes" "\
Prompts for directory and value, and create a counter

\(fn &optional DIR VALUE)" t nil)

(autoload 'phi-add-repository "phi-notes" "\
Helper function for intializing a repository and customizing `phi-repository-alist'" t nil)

(autoload 'phi-buffer-repository "phi-notes" "\
Get the repository name for buffer `BUF', or the current buffer if `nil'.

\(fn &optional BUF)" nil nil)

(autoload 'phi-find-note "phi-notes" "\
Visit note `ID' in repository `REPO'

\(fn ID REPO)" nil nil)

(autoload 'phi-visit-parent-note "phi-notes" "\
Visit the parent note" t nil)

(autoload 'phi-visit-next-link "phi-notes" "\
Visit the next linked note. `C-u' to visit note in other window." t nil)

(autoload 'phi-new-originating-note "phi-notes" "\
Create an originating note. `C-u' to create note in other window." t nil)

(autoload 'phi-new-descendant-note "phi-notes" "\
Create a child linked note. `C-u' to create note in other window." t nil)

(autoload 'phi-kill-to-new-note "phi-notes" "\
Kill region to linked note. `C-u' to create note in other window.

\(fn START END)" t nil)

(autoload 'phi-yank-to-new-note "phi-notes" "\
Yank to linked note. `C-u' to create note in other window." t nil)

(autoload 'phi-toggle-sidebar "phi-notes" "\
Toggle visibility of PHI Sidebar" t nil)

(autoload 'phi-focus-sidebar "phi-notes" "\
Focus on PHI Sidebar" t nil)

(autoload 'helm-ag-phi-find-backlinks "phi-notes" nil t nil)

(autoload 'helm-ag-phi-find-like-tags "phi-notes" nil t nil)

(autoload 'helm-ag-phi-find "phi-notes" nil t nil)

(autoload 'helm-phi-find "phi-notes" "\


\(fn &optional INPUT)" t nil)

(autoload 'helm-phi-find-like-tags "phi-notes" nil t nil)

(autoload 'helm-phi-insert "phi-notes" "\
Calls `helm-phi-find' considering the current note's context" t nil)

(autoload 'helm-phi-open-repo "phi-notes" "\
Prompt for a repository and call `helm-phi-find`." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "phi-notes" '("bibtex-completion-create-phi-note" "helm-" "phi-")))

;;;***

(provide 'phi-notes-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; phi-notes-autoloads.el ends here
