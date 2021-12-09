; phi-notes.el - by Bruno Conte, bruno@brunoc.com.br
                                        ;
(defgroup phi nil
  "Tools for phi notes"
  :group 'tools)

(defcustom phi-notes-path nil
  "Path for note files"
  :type 'string
  :group 'phi)

(defcustom phi-counter-path nil
  "Path for counter file"
  :type 'string
  :group 'phi)

(defcustom phi-originating-symbol "○"
  "Symbol for originating notes breadcrumb"
  :type 'string
  :group 'phi)

(defcustom phi-parent-symbol "△"
  "Symbol for parent note in breadcrumb"
  :type 'string
  :group 'phi)

(defcustom phi-child-symbol "▽"
  "Symbol for child notes breadcrumb"
  :type 'string
  :group 'phi)

(defcustom phi-breadcrumb t
  "Create breadcrumbs"
  :type 'boolean
  :group 'phi)

(defun phi/get-counter ()
    "Return current counter")
