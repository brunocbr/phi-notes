;; ol-phi.el  -*- lexical-binding:t -*-

(require 'ol)
(require 'phi-notes)

(defun org-link-phi-open-function (name)
  "Return the open function for an Org link type corresponding to
repository NAME."
  (function
   (lambda (id _)
     (phi-find-note id name))))

(defun org-link-phi-store-function (name)
  "Create the link store function for an Org link type
corresponding to repository NAME."
  (function
   (lambda ()
     (when (and (phi-mode)
                (phi-in-repository-p (buffer-file-name (current-buffer))
                                     name))
       (let* ((id (phi-get-current-note-id))
              (link (format "%s:%s" name id))
              (title (phi-get-current-note-title)))
         (message (format "store fn for repository %s" name))
         (org-link-store-props
          :type name
          :link link
          :description title))))))

(defun org-link-phi-export (link description _format _channel)
  "Export wiki link for LINK"
  (lambda
    (concat (when description (concat description " ")) "[[" link "]]")))

(defun org-link-phi-register-link-types ()
  "Add Org link types for each note repository."
  (interactive)
  (let ((repositories (mapcar 'car phi-repository-alist)))
    (dolist (r repositories)
      (org-link-set-parameters r
                               :follow (org-link-phi-open-function r)
                               :export #'org-link-phi-export
                               :store (org-link-phi-store-function r)))))

(org-link-phi-register-link-types)
(provide 'ol-phi)