;;; phi-brain.el --- Emacs package for querying ChromaDB and displaying results with Helm -*- lexical-binding: t; -*-

;; Author: Bruno Conte
;; Version: 0.1
;; Package-Requires: ((emacs "24.3") (helm "3.6.2") (json "1.5"))
;; Keywords: tools, machine learning, helm, chromadb, zettelkasten
;; URL: https://github.com/brunoc_br/phi-notes/phi-brain.el

;;; Commentary:

;; `phi-brain` is an Emacs package that allows you to query a ChromaDB
;; database from Emacs by selecting text or using the entire buffer content.
;; Results are displayed in Helm, showing the document filename, a snippet
;; of the document, and the vector distance.

(require 'helm)
(require 'dash)
(require 'json)
(require 'url)
(require 'url-http)

(defgroup phi-brain nil
  "Emacs package for querying ChromaDB and displaying results with Helm."
  :group 'tools
  :prefix "phi-brain")

(defcustom phi-brain-python-script "phi_brain_query.py"
  "Path to the Python script for querying ChromaDB."
  :type 'string
  :group 'phi-brain)

(defcustom phi-brain-chromadb-host "localhost"
  "ChromaDB host address"
  :type 'string
  :group 'phi-brain)

(defcustom phi-brain-chromadb-port "8000"
  "ChromaDB host port"
  :type 'string
  :group 'phi-brain)

(defcustom phi-brain-embedding-model "text-embedding-3-small"
  "The vector embedding model."
  :type 'string
  :group 'phi-brain)

(defvar phi-brain-text-embedding-cache (make-hash-table :test 'equal)
  "Hash table to cache text embeddings using MD5 hashes as keys.")

(defvar phi-brain-collection-list-cache nil)

(defvar phi-brain-collection-completion-history nil)

(defun phi-brain-get-text-embedding (text)
  "Obtain an embedding vector for TEXT from OpenAI's API."
  (let* ((openai-api-key (getenv "OPENAI_API_KEY"))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " (encode-coding-string openai-api-key 'utf-8)))))
         ;; Encode the JSON payload as a unibyte UTF-8 string
         (url-request-data
          (encode-coding-string
           (json-encode `(("model" . ,phi-brain-embedding-model)
                          ("input" . ,text)))
           'utf-8))
         (api-url "https://api.openai.com/v1/embeddings"))
    (with-current-buffer (url-retrieve-synchronously api-url)
      (message "Ok")
      (goto-char (point-min))
      (re-search-forward "^$")
      (let* ((response (json-read))
             (embedding (cdr (assoc 'embedding (aref (cdr (assoc 'data response)) 0)))))
        embedding))))

(defun phi-brain-get-or-cache-text-embedding (text)
  "Retrieve the embedding for TEXT from cache if available.
Otherwise, fetch from OpenAI's API and cache the result using an MD5 hash as the key."
  (let ((text-hash (secure-hash 'md5 text)))
    (or (gethash text-hash phi-brain-text-embedding-cache)
        (let ((embedding (phi-brain-get-text-embedding text)))
          (puthash text-hash embedding phi-brain-text-embedding-cache)
          embedding))))

;; (phi-brain-get-or-cache-text-embedding "Plato")

(defun phi-brain-get-text ()
  "Get the selected region or the entire buffer as text."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun phi-brain-set-environment ()
  "Set enviroment variables for calling the Python script"
  (setenv "CHROMADB_HOST" phi-brain-chromadb-host)
  (setenv "CHROMADB_PORT" phi-brain-chromadb-port))

;;;###autoload
(defun phi-brain-query (collection-name query-text &optional n-results)
  "Query ChromaDB using COLLECTION-NAME with optional N-RESULTS.
Returns parsed JSON results as a list of alists."
  (let* ((n-results (or n-results 5))
         (output-buffer (generate-new-buffer "*phi-brain-output*"))
         (embedding (phi-brain-get-or-cache-text-embedding query-text))
         (embedding-json (json-encode embedding))
         (command (list phi-brain-python-script
                        collection-name
                        "--embedding"
                        (format "--n_results=%d" n-results))))

    (phi-brain-set-environment)

    (message (format "Querying %s" collection-name))
    ;; Call the Python process with query-text as input
    (with-temp-buffer
      (insert embedding-json)
      (apply 'call-process-region
             (point-min) (point-max) ;; region to send as stdin
             (car command)           ;; program to run
             nil                     ;; don't replace buffer
             output-buffer           ;; where to store output
             nil                     ;; no display
             (cdr command)))         ;; additional arguments
    ;; Parse the JSON output from the output buffer
    (with-current-buffer output-buffer
      (goto-char (point-min))
      (let ((json-object (json-parse-buffer :object-type 'alist)))
        (kill-buffer output-buffer)
        json-object))))

(defun phi-brain-get-collections ()
  "Run the ChromaDB script to obtain and return the list of collections as a list,
if the list is not already cached."
  (or phi-brain-collection-list-cache
      (let* ((output (progn
                       (phi-brain-set-environment)
                       (shell-command-to-string
                        (format "%s --list-collections" phi-brain-python-script))))
             (collections (json-read-from-string output)))
        (setq phi-brain-collection-list-cache collections)
        collections)))

(defun phi-brain--clean-string (str)
  "Remove newlines, carriage returns, and other stuff from STR."
  (->> str
       (replace-regexp-in-string "[\n\r\t]" " ")
       (replace-regexp-in-string "[△○▽]" "")
       (replace-regexp-in-string "\\[\\[[[:digit:]]+\\]\\]:" "")
       (replace-regexp-in-string "§ *\\[\\[[[:digit:]]+\\]\\]" "")
       (replace-regexp-in-string "\\[\\[[[:digit:]]+\\]\\]" "")
       (replace-regexp-in-string "#+" "")
       (replace-regexp-in-string "^[ \t]+" "")
       (replace-regexp-in-string " +" " ")))

;; (phi-brain--clean-string "### aa [[1234]] vv")

(defun phi-brain-format-result (result)
  "Format a RESULT for display in Helm.
Displays the filename (without extension), beginning of document, and vector distance."
  (let* ((document (alist-get 'document result))
         (snippet (phi-brain--clean-string
                   (if (< 120 (length document))
                       (substring document 0 119)
                     document)))
         (distance (->> result (alist-get 'distance) (format "%.2f")))
         (metadata (alist-get 'metadata result))
         (filename (->> metadata (alist-get 'file_name)))
         (tags (or (->> metadata (alist-get 'tags)) ""))
         (citekey (->> metadata (alist-get 'citekey)))
         (info-str (if citekey (format "@%s %s" citekey tags)
                     tags))
         (basename (file-name-base filename))

         (helm-window-body-width (with-helm-window (window-body-width)))
         (width-left (round (/ (1- helm-window-body-width) 1.61)))
         (width-tags (if (> (length info-str) 0)
                         (round (/ (1- width-left) 2.63))
                       0))
         (width-title (- width-left width-tags))
         (width-right (- helm-window-body-width 3 width-left))
         (width-text (- width-right 6)))
    (cons
     (concat
      (truncate-string-to-width
       (format "%s"
               (propertize basename 'face 'font-lock-builtin-face))
       width-title nil ?\s t #'helm-moccur-buffer) ;; TODO: id highlight
      " "
      (truncate-string-to-width
       (propertize info-str 'face 'font-lock-keyword-face)
       width-tags nil ?\s t #'helm-moccur-buffer)
      " "
      (truncate-string-to-width
       (propertize snippet 'face 'font-lock-string-face)
       width-text nil ?\s t #'helm-moccur-buffer)
      " "
      (propertize distance 'face 'font-lock-keyword-face))
     result)))

(defun phi-brain-jump-to-text-in-file (file-name text)
  "Open FILE-NAME and jump to the first occurrence of TEXT,
highlighting it momentarily."
  ;; Open the file
  (find-file file-name)
  ;; Move to the beginning of the buffer
  (goto-char (point-min))
  ;; Search for the text
  (if (search-forward text nil t)
      (progn
        ;; Move cursor to the beginning of the found text
        (goto-char (match-beginning 0))
        ;; Highlight the found text temporarily
        (let ((highlight (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put highlight 'face 'highlight)
          ;; Flash the highlight briefly
          (run-at-time "0.5 sec" nil 'delete-overlay highlight)
          ;; Center the line with the found text in the window
          (recenter 1)))
    (message "Text not found in file.")))

(defun phi-brain-kill-links-and-titles (selection)
  "Kill a list of wikilinks and titles for SELECTION to the kill ring."
  (let ((wikilinks (mapcar (lambda (result)
                             (let ((id (alist-get 'id (alist-get 'metadata result)))
                                   (title (alist-get 'title (alist-get 'metadata result)))
                                   (filename (alist-get 'file_name (alist-get 'metadata result))))
                               (format "- %s [[%s]]" (or title
                                                         (replace-regexp-in-string "^[0-9]+ +" "" (file-name-base filename)))
                                       id)))
                           selection)))
    ;; Kill the formatted WikiLinks as a single string
    (kill-new (string-join wikilinks "\n"))
    (message "Wikilinks and titles copied to kill ring!")))

(defun phi-brain-helm-source (results)
  "Create a Helm source from RESULTS."
  (helm-build-sync-source "ChromaDB Results"
    :candidates results
    :candidate-transformer (lambda (candidates) (mapcar #'phi-brain-format-result candidates))
    :action '(("Jump to text in file" .
               (lambda (result) (phi-brain-jump-to-text-in-file
                                 (alist-get 'file_path (alist-get 'metadata result))
                                 (alist-get 'document result))))
              ("Kill titles and wikilinks" .
               (lambda (_) (phi-brain-kill-links-and-titles (helm-marked-candidates)))))))

;;;###autoload
(defun phi-brain-helm-search (&optional text collection-name n-results)
  "Search the current buffer content or selected text in
 COLLECTION-NAME with optional N-RESULTS."
  (interactive)
  (let* ((query-text (or text (phi-brain-get-text)))
         (col (or collection-name
                  (completing-read "Select a collection : "
                                   (mapcar #'identity (phi-brain-get-collections))
                                   nil t nil 'phi-brain-collection-completion-history)))
         (results (mapcar #'identity (phi-brain-query col query-text (or n-results 100)))))
    (helm :sources (phi-brain-helm-source results)
          :buffer "*helm phi-brain results*")))

;;;###autoload
(defun phi-brain-helm-query (text)
  "Prompt for text for a query"
  (interactive "sQuery: ")
  (message text)
  (phi-brain-helm-search text))

(provide 'phi-brain)

;;; phi-brain.el ends here
