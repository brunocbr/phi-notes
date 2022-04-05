;;; phi-diogenes.el --- phi & Diogenes integration -*- lexical-binding:t -*-

;; Copyright (C) 2022  Bruno Conte

;; Author: Bruno Conte <bruno@brunoc.com.br>
;; URL: https://github.com/brunocbr/phi-notes/
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: zettelkasten, writing, tlg

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

(require 'phi-notes)

(defgroup phi-diogenes ()
  "Integration with Tinderbox"
  :group 'phi)

(defcustom phi-diogenes-server-command "/usr/local/diogenes/server/diogenes-server.pl"
  "Command to start Diogenes server"
  :type 'string
  :group 'phi-diogenes)

(defun phi-diogenes-start-server ()
  (interactive)
  (start-process-shell-command "phi-diogenes" nil phi-diogenes-server-command))

(provide 'phi-diogenes)
