;;; kohai.el --- A strange timetracker   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) since 2025  Xavier Van de Woestyne
;; Licensed under the MIT license.

;; Author: Xavier Van de Woestyne <xaviervdw@gmail.com>

;; This file is NOT part of GNU Emac

;; Maintainer: Xavier Van de Woestyne <xaviervdw@gmail.com>
;; Created: 24 January 2025
;; Keywords: tool timetracker productivity
;; URL: https://github.com/xvw/kohai
;; Package-Requires: ((emacs "29.1"))
;; Package-Version: 0.1
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A client for the Kohai server in order to manage timetracking from Emacs

;;; Code:

(require 'jsonrpc)
(require 'vtable)
(require 'cl-lib)
(require 'rens-mode)
(require 'kohai-req)

(defgroup kohai nil
  "Interaction from Emacs to a Kohai server."
  :link '(url-link "https://xvw.lol")
  :group 'tool
  :prefix "kohai-")

;;; Custom variables

(defcustom kohai-binary "/home/xvw/Projects/perso/kohai/kohai.exe"
  "The full path of the Kohai Binary."
  :group 'kohai
  :type 'string)

(defcustom kohai-supervised nil
  "The current path of the supervised directory."
  :group 'kohai
  :type 'directory)


(defcustom kohai-stderr-buffer-name "*kohai-stderr*"
  "The name of the buffer displaying the Kohai errors."
  :group 'kohai :type 'string)

(defcustom kohai-sectors-buffer-name "*kohai-sectors*"
  "The name of the buffer listing sectors."
  :group 'kohai :type 'string)


(defcustom kohai-transient-logs-buffer-name "*kohai-transient-logs*"
  "The name of the buffer listing transient logs."
  :group 'kohai :type 'string)

;;; Variables

(defvar kohai--connection nil
  "The Kohai JSONRPC instance.")


;;; Transient stuff

(transient-define-prefix kohai-dashboard ()
  "Global command access of Kohai."
  [["Dir"
    ("dg" "get" kohai-supervised-get :transient t)
    ("ds" "set" kohai-supervised-set :transient t)]

   ["Sectors"
    ("sl" "list" kohai-sector-list)
    ("sn" "new" kohai-sector-save :transient t)]

   ["Logs"
    ("r" "record" kohai-record-log)
    ("t" "transients" kohai-transient-logs)]

   ["Other"
    ("q" "close" transient-quit-one)]])

;;; Internal function

(defun kohai--ensure-connection ()
  "Ensure that the current session is connected to a Kohai server."
  (or kohai--connection (error "Not connected (use M-x kohai)")))

(defun kohai--ensure-supervision ()
  "Ensure that the current session is properly supervised."
  (and (kohai--ensure-connection)
       (or kohai-supervised (error "No supervised folder"))))

(cl-defun kohai--send (method params &key
                              timeout
                              cancel-on-input
                              cancel-on-input-retval)
  "Execute the request METHOD with given PARAMS.
TIMEOUT is a timeout time response.  CANCEL-ON-INPUT and
CANCEL-ON-INPUT-RETVAL are hooks for cancellation."
  (kohai--ensure-connection)
  (let ((server kohai--connection))
    (jsonrpc-request server method params
                     :timeout timeout
                     :cancel-on-input cancel-on-input
                     :cancel-on-input-retval cancel-on-input-retval)))

(defun kohai--fill-buffer (buffer-name &optional action)
  "Fill buffer BUFFER-NAME and perform ACTION (if non nil)."
  (let ((buff (get-buffer-create buffer-name)))
    (with-current-buffer buff
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char 1)
      (when action
        (funcall action buff))
      (setq buffer-read-only t))))

(defun kohai--with-buffer (buffer-name &optional action)
  "Switch buffer BUFFER-NAME and perform ACTION (if non nil)."
  (let ((buff (get-buffer-create buffer-name)))
    (kohai--fill-buffer buffer-name action)
    (switch-to-buffer-other-window buff)))

(defun kohai--read-dt-duration (prompt &optional default)
  "Read a duration or a datetime from the minibuffer with a PROMPT."
  (let* ((value (or default "now"))
         (raw-time (string-trim (read-from-minibuffer prompt value))))
    (if (or (string-blank-p raw-time)
            (string= (downcase raw-time) "now"))
        nil raw-time)))

(defun kohai--sector-save (sector desc)
  "Send a request that store SECTOR with a given DESC."
  (let* ((real-name (string-trim sector))
         (real-desc (string-trim desc))
         (desc (if (string-blank-p real-desc) nil real-desc))
         (param (list :name real-name
                      :description desc)))
    (let ((result (kohai--send :kohai/sector/save param)))
      (message "%s has been stored." real-name)
      (kohai--get-sectors result))))

(defun kohai--sector-update-description (object)
  "Update the selected sector (OBJECT) description."
  (let* ((sector (car object))
         (description (car (cdr object)))
         (new-desc (read-from-minibuffer
                    (format "New description of [%s]: " sector)
                    description)))
    (when (not (string-blank-p new-desc))
      (kohai--sector-save sector new-desc))))

(defun kohai--get-sectors (&optional given-sectors)
  "Return the list of sectors (or GIVEN-SECTORS) in a dedicated buffer."
  (kohai--ensure-supervision)
  (kohai--fill-buffer
   kohai-sectors-buffer-name
   (lambda (_buff)
     (let* ((sectors (or given-sectors (kohai--send :kohai/sector/list nil)))
            (objects (mapcar
                      (lambda (elt)
                        (list (propertize (cl-getf elt :name)
                                          'face '(bold default))
                              (propertize (or (cl-getf elt :description) "")
                                          'face 'default)))
                      sectors)))
       (if (<= (length sectors) 0)
           (insert "no sectors")
         (make-vtable
          :actions '("RET" kohai--sector-update-description)
          :divider-width "5px"
          :columns '("Sector name" "Sector description")
          :objects objects))))))


(defun kohai--get-transient-logs (&optional given-logs)
  "Return the list of transient logs (or GIVEN-LOGS) in a dedicated buffer."
  (kohai--ensure-supervision)
  (kohai--fill-buffer
   kohai-transient-logs-buffer-name
   (lambda (_buff)
     (let* ((logs (or given-logs (kohai--send :kohai/log/transient nil)))
            (objects (mapcar
                      (lambda (elt)
                        (list (cl-getf elt :index)
                              (cl-getf elt :start_date)
                              (or (cl-getf elt :duration) "?")
                              (or (cl-getf elt :project) "")
                              (cl-getf elt :sector)
                              (cl-getf elt :label)))
                      logs)))
       (if (<= (length logs) 0)
           (insert "no transient logs")
         (make-vtable
          :actions '("c" kohai--transient-log-close
                     "r" kohai--transient-log-rewrite)
          :divider-width "5px"
          :columns '("n" "st" "d" "p" "s" "l")
          :objects objects))))))


(defun kohai--record-transient-log (date project label)
  "Prompt the recording of a log with a prefilled DATE, PROJECT and LABEL."
  (kohai--ensure-supervision)
  (let* ((sectors (kohai--send :kohai/sector/list nil))
         (sector (kohai--complete-sectors sectors))
         (raw-project (string-trim (read-from-minibuffer "Project: " project)))
         (project (if (string-blank-p raw-project) nil raw-project))
         (at-time (kohai--read-dt-duration "When: " date))
         (label (string-trim (read-from-minibuffer "# " label))))
    (list :start_date at-time
          :project project
          :sector sector
          :label label)))

(defun kohai--transient-log-close (object)
  "Close the transient log defined by OBJECT"
  (kohai--ensure-supervision)
  (let* ((index (car object))
         (duration (kohai--read-dt-duration "Duration: "))
         (param (list :index index :duration duration))
         (logs (kohai--send :kohai/log/transient/stop param)))
    (kohai--get-transient-logs logs)))

(defun kohai--transient-log-rewrite (object)
  "Rewrite the transient log defined by OBJECT."
  (kohai--ensure-supervision)
  (let* ((index (car object))
         (date (car (cdr object)))
         (project (car (cdr (cdr (cdr object)))))
         (label (car (cdr (cdr (cdr (cdr (cdr object)))))))
         (param (kohai--record-transient-log date project label))
         (param-with-index (append param (list :index index)))
         (logs (kohai--send :kohai/log/transient/rewrite
                              param-with-index)))
    (kohai--get-transient-logs logs)))

(defun kohai--ask-for-closing-transient-logs (outdated)
  "Interactively ask for filling opening tasks (OUTDATED)."
  (dolist (log (append outdated nil))
    (let* ((index (cl-getf log :index))
           (date (cl-getf log :start_date))
           (label (cl-getf log :label))
           (v (format "%02d. %s %s\t"
                      index
                      date
                      (propertize label 'face 'bold))))
      (when (yes-or-no-p v)
        (let  ((logs (kohai--send :kohai/log/transient/stop
                                  (list :index index))))
          (kohai--get-transient-logs logs))))))

(defun kohai--complete-sectors (sectors)
  "Get SECTORS as a completion list."
  (let ((sector-entries
         (mapcar (lambda (sector)
                   (let* ((name (cl-getf sector :name))
                          (desc (cl-getf sector :description))
                          (key (concat (propertize name 'face 'bold)
                                       ": " desc)))
                     (cons key name)))
                 sectors)))
    (let ((selected-sector (completing-read "Sector: "
                                            sector-entries nil nil nil t)))
      (or (alist-get selected-sector
                     sector-entries
                     nil nil #'equal) selected-sector))))

;; Features

(defun kohai ()
  "Run a kohai process."
  (interactive)
  (when (not kohai--connection)
    (kohai-req--make-connection))
  (jsonrpc--message "Connected")
  (if kohai-supervised
      (progn (kohai--send :kohai/supervision/set kohai-supervised)
             (message "%s is supervised" kohai-supervised)
             (kohai-dashboard))
    (call-interactively #'kohai-supervised-set)))

(defun kohai-sector-save (name description)
  "Save a new sector with a NAME and a DESCRIPTION."
  (interactive "sName: \nsDescription of %s: ")
  (kohai--ensure-supervision)
  (kohai--sector-save name description))

(defun kohai-sector-list ()
  "Get the list of sectors in a dedicated buffer."
  (interactive)
  (kohai--ensure-supervision)
  (kohai--get-sectors)
  (switch-to-buffer-other-window
   (get-buffer-create kohai-sectors-buffer-name)))

(defun kohai-record-log ()
  "Start recording a log."
  (interactive)
  (kohai--ensure-supervision)
  (let* ((param (kohai--record-transient-log nil nil nil))
         (result (kohai--send :kohai/log/record param))
         (logs (cl-getf result :logs))
         (outd (cl-getf result :outdated)))
    (kohai--get-sectors)
    (kohai--get-transient-logs logs)
    (kohai--ask-for-closing-transient-logs outd)))

(defun kohai-transient-logs ()
  "Fetch the transient log list."
  (interactive)
  (kohai--ensure-supervision)
  (kohai--get-transient-logs)
  (switch-to-buffer-other-window
   (get-buffer-create kohai-transient-logs-buffer-name)))

(defun kohai-supervised-get ()
  "Display the current supervised directory."
  (interactive)
  (kohai--ensure-connection)
  (let ((result (kohai--send :kohai/supervision/get nil)))
    (if (not result)
        (message "No supervised directory")
      (message result))))

(defun kohai-supervised-set ()
  "Set interactively the current supervised directory."
  (interactive)
  (kohai--ensure-connection)
  (let* ((dir-table (apply-partially #'completion-table-with-predicate
                                     #'completion-file-name-table
                                     #'file-directory-p
                                     'strict))
         (folder-table (completion-table-in-turn dir-table))
         (result (completing-read "Directory: " folder-table))
         (absolute (expand-file-name result)))
    (setq kohai-supervised absolute)
    (customize-save-variable 'kohai-supervised absolute)
    (kohai--send :kohai/supervision/set absolute)
    (message "%s is now the supervised directory" absolute)))

(provide 'kohai)
;;; kohai.el ends here
