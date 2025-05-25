;;; kohai-transient-log.el --- Deal with Transient logs  -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) since 2025  Xavier Van de Woestyne
;; Licensed under the MIT license.

;; Author: Xavier Van de Woestyne <xaviervdw@gmail.com>

;; This file is NOT part of GNU Emac

;; Maintainer: Xavier Van de Woestyne <xaviervdw@gmail.com>
;; Created: 13 February 2025
;; Keywords: tool timetracker productivity
;; URL: https://github.com/xvw/kohai
;; Package-Requires: ((emacs "29.1"))
;; Package-Version: 0.1
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Interaction with transient logs

;;; Code:

(require 'cl-lib)
(require 'vtable)
(require 'kohai-core)
(require 'kohai-req)
(require 'kohai-buffer)
(require 'kohai-generic)
(require 'kohai-sector)
(require 'kohai-project)


(defun kohai-transient-log--prompt-record (&optional date sector project label)
  "Create params for recording a transient log.
DATE, SECTOR, PROJECT and LABEL can be pre-filled (for edition)."
  (kohai--ensure-supervision)
  (let* ((sector (kohai-sector--ac nil nil sector))
         (project (kohai--nil-if-blank (kohai-project--ac nil nil project)))
         (at-time (kohai--read-datetime "When: " date))
         (label (string-trim (read-string "# " label))))
    (kohai--not-blank label "label")
    (list :date_query at-time
          :sector sector
          :project project
          :label label)))

(defun kohai-transient-log--list-vtable ()
  "Create the vtable displaying transient logs."
  (lambda (entries)
    (make-vtable
     :divider-width kohai--vtable-default-divider
     :objects (append entries nil)
     :columns '("Index"
                "Sector"
                "Project"
                "Start date"
                "Duration"
                "Label")
     :getter (lambda (o column vtable)
               (pcase (vtable-column vtable column)
                 ("Index" (cl-getf o :index))
                 ("Sector" (kohai--bold (cl-getf o :sector)))
                 ("Project" (or (cl-getf o :project) ""))
                 ("Start date" (cl-getf o :start_date_repr))
                 ("Duration" (or (cl-getf o :duration_repr) "?"))
                 ("Label" (cl-getf o :label))))
     :actions '("c" (lambda (o) (kohai-transient-log--stop-recording
                                 (cl-getf o :index)))
                "r" (lambda (o) (kohai-transient-log--rewrite
                                 (cl-getf o :index)))
                "d" (lambda (o) (kohai-transient-log--delete
                                 (cl-getf o :index)))
                "+" (lambda (o) (kohai-transient-log--duplicate
                                 (cl-getf o :index)))
                "m" (lambda (o) (kohai-transient-log--handle-meta
                                 (cl-getf o :index)))
                "l" (lambda (o) (kohai-transient-log--handle-link
                                 (cl-getf o :index)))
                "p" (lambda (o) (kohai-transient-log--promote
                                 (cl-getf o :index)))
                "n" (lambda (o) (kohai-transient-log--record
                                 (cl-getf o :start_date)))
                "q" (lambda (_o)
                      (kohai-buffer--kill kohai-transient-log-buffer-name))))))

(defun kohai-transient-log--list (&optional given-entries)
  "Return the list of entries (or GIVEN-ENTRIES) in log buffer."
  (let ((entries (or given-entries (kohai-req--transient-log-list))))
    (kohai-generic--vtable
     "transient-log"
     kohai-transient-logs-buffer-name
     entries
     (kohai-transient-log--list-vtable))))

(defun kohai-transient-log--minimize (log)
  "A very small string representation of a LOG."
  (let ((index (cl-getf log :index))
         (sdate (cl-getf log :start_date))
         (sector (cl-getf log :sector))
         (project (cl-getf log :project))
         (label (cl-getf log :label)))
    (format "%02d. %s: %s (%s, %s)" index sdate label sector
            (or project "N/A"))))

(defun kohai-transient-log--ask-for-closing-outdated (outdated)
  "Perform interactively closing of OUTDATED entries."
  (dolist (log (append outdated nil))
    (let* ((record (cl-getf log :record))
           (computed_duration (cl-getf log :computed_duration))
           (index (cl-getf record :index))
           (v (format "Close %s" (kohai-transient-log--minimize record))))
      (when (y-or-n-p v)
        (let* ((duration (kohai--read-datetime
                          "Duration: " (number-to-string computed_duration)))
               (logs (kohai-req--transient-log-stop-recording
                      (list :index index
                            :duration duration))))
          (kohai-transient-log--list (cl-getf logs :all)))))))

(defun kohai-transient-log--record (&optional start-date)
  "Record a transient log."
  (let* ((param (kohai-transient-log--prompt-record start-date))
         (result (kohai-req--transient-log-record param))
         (all (cl-getf result :all))
         (outdated (cl-getf result :outdated)))
    (kohai-sector--list)
    (kohai-project--list)
    (kohai-transient-log--list all)
    (kohai-transient-log--ask-for-closing-outdated outdated)))

(defun kohai-transient-log--stop-recording (index)
  "Stop the recording for a log referenced by INDEX."
  (let* ((duration (kohai--read-datetime "Duration: "))
         (param (list :index index :duration duration))
         (logs (kohai-req--transient-log-stop-recording param)))
    (kohai-transient-log--list (cl-getf logs :all))))

(defun kohai-transient-log--rewrite (index)
  "Rewrite a transient log referenced by INDEX."
  (let ((entry (kohai-req--transient-log-get index)))
    (kohai--should-exists entry "transient log")
    (let* ((param
            (kohai-transient-log--prompt-record
             (cl-getf entry :start_date)
             (cl-getf entry :sector)
             (cl-getf entry :project)
             (cl-getf entry :label)))
           (param-with-index (append param (list :index index)))
           (logs (kohai-req--transient-log-rewrite param-with-index)))
      (kohai-transient-log--list (cl-getf logs :all)))))

(defun kohai-transient-log--delete (index)
  "Delete a log referenced by INDEX."
  (let ((entry (kohai-req--transient-log-get index)))
    (kohai--should-exists entry "transient log")
    (when (y-or-n-p (format "Delete %s? " entry))
      (let ((logs (kohai-req--transient-log-delete index)))
        (kohai-transient-log--list (cl-getf logs :all))))))

(defun kohai-transient-log--duplicate (index)
  "Duplicate the log referenced by INDEX."
  (let ((entry (kohai-req--transient-log-get index)))
    (kohai--should-exists entry "transient log")
    (let ((logs (kohai-req--transient-log-duplicate index)))
      (kohai-transient-log--list (cl-getf logs :all)))))

(defun kohai-transient-log--meta-add (entry)
  "Prompt metadata adding for ENTRY."
  (let* ((key (string-trim (read-string "Key: ")))
        (value (string-trim (read-string "Value: ")))
        (index (cl-getf entry :index))
        (_ (kohai-req--transient-log-add-meta index key value))
        (entry (kohai-req--transient-log-get index)))
    (kohai-transient-log--list-meta entry)))

(defun kohai-transient-log--link-add (entry)
  "Prompt link adding for ENTRY."
  (let* ((key (string-trim (read-string "Name: ")))
        (value (string-trim (read-string "Url: ")))
        (index (cl-getf entry :index))
        (_ (kohai-req--transient-log-add-link index key value))
        (entry (kohai-req--transient-log-get index)))
    (kohai-transient-log--list-link entry)))

(defun kohai-transient-log--meta-remove (entry meta)
  "Remove the selected META for the given ENTRY."
  (let ((index (cl-getf entry :index)))
    (when (y-or-n-p (format "Delete the meta [%s] for %d ?" meta index))
      (let ((_ (kohai-req--transient-log-remove-meta index meta))
            (entry (kohai-req--transient-log-get index)))
        (kohai-transient-log--list-meta entry)))))

(defun kohai-transient-log--link-remove (entry link)
  "Remove the selected LINK for the given ENTRY."
  (let ((index (cl-getf entry :index)))
    (when (y-or-n-p (format "Delete the link [%s] for %d ?" link index))
      (let ((_ (kohai-req--transient-log-remove-link index link))
            (entry (kohai-req--transient-log-get index)))
        (kohai-transient-log--list-link entry)))))

(defun kohai-transient-log--meta-or-link-entry (meta)
  "Render a META for a vtable."
  (list (kohai--bold (cl-getf meta :key))
        (cl-getf meta :value)))

(defun kohai-transient-log--meta-or-link-vtable
    (k v obj entry adding removing)
  "Create the vtable for displaying meta of an ENTRY with OBJ.
K and V are used to describe the heading.
ADDING and REMOVING are used to control which structure
shoudl be treated."
  (make-vtable
   :columns `(,k ,v)
   :divider-width kohai--vtable-default-divider
   :objects (mapcar #'kohai-transient-log--meta-or-link-entry obj)
   :actions `("n" ,(lambda (_o) (funcall adding entry))
              "d" ,(lambda (o) (funcall removing entry (car o)))
              "q" ,(lambda (_o) (kohai-buffer--kill
                                 kohai-transient-log-buffer-name)))))

(defun kohai-transient-log--meta-vtable (meta entry)
  "Create the VTABLE for META (related to an ENTRY)."
  (kohai-transient-log--meta-or-link-vtable
   "Key" "Value" meta entry
   #'kohai-transient-log--meta-add
   #'kohai-transient-log--meta-remove))

(defun kohai-transient-log--link-vtable (link entry)
  "Create the VTABLE for LINK (related to an ENTRY)."
  (kohai-transient-log--meta-or-link-vtable
   "Name" "Url" link entry
   #'kohai-transient-log--link-add
   #'kohai-transient-log--link-remove))

(defun kohai-transient-log--list-meta-or-link
    (name k entry fvtable adding)
  "List K (with a NAME) related to ENTRY."
  (kohai-buffer--truncate-with
   kohai-transient-log-buffer-name
   (lambda (_buff)
     (let ((obj (append (cl-getf entry k) nil)))
       (if obj
           (progn
             (funcall fvtable obj entry)
             (display-buffer
              kohai-transient-log-buffer-name
              '(display-buffer-in-side-window .((side . bottom))))
             (pop-to-buffer kohai-transient-log-buffer-name))
         (when (y-or-n-p (format "No %s, adding it? " name))
           (funcall adding entry)))))))

(defun kohai-transient-log--list-meta (entry)
  "List META related to ENTRY."
  (kohai-transient-log--list-meta-or-link "METADATA"
                                          :meta
                                          entry
                                          #'kohai-transient-log--meta-vtable
                                          #'kohai-transient-log--meta-add))

(defun kohai-transient-log--list-link (entry)
  "List LINK related to ENTRY."
  (kohai-transient-log--list-meta-or-link "LINK"
                                          :links
                                          entry
                                          #'kohai-transient-log--link-vtable
                                          #'kohai-transient-log--link-add))


(defun kohai-transient-log--handle-meta-or-link (index listing)
  "LISTING metadata or link of transient log referenced by INDEX."
  (let ((entry (kohai-req--transient-log-get index)))
    (kohai--should-exists entry "transient log")
    (funcall listing entry)))

(defun kohai-transient-log--handle-meta (index)
  "Handle metadata of transient log referenced by INDEX."
  (kohai-transient-log--handle-meta-or-link index
                                            #'kohai-transient-log--list-meta))

  (defun kohai-transient-log--handle-link (index)
  "Handle links of transient log referenced by INDEX."
  (kohai-transient-log--handle-meta-or-link index
                                            #'kohai-transient-log--list-link))


(defun kohai-transient-log--promote (index)
  "Promote a transient log (via INDEX) into a real one."
  (let ((tlog (kohai-req--transient-log-get index)))
    (kohai--should-exists tlog "Transient log")
    (kohai--should-have-duration tlog)
    (let* ((result (kohai-req--transient-log-promote index))
           (transient-logs (cl-getf result :all))
           (label (cl-getf tlog :label)))
      (message "[Transient Log %02d, %s] properly promoted"
               index label)
      (kohai-sector--list)
      (kohai-project--list)
      (kohai-transient-log--list transient-logs))))

(provide 'kohai-transient-log)
;;; kohai-transient-log.el ends here
