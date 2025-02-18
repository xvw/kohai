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

(require 'kohai-core)
(require 'kohai-req)
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
    (list :start_date at-time
          :sector sector
          :project project
          :label label)))

(defun kohai-transient-log--to-vtable-entry (log)
  "Render LOG as a vtable entry."
  (list (cl-getf log :index)
        (kohai--bold (cl-getf log :sector))
        (or  (cl-getf log :project) "")
        (cl-getf log :start_date)
        (or (cl-getf log :duration) "?")
        (cl-getf log :label)))

(defun kohai-transient-log--list-vtable ()
  "Create the vtable displaying transient logs."
  (lambda (entries)
    (make-vtable
     :divider-width kohai--vtable-default-divider
     :objects (mapcar #'kohai-transient-log--to-vtable-entry
                      entries)
     :columns '("Index"
                "Sector"
                "Project"
                "Start date"
                "Duration"
                "Label")
     :actions '("c" (lambda (o)
                      (kohai-transient-log--stop-recording
                       (car o)))
                "r" (lambda (o)
                      (kohai-transient-log--rewrite
                       (car o)))
                "d" (lambda (o)
                      (kohai-transient-log--delete
                       (car o)))
                "n" (lambda (_) (kohai-transient-log--record))
                "m" (lambda (o)
                      (kohai-transient-log--handle-meta
                       (car o)))))))

(defun kohai-transient-log--list (&optional given-entries)
  "Return the list of entries (or GIVEN-ENTRIES) in log buffer."
  (let ((entries (or given-entries (kohai-req--transient-log-list))))
    (kohai-generic-vtable
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
    (let ((index (cl-getf log :index))
          (v (format "Close %s\t" (kohai-transient-log--minimize log))))
      (when (yes-or-no-p v)
        (let ((logs (kohai-req--transient-log-stop-recording
                     (list :index index))))
          (kohai-transient-log--list (cl-getf logs :all)))))))

(defun kohai-transient-log--record ()
  "Record a transient log."
  (let* ((param (kohai-transient-log--prompt-record))
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
    (when (yes-or-no-p (format "Delete %s? " entry))
      (let ((logs (kohai-req--transient-log-delete index)))
        (kohai-transient-log--list (cl-getf logs :all))))))


(defun kohai-transient-log--meta-add (entry)
  "Prompt metadata adding for ENTRY."
  (let* ((key (string-trim (read-string "Key: ")))
        (value (string-trim (read-string "Value: ")))
        (index (cl-getf entry :index))
        (_ (kohai-req--transient-log-add-meta index key value))
        (entry (kohai-req--transient-log-get index)))
    (kohai-transient-log--list-meta entry)))

(defun kohai-transient-log--meta-remove (entry meta)
  "Remove the selected META for the given ENTRY."
  (let ((index (cl-getf entry :index)))
    (when (yes-or-no-p (format "Delete the meta [%s] for %d ?" meta index))
      (let ((_ (kohai-req--transient-log-remove-meta index meta))
            (entry (kohai-req--transient-log-get index)))
        (kohai-transient-log--list-meta entry)))))

(defun kohai-transient-log--meta-entry (meta)
  "Render a META for a vtable."
  (list (kohai--bold (cl-getf meta :key))
        (cl-getf meta :value)))

(defun kohai-transient-log--meta-vtable (meta entry)
  "Create the vtable for displaying meta of an ENTRY with META."
  (make-vtable
   :columns '("Key" "Value")
   :divider-width kohai--vtable-default-divider
   :objects (mapcar #'kohai-transient-log--meta-entry
                    meta)
   :actions `("n" ,(lambda (_o)
                     (kohai-transient-log--meta-add entry))
              "d" ,(lambda (o)
                     (kohai-transient-log--meta-remove entry (car o)))
              "q" ,(lambda (_o)
                     (kill-buffer
                      kohai-transient-log-buffer-name)))))

(defun kohai-transient-log--list-meta (entry)
  "List META related to ENTRY."
  (kohai-buffer--truncate-with
   kohai-transient-log-buffer-name
   (lambda (_buff)
     (let ((meta (append (cl-getf entry :meta) nil)))
       (if meta
           (progn
             (kohai-transient-log--meta-vtable meta entry)
             (display-buffer
              kohai-transient-log-buffer-name
              '(display-buffer-in-side-window .((side . bottom))))
             (pop-to-buffer kohai-transient-log-buffer-name))
         (when (yes-or-no-p "No metadata, adding it? ")
           (kohai-transient-log--meta-add entry)))))))

(defun kohai-transient-log--handle-meta (index)
  "Handle metadata of transient log referenced by INDEX."
  (let ((entry (kohai-req--transient-log-get index)))
    (kohai--should-exists entry "transient log")
    (kohai-transient-log--list-meta entry)))

(provide 'kohai-transient-log)
;;; kohai-transient-log.el ends here
