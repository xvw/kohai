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
    (make-vtable :divider-width kohai--vtable-default-divider
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
                                   (car o)))))))

(defun kohai-transient-log--list (&optional given-entries)
  "Return the list of entries (or GIVEN-ENTRIES) in log buffer."
  (let ((entries (or given-entries (kohai-req--transient-log-list))))
    (kohai-generic-vtable "transient-log"
                          kohai-transient-logs-buffer-name
                          entries
                          (kohai-transient-log--list-vtable))))

(defun kohai-transient-log--ask-for-closing-outdated (outdated)
  "Perform interactively closing of OUTDATED entries."
  (dolist (log (append outdated nil))
    (let* ((index (cl-getf log :index))
           (sdate (cl-getf log :start_date))
           (sector (cl-getf log :sector))
           (project (cl-getf log :project))
           (label (cl-getf log :label))
           (v (format "%02d. %s: %s (%s, %s)\t"
                      index sdate label sector
                      (or project "N/A"))))
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


(provide 'kohai-transient-log)
;;; kohai-transient-log.el ends here
