;;; kohai-core.el --- Display information about the current state of a Kohai directory   -*- coding: utf-8; lexical-binding: t -*-

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

;; Tool to deal with Kohai States

;;; Code:

(require 'cl-lib)
(require 'vtable)
(require 'kohai-core)
(require 'kohai-req)
(require 'kohai-buffer)
(require 'kohai-generic)
(require 'kohai-sector)
(require 'kohai-project)
(require 'kohai-transient-log)

(defun kohai-state--kill-buffer (&optional _)
  "Kill buffer related to state."
  (kohai-buffer--kill kohai-logs-buffer-name)
  (kohai-buffer--kill kohai-state-buffer-name))

(defun kohai-state--log-unpromote (uuid)
  "Unpromote the log referenced by UUID."
  (when (yes-or-no-p (format "Unpromote log %s?" uuid))
    (let ((_ (kohai-req--log-unpromote uuid)))
      (kohai-state--kill-buffer)
      (kohai-transient-log--list)
      (pop-to-buffer kohai-transient-logs-buffer-name))))

(defun kohai-state--list-logs-vtable ()
  "Create a vtable displaying logs."
  (lambda (logs)
    (make-vtable
     :divider-width kohai--vtable-default-divider
     :objects (append logs nil)
     :columns '("Sector"
                "Project"
                "Start date"
                "Duration"
                "Label")
     :getter (lambda (o column vtable)
               (pcase (vtable-column vtable column)
                 ("Sector" (kohai--bold (cl-getf o :sector)))
                 ("Project" (or (cl-getf o :project) ""))
                 ("Start date" (cl-getf o :start_date_repr))
                 ("Duration" (cl-getf o :duration_repr))
                 ("Label" (cl-getf o :label))))
     :actions '("u" (lambda (o) (kohai-state--log-unpromote (cl-getf o :id)))
                "q" (lambda (_) (kohai-state--kill-buffer))))))

(defun kohai-state--list-state-enum ()
  "Create a vtable displaying state information."
  (lambda (state)
    (let ((big-bang (cl-getf state :big_bang))
          (end-of-world (cl-getf state :end_of_world))
          (number-of-logs (cl-getf state :number_of_logs))
          (duration (cl-getf state :duration)))
      (make-vtable
       :divider-width kohai--vtable-default-divider
       :columns '("Key" "Value")
       :objects (list (list :key "Number of logs" :value number-of-logs)
                      (list :key "Total duration" :value duration)
                      (list :key "Start date" :value big-bang)
                      (list :key "Last date" :value end-of-world))
       :getter (lambda (o column vtable)
                 (pcase (vtable-column vtable column)
                   ("Key" (kohai--bold (cl-getf o :key)))
                   ("Value" (cl-getf o :value))))
       :actions '("q" (lambda (_) (kohai-state--kill-buffer)))))))

(defun kohai-state--display-state (&optional state logs)
  "Display a list of LOGS based and a STATE."
  (let* ((current-state (or state (kohai-req--state-get)))
         (tail (or logs (kohai-req--log-tail)))
         (number-of-logs (or (cl-getf current-state :number_of_logs) 0)))
    (kohai--should-not-be-zero number-of-logs
                               "There is no logs for the current state"
                               (lambda () (kohai-state--kill-buffer)))
    (kohai-generic--vtable "logs"
                           kohai-logs-buffer-name
                           tail
                           (kohai-state--list-logs-vtable))
    (kohai-generic--vtable "state"
                           kohai-state-buffer-name
                           current-state
                           (kohai-state--list-state-enum))))

(defun kohai-state--pop-with-side-window ()
  "Pop the buffer for logs and side window."
  (display-buffer kohai-state-buffer-name
                  '(display-buffer-in-side-window .((side . bottom))))
  (pop-to-buffer kohai-logs-buffer-name))

(defun kohai-state--get ()
  "Get the global state of a supervised directory."
  (kohai-state--display-state)
  (kohai-state--pop-with-side-window))

(defun kohai-state--get-by-sector ()
  "Get the global state of a supervised directory for a given sector."
  (let* ((sector (kohai-sector--ac nil t nil))
         (state (kohai-req--state-get-sector sector))
         (tail (kohai-req--log-tail-sector sector)))
    (kohai-state--display-state state tail)
    (kohai-state--pop-with-side-window)))

(defun kohai-state--get-by-project ()
  "Get the global state of a supervised directory for a given project."
  (let* ((project (kohai-project--ac nil t nil))
         (state (kohai-req--state-get-project project))
         (tail (kohai-req--log-tail-project project)))
    (kohai-state--display-state state tail)
    (kohai-state--pop-with-side-window)))

(provide 'kohai-state)
;;; kohai-state.el ends here
