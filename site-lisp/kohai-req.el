;;; kohai-req.el --- Kohai request   -*- coding: utf-8; lexical-binding: t -*-

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

;; Request specific module

;;; Code:

(require 'jsonrpc)
(require 'kohai-core)

(defun kohai-req--make-connection ()
  "Initialize the connection with the Kohai server."
  (let ((server (apply-partially
                 #'make-instance 'jsonrpc-process-connection
                 :name "kohai"
                 :on-shutdown (lambda () (setq kohai--connection nil))
                 :process(make-process
                          :name "kohai process"
                          :connection-type 'pipe
                          :coding 'utf-8-emacs-unix
                          :command (list kohai-binary)
                          :stderr
                          (get-buffer-create
                           kohai-stderr-buffer-name)
                          :noquery t))))
    (setq kohai--connection (funcall server))))


(cl-defun kohai-req--send (method
                           &optional params &key
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


(defun kohai-req--constr (ctor &optional value)
  "Build a sum type (based on CTOR and VALUE)."
  (list :ctor ctor
        :value value))

;;; Request related to the supervised directory

(defun kohai-req--supervised-get ()
  "A request that return the current supervised folder."
  (kohai-req--send :kohai/supervision/get))

(defun kohai-req--supervised-set (directory)
  "A request to patch the DIRECTORY supervised by the server."
  (kohai-req--send :kohai/supervision/set directory))

;;; Described item requests

(defun kohai-req--described-item-list (subject)
  "Use SUBJECT to request a generalized described item."
  (kohai-req--send (intern (format ":kohai/%s/list" subject))))

(defun kohai-req--described-item-save (subject name desc)
  "A request that store a described item via a NAME and a DESC in SUBJECT."
  (let* ((pname (kohai--trim-downcase name))
         (pdesc (kohai--trim-downcase desc)))
    (kohai--not-blank pname (format "%s.name" subject))
    (let ((param (list :name pname
                       :description (kohai--nil-if-blank pdesc))))
      (kohai-req--send (intern (format ":kohai/%s/save" subject))
                       param))))


(defun kohai-req--described-item-get (subject name)
  "A request that retreive a SUBJECT by his NAME."
  (kohai-req--send (intern (format ":kohai/%s/get" subject)) name))

;;; Request related to sectors

(defun kohai-req--sector-list ()
  "A request that return the list of stored sectors."
  (kohai-req--described-item-list "sector"))

(defun kohai-req--sector-save (name desc)
  "A request that store a sector via a NAME and a DESC."
  (kohai-req--described-item-save "sector" name desc))

(defun kohai-req--sector-get (name)
  "A request that retreive a sector by his NAME."
  (kohai-req--described-item-get "sector" name))

;;; Request related to projects

(defun kohai-req--project-list ()
  "A request that return the list of stored project."
  (kohai-req--described-item-list "project"))

(defun kohai-req--project-save (name desc)
  "A request that store a project via a NAME and a DESC."
  (kohai-req--described-item-save "project" name desc))

(defun kohai-req--project-get (name)
  "A request that retreive a project by his NAME."
  (kohai-req--described-item-get "project" name))

;;; Request related to transient logs

(defun kohai-req--transient-log-list ()
  "A request that return the list of transient logs."
  (kohai-req--send :kohai/transient-log/list))

(defun kohai-req--transient-log-get (index)
  "A request that return one transient log by INDEX."
  (kohai-req--send :kohai/transient-log/get index))

(defun kohai-req--transient-log-action (action &optional value)
  "A request that perform an ACTION (with VALUE) on transient logs."
  (kohai-req--send :kohai/transient-log/action
                   (kohai-req--constr action value)))

(defun kohai-req--transient-log-record (param)
  "A request that record a log with PARAM."
  (kohai-req--transient-log-action "record" param))

(defun kohai-req--transient-log-stop-recording (index)
  "A request that stop the recording of a log with INDEX."
  (kohai-req--transient-log-action "stop_recording" index))

(defun kohai-req--transient-log-rewrite (param)
  "A request that rewrite an existing transient log with PARAM."
  (kohai-req--transient-log-action "rewrite" param))

(defun kohai-req--transient-log-delete (index)
  "A request that delete a transient log by INDEX."
  (kohai-req--transient-log-action "delete" (list :index index)))

(provide 'kohai-req)
;;; kohai-req.el ends here
