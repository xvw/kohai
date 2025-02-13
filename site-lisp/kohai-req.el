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


(cl-defun kohai-req--send (method params &key
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


;;; Request related to the supervised directory

(defun kohai-req--supervised-get ()
  "A request that return the current supervised folder."
  (kohai-req--send :kohai/supervision/get nil))

(defun kohai-req--supervised-set (directory)
  "A request to patch the DIRECTORY supervised by the server."
  (kohai-req--send :kohai/supervision/set directory))


(provide 'kohai-req)
;;; kohai-req.el ends here
