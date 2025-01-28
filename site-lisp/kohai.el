;;; kohai.el --- A strange timetracker   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) since 2025  The OCaml-eglot Project Contributors
;; Licensed under the MIT license.

;; Author: Xavier Van de Woestyne <xaviervdw@gmail.com>

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
  "The current path of the supervised directory"
  :group 'kohai
  :type 'directory)


(defcustom kohai-status-buffer-name "*kohai-status*"
  "The name of the buffer listing displaying the Kohai status."
  :group 'kohai :type 'string)


;;; Variables

(defvar kohai--connection nil
  "The Kohai JSONRPC instance.")

;;; Internal function

(defun kohai--make-connection ()
  "Initialize the connection with the Kohai server."
  (let ((server (apply-partially
                 #'make-instance 'jsonrpc-process-connection
                 :name "kohai"
                 :on-shutdown (lambda () (setq kohai--connection nil))
                 :process (make-process :name "kohai process"
                                        :connection-type 'pipe
                                        :coding 'utf-8-emacs-unix
                                        :command (list kohai-binary)
                                        :noquery t
                                        :stderr (get-buffer-create "*Kohai stderr*")))))
    (setq kohai--connection (funcall server))))

(defun kohai--ensure-connection ()
  "Ensure that the current session is connected to a Kohai server."
  (or kohai--connection (jsonrpc-error "Not connected (use M-x kohai)")))

(defun kohai--ensure-supervision ()
  "Ensure that the current session is properly supervised."
  (and (kohai--ensure-connection)
       (or kohai-supervised (jsonrpc-error "No supervised folder"))))

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


(defun kohai--invoke-status ()
  "Invoke the Kohai Status Buffer."
  (let ((status-buffer (get-buffer-create kohai-status-buffer-name)))
    (with-current-buffer status-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char 1)
      (insert "foo")
      (setq buffer-read-only t)
      (switch-to-buffer-other-window status-buffer))))

;; Features


(defun kohai ()
  "Run a kohai process."
  (interactive)
  (when (not kohai--connection)
    (kohai--make-connection))
  (jsonrpc--message "Connected")
  (when kohai-supervised
    (kohai--send :kohai/supervision/set kohai-supervised))
  (kohai--invoke-status))

(defun kohai-ping ()
  "Send the ping request (just for testing rpc-server)."
  (interactive)
  (kohai--ensure-connection)
  (let ((result (kohai--send :experimental/ping nil)))
    (message result)))

(defun kohai-supervised ()
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
         (result (completing-read "Foo: " folder-table))
         (absolute (expand-file-name result)))
    (setq kohai-supervised absolute)
    (let ((supervision (kohai--send :kohai/supervision/set absolute)))
      (message "%s is now the supervised directory" absolute))))

(provide 'kohai)
;;; kohai.el ends here
