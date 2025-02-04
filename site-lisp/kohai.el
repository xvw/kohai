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
(require 'vtable)

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
  "The name of the buffer listing displaying the Kohai errors."
  :group 'kohai :type 'string)

(defcustom kohai-sectors-buffer-name "*kohai-sectors*"
  "The name of the buffer listing displaying the Kohai sectors."
  :group 'kohai :type 'string)


;;; Variables

(defvar kohai--connection nil
  "The Kohai JSONRPC instance.")

;;; Custom modes for syntax highlighting

(defvar rensai-constants
  '("null" "true" "false")
  "Constants for the Rensai Language.")

(defvar rensai-font-lock-defaults
  `((("\"\\.\\*\\?" . font-lock-string-face)
     ( ,(regexp-opt rensai-constants 'words) . font-lock-builtin-face)))
  "Default Font Lock for the Rensai Language.")

(define-derived-mode rens-mode fundamental-mode "Rensai"
  "Major mode for highlighting Rensai text buffer."
  (setq font-lock-defaults rensai-font-lock-defaults))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rens\\'" . rens-mode))

(provide 'rens-mode)


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
                                        :stderr
                                        (get-buffer-create
                                         kohai-stderr-buffer-name)
                                        :noquery t))))
    (setq kohai--connection (funcall server))))

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

(defun kohai--get-sectors ()
  "Return the list of sectors in a dedicated buffer."
  (kohai--ensure-supervision)
  (kohai--fill-buffer
   kohai-sectors-buffer-name
   (lambda (_buff)
     (let* ((sectors (kohai--send :kohai/sector/list nil))
            (objects (mapcar
                      (lambda (elt)
                        (list (cl-getf elt :name)
                              (or (cl-getf elt :description) "No description")))
                      sectors)))
       (make-vtable
        :divider-width "2px"
        :columns '("Sector name" "Sector description")
        :objects objects)))))

;; Features

(defun kohai ()
  "Run a kohai process."
  (interactive)
  (when (not kohai--connection)
    (kohai--make-connection))
  (jsonrpc--message "Connected")
  (if kohai-supervised
      (progn (kohai--send :kohai/supervision/set kohai-supervised)
             (message "%s is supervised" kohai-supervised))
    (call-interactively #'kohai-supervised-set)))

(defun kohai-sector-save (name description)
  "Save a new sector with a NAME and a DESCRIPTION."
  (interactive "sName: \nsDescription of %s: ")
  (kohai--ensure-supervision)
  (let* ((real-name (string-trim name))
         (real-desc (string-trim description))
         (desc (if (string-blank-p real-desc) nil real-desc))
         (param (list :name real-name
                      :description desc)))
    (let ((_result (kohai--send :kohai/sector/save param)))
      (message "%s has been stored." real-name)
      (kohai--get-sectors))))

(defun kohai-sector-list ()
  "Get the list of sectors in a dedicated buffer."
  (interactive)
  (kohai--get-sectors)
  (switch-to-buffer-other-window
   (get-buffer-create kohai-sectors-buffer-name)))

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
