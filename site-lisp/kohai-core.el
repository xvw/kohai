;;; kohai-core.el --- The core of Kohai   -*- coding: utf-8; lexical-binding: t -*-

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

;; Core of Kohai

(require 'cl-lib)

;;; Code:

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

(defcustom kohai-projects-buffer-name "*kohai-projects*"
  "The name of the buffer listing sectors."
  :group 'kohai :type 'string)


(defcustom kohai-transient-logs-buffer-name "*kohai-transient-logs*"
  "The name of the buffer listing transient logs."
  :group 'kohai :type 'string)

(defcustom kohai-transient-log-buffer-name "*kohai-a-transient-log*"
  "The name of the buffer displaying a specific transient log."
  :group 'kohai :type 'string)

(defcustom kohai-logs-buffer-name "*kohai-logs*"
  "The name of the buffer listing logs."
  :group 'kohai :type 'string)

(defcustom kohai-state-buffer-name "*kohai-state*"
  "The name of the buffer displaying state."
  :group 'kohai :type 'string)

;;; Variables

(defvar kohai--connection nil
  "The Kohai JSONRPC instance.")

(defvar kohai--vtable-default-divider "5px"
  "Define the default divider width for vtable.")

;;; Guards

(defun kohai--ensure-connection ()
  "Ensure that the current session is connected to a Kohai server."
  (or kohai--connection (error "Not connected (use M-x kohai)")))

(defun kohai--ensure-supervision ()
  "Ensure that the current session is properly supervised."
  (and (kohai--ensure-connection)
       (or kohai-supervised (error "No supervised folder"))))

;;; Helpers

(defun kohai--trim-downcase (term)
  "Apply trim and downcase to TERM."
  (string-trim (downcase term)))

(defun kohai--nil-if-blank (term)
  "Return nil if a TERM is blank."
  (when (and term (not (string-blank-p term))) term))

(defun kohai--vector-empty-p (v)
  "Return t if V is an empty vector."
  (and (vectorp v) (zerop (length v))))

(defun kohai--read-datetime (prompt &optional default)
  "Read a duration or a datetime from the minibuffer with a PROMPT.
DEFAULT is the prefilled value."
  (let* ((value (or default "now"))
         (datetime (string-trim (read-string prompt value))))
    (if (or (string-blank-p datetime)
            (string= (downcase datetime) "now")) nil
      datetime)))

;;; Messages

(defun kohai--message-supervised (directory)
  "Display a message for DIRECTORY as a supervised folder."
  (if (not directory)
      (message "There is no supervised directory")
    (message "[%s] is supervised" directory)))

(defun kohai--message-stored (name &optional kind)
  "Message for NAME of type KIND stored."
  (let ((prefix (if kind (format "%s.%s" name kind) name)))
    (message "[%s] has been stored" prefix)))


(defun kohai--message-deleted (name &optional kind)
  "Message for NAME of type KIND deleted."
  (let ((prefix (if kind (format "%s.%s" name kind) name)))
    (message "[%s] has been deleted" prefix)))


(defun kohai--error-no-entries (kind)
  "Display an error if a list of KIND is empty."
  (error "[%s] is empty" kind))

(defun kohai--should-exists (value subject)
  "Display an error on SUBJECT is VALUE is nil."
  (when (not value)
    (error "[%s] does not exists" subject)))

(defun kohai--not-blank (value &optional name)
  "Display an error if a VALUE (with NAME) is blank."
  (let ((pname (or name "VALUE")))
    (when (not (kohai--nil-if-blank value))
      (error "[%s] cannot be blank (or empty)" pname))))

(defun kohai--should-be-eraseable (entry &optional name)
  "Display an error if an ENTRY (with NAME) is not eraseable."
  (let ((pname (or name "ENTRY"))
        (counter (cl-getf entry :counter)))
    (when (> counter 0)
      (error "[%s] cannot be erased (%d occurence(s))" pname counter))))

(defun kohai--should-have-duration (log)
  "Display an error if LOG does not have duration."
  (when (not (cl-getf log :duration))
    (error "[Transient log %02d, %s] need to be closed with a duration"
           (cl-getf log :index)
           (cl-getf log :label))))

(defun kohai--should-not-be-zero (value message &optional callback)
  "Fire an error (MESSAGE) and raise CALLBACK if a VALUE is zero."
  (when (zerop value)
    (when callback (funcall callback))
    (error "%s" message)))

;;; Text propertize

(defun kohai--bold (text)
  "Propertize TEXT in bold."
  (propertize text 'face 'bold))

(provide 'kohai-core)
;;; kohai-core.el ends here
