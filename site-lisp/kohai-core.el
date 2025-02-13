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


(defcustom kohai-transient-logs-buffer-name "*kohai-transient-logs*"
  "The name of the buffer listing transient logs."
  :group 'kohai :type 'string)

;;; Variables

(defvar kohai--connection nil
  "The Kohai JSONRPC instance.")

;;; Messages

(defun kohai--message-supervised (directory)
  "Display a message for DIRECTORY as a supervised folder."
  (if (not directory)
      (message "There is no supervised directory")
    (message "[%s] is supervised" directory)))

(defun kohai--error-no-entries (kind)
  "Display an error if a list of KIND is empty."
  (error "%s is empty" kind))


;;; Internal functions

(defun kohai--ensure-connection ()
  "Ensure that the current session is connected to a Kohai server."
  (or kohai--connection (error "Not connected (use M-x kohai)")))

(defun kohai--ensure-supervision ()
  "Ensure that the current session is properly supervised."
  (and (kohai--ensure-connection)
       (or kohai-supervised (error "No supervised folder"))))


(provide 'kohai-core)
;;; kohai-core.el ends here
