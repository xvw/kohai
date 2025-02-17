;;; kohai.el --- A strange timetracker   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) since 2025  Xavier Van de Woestyne
;; Licensed under the MIT license.

;; Author: Xavier Van de Woestyne <xaviervdw@gmail.com>

;; This file is NOT part of GNU Emac

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
(require 'rens-mode)
(require 'kohai-core)
(require 'kohai-req)
(require 'kohai-sector)
(require 'kohai-project)
(require 'kohai-transient-log)
(require 'kohai-transient)

;;; Features

(defun kohai-set-supervised ()
  "Set interactively the current supervised directory."
  (interactive)
  (kohai--ensure-connection)
  (let* ((directories
          (apply-partially #'completion-table-with-predicate
                           #'completion-file-name-table
                           #'file-directory-p
                           'strict))
         (dir-completion (completion-table-in-turn directories))
         (chosen-directory (completing-read "Supervised directory: "
                                            dir-completion))
         (absolute-path (expand-file-name chosen-directory)))
    (kohai-req--supervised-set absolute-path)
    (setq kohai-supervised absolute-path)
    (customize-save-variable 'kohai-supervised absolute-path)
    (kohai--message-supervised absolute-path)))

(defun kohai-get-supervised ()
  "Display, in the minibuffer, the current supervised directory."
  (interactive)
  (kohai--ensure-connection)
  (let ((supervised (kohai-req--supervised-get)))
    (kohai--message-supervised supervised)))

(defun kohai-list-sectors ()
  "Fill the sector's buffer."
  (interactive)
  (kohai--ensure-supervision)
  (kohai-sector--list)
  (pop-to-buffer kohai-sectors-buffer-name))

(defun kohai-new-sector ()
  "Save a new sector."
  (interactive)
  (kohai--ensure-supervision)
  (kohai-sector--new))

(defun kohai-edit-sector ()
  "Edit sector's description."
  (interactive)
  (kohai--ensure-supervision)
  (let ((sector-name (kohai-sector--ac nil t)))
    (kohai-sector--update-desc sector-name)
    (pop-to-buffer kohai-sectors-buffer-name)))


(defun kohai-list-projects ()
  "Fill the project's buffer."
  (interactive)
  (kohai--ensure-supervision)
  (kohai-project--list)
  (pop-to-buffer kohai-projects-buffer-name))

(defun kohai-new-project ()
  "Save a new project."
  (interactive)
  (kohai--ensure-supervision)
  (kohai-project--new))

(defun kohai-edit-project ()
  "Edit project's description."
  (interactive)
  (kohai--ensure-supervision)
  (let ((project-name (kohai-project--ac nil t)))
    (kohai-project--update-desc project-name)
    (pop-to-buffer kohai-projects-buffer-name)))

(defun kohai-list-transient-log ()
  "Fill the transient-log's buffer."
  (interactive)
  (kohai--ensure-supervision)
  (kohai-transient-log--list)
  (pop-to-buffer kohai-transient-logs-buffer-name))

(defun kohai-record-transient-log ()
  "Record a transient log."
  (interactive)
  (kohai--ensure-supervision)
  (kohai-transient-log--record)
  (pop-to-buffer kohai-transient-logs-buffer-name))


(defun kohai ()
  "Launch Kohai."
  (interactive)
  (when (not kohai--connection) (kohai-req--make-connection))
  (if (and kohai-supervised (not (string-blank-p kohai-supervised)))
      (progn (kohai-req--supervised-set kohai-supervised)
             (kohai--message-supervised kohai-supervised)
             (call-interactively #'kohai-transient--dashboard))
    (call-interactively #'kohai-set-supervised)))

(provide 'kohai)
;;; kohai.el ends here
