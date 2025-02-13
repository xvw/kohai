;;; kohai-buffer.el --- Buffer helpers for Kohai   -*- coding: utf-8; lexical-binding: t -*-

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

;; Buffer manipulatio

;;; Code:

(require 'vtable)
(require 'kohai-core)
(require 'kohai-req)

;;; Variables

(defvar kohai--vtable-default-divider "5px"
  "Define the default divider width for vtable.")

;;; Buffer helpers

(defun kohai-buffer--truncate-with (buffer-name &optional action)
  "Truncate and fill buffer BUFFER-NAME and perform ACTION (if non nil)."
  (let ((buff (get-buffer-create buffer-name)))
    (with-current-buffer buff
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char 1)
      (when action (funcall action buff))
      (setq buffer-read-only t))))

(defun kohai-buffer--with (buffer-name  &optional action)
  "Switch to truncated BUFFER-NAME and perform ACTION (if non nil)."
  (let ((buff (get-buffer-create buffer-name)))
    (kohai-buffer--truncate-with buffer-name action)
    (pop-to-buffer buff)))

;;; Buffer related to sectors

(defun kohai-buffer--with-sector (action)
  "Run ACTION inside the sector's buffer."
  (kohai-buffer--truncate-with
   kohai-sectors-buffer-name (lambda (_b) (funcall action))))

(defun kohai-buffer--sector-to-vtable-entry (sector)
  "Render a SECTOR into a vtable column."
  (list (propertize (cl-getf sector :name) 'face 'bold)
        (or (cl-getf sector :description) "")))

(defun kohai-buffer--sector-list-vtable (sectors)
  "Create the vtable displaying SECTORS."
  (lambda ()
    (make-vtable :columns '("Name" "Description")
                 :divider-width kohai--vtable-default-divider
                 :objects (mapcar #'kohai-buffer--sector-to-vtable-entry
                                  sectors))))

(defun kohai-buffer--sector-list (&optional given-sectors)
  "Return the list of sectors (or GIVEN-SECTORS) in a dedicated buffer."
  (let* ((sectors (or given-sectors (kohai-req--sector-list))))
    (if (<= (length sectors) 0)
        (kohai--error-no-entries "sectors")
      (kohai-buffer--with-sector
       (kohai-buffer--sector-list-vtable sectors)))))

(provide 'kohai-buffer)
;;; kohai-buffer.el ends here
