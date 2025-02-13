;;; kohai-sector.el --- Deal with sectors   -*- coding: utf-8; lexical-binding: t -*-

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

;; Interaction with sectors

;;; Code:

(require 'vtable)
(require 'kohai-core)
(require 'kohai-req)
(require 'kohai-buffer)

(defun kohai-sector--ac (&optional sectors)
  "Get SECTORS as a completion list."
  (let* ((sector-entries
          (mapcar (lambda (sector)
                    (let* ((name (cl-getf sector :name))
                           (desc (cl-getf sector :description))
                           (key (format "%s: %s" (kohai--bold name) desc)))
                      (cons key name)))
                  (or sectors (kohai-req--sector-list))))
         (selected (completing-read "Sector: "
                                    sector-entries
                                    nil nil nil t)))
    (or (alist-get selected sector-entries nil nil #'equal) selected)))

(defun kohai-sector--with-buffer (action)
  "Run ACTION inside the sector's buffer."
  (kohai-buffer--truncate-with
   kohai-sectors-buffer-name (lambda (_b) (funcall action))))

(defun kohai-sector--to-vtable-entry (sector)
  "Render a SECTOR into a vtable column."
  (list (kohai--bold (cl-getf sector :name))
        (or (cl-getf sector :description) "")))

(defun kohai-sector--list-vtable (sectors)
  "Create the vtable displaying SECTORS."
  (lambda ()
    (make-vtable :columns '("Name" "Description")
                 :divider-width kohai--vtable-default-divider
                 :objects (mapcar #'kohai-sector--to-vtable-entry
                                  sectors)
                 :actions '("d" (lambda (o)
                                  (kohai-sector--update-desc (car o)))))))

(defun kohai-sector--list (&optional given-sectors)
  "Return the list of sectors (or GIVEN-SECTORS) in a dedicated buffer."
  (let* ((sectors (or given-sectors (kohai-req--sector-list))))
    (if (<= (length sectors) 0)
        (kohai--error-no-entries "sectors list")
      (kohai-sector--with-buffer (kohai-sector--list-vtable sectors)))))

(defun kohai-sector--save (name desc)
  "Smartly save a sector (with NAME and DESC)."
  (let ((sectors (kohai-req--sector-save name desc)))
    (kohai--message-stored name "sector")
    (kohai-sector--list sectors)))

(defun kohai-sector--update-desc (name)
  "Update the description of a sector by his NAME."
  (let ((sector (kohai-req--sector-get name)))
    (kohai--should-exists sector "sector")
    (let* ((old-desc (cl-getf sector :description))
           (new-desc (read-string (format "New description (%s): " name)
                                  old-desc)))
      (kohai-sector--save name new-desc))))

(provide 'kohai-sector)
;;; kohai-sector.el ends here

