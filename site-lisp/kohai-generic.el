;;; kohai-generic.el --- Deal with generic items   -*- coding: utf-8; lexical-binding: t -*-

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

;;; Some generics utils to deal with generic items

;;; Code:

(require 'vtable)
(require 'kohai-core)
(require 'kohai-req)
(require 'kohai-buffer)

(defun kohai-generic--vtable (key buffer entries vtable)
  "Fill a BUFFER with ENTRIES into a VTABLE (KEY is used for reporting)."
  (if (kohai--vector-empty-p entries)
      (progn (kohai-buffer--empty buffer t)
             (kohai--error-no-entries (format "%s list" key)))
    (kohai-buffer--truncate-with
     buffer (lambda (_buf) (funcall vtable entries)))))

(defun kohai-generic--ditem-ac (key &optional entries not-empty default)
  "Get ENTRIES (dispatched on KEY) as a completion list.
If NOT-EMPTY the list must be filled.  DEFAULT is the default value."
  (let ((given-entries (or entries (kohai-req--described-item-list key))))
    (when (and not-empty (kohai--vector-empty-p given-entries))
      (kohai--error-no-entries key))
    (let* ((formatted-entries
            (mapcar (lambda (entry)
                      (let* ((name (cl-getf entry :name))
                             (desc (cl-getf entry :description))
                             (key (format "%s: %s" (kohai--bold name) desc)))
                        (cons key name)))
                    given-entries))
           (selected (completing-read (format "%s: " (capitalize key))
                                      formatted-entries
                                      nil nil default t)))
      (or (alist-get selected formatted-entries nil nil #'equal) selected))))

(defun kohai-generic--ditem-save (key buffer name desc)
  "Smartly save a ditem (with NAME and DESC) using KEY.
Into BUFFER."
  (let ((entries (kohai-req--described-item-save key name desc)))
    (kohai--message-stored name key)
    (kohai-generic--ditem-list key buffer entries)))

(defun kohai-generic--ditem-update-desc (key buffer name)
  "Update the description of an entry by his NAME using KEY.
In BUFFER."
  (let ((entry (kohai-req--described-item-get key name)))
    (kohai--should-exists entry key)
    (let* ((old-desc (cl-getf entry :description))
           (new-desc (read-string (format "New description (%s): " name)
                                  old-desc)))
      (kohai-generic--ditem-save key buffer name new-desc))))

(defun kohai-generic--ditem-list-vtable (key buffer)
  "Create the vtable displaying ENTRIES using KEY.
In BUFFER, CREATE is used to create a new entry."
  (lambda (entries)
    (make-vtable :columns '("Name" "Description" "Deleteable")
                 :divider-width kohai--vtable-default-divider
                 :objects (append entries nil)
                 :getter (lambda (o column vtable)
                              (pcase (vtable-column vtable column)
                                ("Name" (kohai--bold (cl-getf o :name)))
                                ("Description" (or (cl-getf o :description) ""))
                                ("Deleteable"
                                 (if (> (cl-getf o :counter) 0) "X" " "))))
                 :actions `("u" ,(lambda (o)
                                   (kohai-generic--ditem-update-desc
                                    key buffer (cl-getf o :name)))
                            "d" ,(lambda (o)
                                   (kohai-generic--ditem-delete
                                    key buffer (cl-getf o :name)))
                            "n" ,(lambda (_o)
                                   (kohai-generic--ditem-new key buffer))
                            "q" ,(lambda (_o)
                                   (kohai-buffer--kill buffer))))))

(defun kohai-generic--ditem-list (key buffer &optional given-entries)
  "Return the list of entries (or GIVEN-ENTRIES).
In a dedicated BUFFER using KEY."
  (let ((entries (or given-entries (kohai-req--described-item-list key))))
    (kohai-generic--vtable key
                          buffer
                          entries
                          (kohai-generic--ditem-list-vtable key buffer))))

(defun kohai-generic--ditem-new (key buffer)
  "Save a generic item (defined by KEY) and switch to BUFFER."
  (let* ((prefix (capitalize key))
         (name (read-string (format "%s name: " prefix)))
         (desc (read-string (format "%s description: " prefix))))
    (kohai-generic--ditem-save key buffer name desc)
    (pop-to-buffer buffer)))

(defun kohai-generic--ditem-delete (key buffer name)
  "Delete a generic item NAME (defined by KEY) and switch to BUFFER."
  (let ((entry (kohai-req--described-item-get key name)))
    (kohai--should-exists entry key)
    (kohai--should-be-eraseable entry)
    (when (y-or-n-p (format "Delete %s %s? " key name))
      (let ((values (kohai-req--described-item-delete key name)))
        (kohai--message-deleted name key)
        (kohai-generic--ditem-list key buffer values)))))

(provide 'kohai-generic)
;;; kohai-generic.el ends here
