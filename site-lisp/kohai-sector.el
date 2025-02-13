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
(require 'kohai-generic)

(defvar-local selected-key "sector"
  "The key for the generic instantiation.")


(defvar-local selected-buffer kohai-sectors-buffer-name
    "The targeted buffer.")


(defun kohai-sector--ac (&optional sectors not-empty)
  "Get SECTORS as a completion list.
If NOT-EMPTY the list must be filled."
  (kohai-generic--ditem-ac selected-key
                           sectors
                           not-empty))

(defun kohai-sector--list (&optional given-sectors)
  "Return the list of sectors (or GIVEN-SECTORS)."
  (kohai-generic--ditem-list selected-key
                             selected-buffer
                             given-sectors))

(defun kohai-sector--save (name desc)
  "Smartly save a sector (with NAME and DESC)."
  (kohai-generic--ditem-save selected-key
                             selected-buffer
                             name desc))

(defun kohai-sector--update-desc (name)
  "Update the description of a sector by his NAME."
  (kohai-generic--ditem-update-desc selected-key
                                    selected-buffer
                                    name))

(provide 'kohai-sector)
;;; kohai-sector.el ends here

