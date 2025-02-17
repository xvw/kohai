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

(require 'kohai-core)
(require 'kohai-generic)

(defun kohai-sector--ac (&optional sectors not-empty default)
  "Get SECTORS as a completion list.
If NOT-EMPTY the list must be filled.  DEFAULT is the default value."
  (kohai-generic--ditem-ac "sector"
                           sectors
                           not-empty
                           default))


(defun kohai-sector--list (&optional given-sectors)
  "Return the list of sectors (or GIVEN-SECTORS)."
  (kohai-generic--ditem-list "sector"
                             kohai-sectors-buffer-name
                             given-sectors))

(defun kohai-sector--save (name desc)
  "Smartly save a sector (with NAME and DESC)."
  (kohai-generic--ditem-save "sector"
                             kohai-sectors-buffer-name
                             name desc))

(defun kohai-sector--update-desc (name)
  "Update the description of a sector by his NAME."
  (kohai-generic--ditem-update-desc "sector"
                                    kohai-sectors-buffer-name
                                    name))

(defun kohai-sector--new ()
  "Prompt a sector's save procedure"
  (kohai-generic--ditem-new "sector" kohai-sectors-buffer-name))


(provide 'kohai-sector)
;;; kohai-sector.el ends here
