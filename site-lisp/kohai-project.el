;;; kohai-project.el --- Deal with projects   -*- coding: utf-8; lexical-binding: t -*-

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

;; Interaction with projects

;;; Code:

(require 'kohai-core)
(require 'kohai-generic)

(defun kohai-project--ac (&optional projects not-empty default)
  "Get PROJECTS as a completion list.
If NOT-EMPTY the list must be filled. DEFAULT is the default value."
  (kohai-generic--ditem-ac "project"
                           projects
                           not-empty
                           default))


(defun kohai-project--list (&optional given-projects)
  "Return the list of projects (or GIVEN-PROJECTS)."
  (kohai-generic--ditem-list "project"
                             kohai-projects-buffer-name
                             given-projects))

(defun kohai-project--save (name desc)
  "Smartly save a project (with NAME and DESC)."
  (kohai-generic--ditem-save "project"
                             kohai-projects-buffer-name
                             name desc))

(defun kohai-project--update-desc (name)
  "Update the description of a project by his NAME."
  (kohai-generic--ditem-update-desc "project"
                                    kohai-projects-buffer-name
                                    name))

(defun kohai-project--new ()
  "Prompt a project's save procedure"
  (kohai-generic--ditem-new "project" kohai-projects-buffer-name))

(provide 'kohai-project)
;;; kohai-project.el ends here
