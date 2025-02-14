;;; kohai-transient-log.el --- Deal with Transient logs  -*- coding: utf-8; lexical-binding: t -*-

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

;; Interaction with transient logs

;;; Code:

(require 'kohai-core)
(require 'kohai-sector)
(require 'kohai-project)


(defun kohai-transient-log--prompt-record (date sector project label)
  "Create params for recording a transient log.
DATE, SECTOR, PROJECT and LABEL can be pre-filled (for edition)."
  (kohai--ensure-supervision)
  (let* ((sector (kohai-sector--ac nil nil sector))
         (project (kohai--nil-if-blank (kohai-project--ac nil nil project)))
         (at-time (kohai--read-datetime "When: " date))
         (label (string-trim (read-from-string "# " label))))
    (kohai--not-blank label "label")
    (list :start-date at-time
          :sector sector
          :project project
          :label label)))



(provide 'kohai-transient-log)
;;; kohai-transient-log.el ends here
