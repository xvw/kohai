;;; kohai-transient.el --- Transient widget  -*- coding: utf-8; lexical-binding: t -*-

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

;; Some transient widgets used in Kohai

;;; Code:

(require 'transient)


;;; Global dashboard

(transient-define-prefix kohai-transient--dashboard ()
  "General Dashboard, opened when `kohai' is called."
  [["Supervised"
    ("dg" "get" kohai-get-supervised)
    ("ds" "set" kohai-set-supervised)]

   ["Sector"
    ("sl" "list" kohai-list-sectors)
    ("sn" "new" kohai-new-sector)]

   ["Projects"
    ("pl" "list" kohai-list-projects)
    ("pn" "new" kohai-new-project)]

   ["Transient logs"
    ("tll" "list" kohai-list-transient-log)
    ("tlr" "record" kohai-record-transient-log)]

   ["Other"
    ("q" "close" transient-quit-one)]])


(provide 'kohai-transient)
;;; kohai-transient.el ends here

