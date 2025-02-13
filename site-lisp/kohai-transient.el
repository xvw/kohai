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
    ("dg" "get" kohai-get-supervised :transient t)
    ("ds" "set" kohai-set-supervised :transient t)]

   ["Sector"
    ("sl" "list" kohai-list-sectors)
    ("sn" "new" kohai-new-sector)]

   ["Other"
    ("q" "close" transient-quit-one)]])



(provide 'kohai-transient)
;;; kohai-transient.el ends here

