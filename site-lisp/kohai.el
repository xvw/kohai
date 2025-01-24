;;; kohai.el --- A strange timetracker   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) since 2025  The OCaml-eglot Project Contributors
;; Licensed under the MIT license.

;; Author: Xavier Van de Woestyne <xaviervdw@gmail.com>

;; Maintainer: Xavier Van de Woestyne <xaviervdw@gmail.com>
;; Created: 24 January 2025
;; Keywords: tool timetracker productivity
;; URL: https://github.com/xvw/kohai
;; Package-Requires: ((emacs "29.1"))
;; Package-Version: 0.1
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A client for the Kohai server in order to manage timetracking from emacs

;;; Code:

(defgroup kohai nil
  "Interaction from Emacs to a Kohai server"
  :link '(url-link "https://xvw.lol")
  :group 'tool
  :prefix "kohai-")

(provide 'kohai)
;;; kohai.el ends here
