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

;; A client for the Kohai server in order to manage timetracking from Emacs

;;; Code:

(defgroup kohai nil
  "Interaction from Emacs to a Kohai server."
  :link '(url-link "https://xvw.lol")
  :group 'tool
  :prefix "kohai-")

;;; Custom variables

(defcustom kohai-server-uri nil
  "The uri for reaching a Kohai server."
  :group 'kohai
  :type 'string)

(defcustom kohai-server-port 8888
  "The port of the kohai server."
  :group 'kohai
  :type 'natnum)

;;; Features

(defun kohai--server ()
  "Compute the uri of the server."
  (let ((host (or kohai-server-uri "http://localhost")))
    (concat host ":" kohai-server-port)))

(provide 'kohai)
;;; kohai.el ends here
