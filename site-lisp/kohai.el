;;; kohai.el --- A strange timetracker   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) since 2025  Xavier Van de Woestyne
;; Licensed under the MIT license.

;; Author: Xavier Van de Woestyne <xaviervdw@gmail.com>

;; This file is NOT part of GNU Emac

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

(require 'jsonrpc)
(require 'rens-mode)
(require 'kohai-core)
(require 'kohai-req)
(require 'kohai-buffer)

;;; Features

(defun kohai-supervise ()
  "Set interactively the current supervised directory."
  (interactive)
  (kohai--ensure-connection)
  (let* ((directories
          (apply-partially #'completion-table-with-predicate
                           #'completion-file-name-table
                           #'file-directory-p
                           'strict))
         (dir-completion (completion-table-in-turn directories))
         (chosen-directory (completing-read "Supervised directory: "
                                            dir-completion))
         (absolute-path (expand-file-name chosen-directory)))
    (kohai-req--supervised-set absolute-path)
    (setq kohai-supervised absolute-path)
    (customize-save-variable 'kohai-supervised absolute-path)
    (kohai--message-supervised absolute-path)))

(defun kohai-supervised ()
  "Display, in the minibuffer, the current supervised directory."
  (interactive)
  (kohai--ensure-connection)
  (let ((supervised (kohai-req--supervised-get)))
    (kohai--message-supervised supervised)))

(defun kohai-sectors ()
  "Fill the sector's buffer."
  (interactive)
  (kohai--ensure-supervision)
  (kohai-buffer--sector-list)
  (pop-to-buffer kohai-sectors-buffer-name))

(defun kohai ()
  "Launch Kohai."
  (interactive)
  (when (not kohai--connection) (kohai-req--make-connection))
  (if (and kohai-supervised (not (string-blank-p kohai-supervised)))
      (progn (kohai-req--supervised-set kohai-supervised)
             (kohai--message-supervised kohai-supervised))
    (call-interactively #'kohai-supervise)))

(provide 'kohai)
;;; kohai.el ends here
