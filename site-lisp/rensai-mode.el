;;; rensai-mode.el --- Modest mode for Rensai   -*- coding: utf-8; lexical-binding: t -*-

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

;; WORK IN PROGRESS

;;; Code:

(require 'treesit)

;; Define a syntax table for basic symbols and escaping
(defvar rensai-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" st)  ;; double quotes for strings
    (modify-syntax-entry ?\\ "\\" st)  ;; backslash for escaping
    st))

;; Define the mode
(define-derived-mode rensai-mode prog-mode "Rensai"
  "Major mode for editing Rensai files."
  :syntax-table rensai-mode-syntax-table
  (when (treesit-ready-p 'rensai)
    (treesit-parser-create 'rensai)      ;; Create the Tree-sitter parser
    (treesit-font-lock-enable)))         ;; Enable Tree-sitter-based font-locking

(add-to-list 'auto-mode-alist '("\\.rens\\'" . rensai-mode))

(provide 'rensai-mode)
