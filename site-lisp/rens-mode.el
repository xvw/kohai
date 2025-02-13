;;; rens-mode.el --- Modest mode for Rensai   -*- coding: utf-8; lexical-binding: t -*-

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

;; Request specific module

;;; Code:

(defvar rensai-constants
  '("null" "true" "false")
  "Constants for the Rensai Language.")

(defvar rensai-font-lock-defaults
  `((("\"\\.\\*\\?" . font-lock-string-face)
     ( ,(regexp-opt rensai-constants 'words) . font-lock-builtin-face)))
  "Default Font Lock for the Rensai Language.")

(define-derived-mode rens-mode fundamental-mode "Rensai"
  "Major mode for highlighting Rensai text buffer."
  (setq font-lock-defaults rensai-font-lock-defaults))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rens\\'" . rens-mode))

(provide 'rens-mode)
;;; rens-mode.el ends here
