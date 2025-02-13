;;; kohai-buffer.el --- Buffer helpers for Kohai   -*- coding: utf-8; lexical-binding: t -*-

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

;; Buffer manipulatio

;;; Code:

(defun kohai-buffer--truncate-with (buffer-name &optional action)
  "Truncate and fill buffer BUFFER-NAME and perform ACTION (if non nil)."
  (let ((buff (get-buffer-create buffer-name)))
    (with-current-buffer buff
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char 1)
      (when action (funcall action buff))
      (setq buffer-read-only t))))

(defun kohai-buffer--with (buffer-name  &optional action)
  "Switch to truncated BUFFER-NAME and perform ACTION (if non nil)."
  (let ((buff (get-buffer-create buffer-name)))
    (kohai-buffer--truncate-with buffer-name action)
    (pop-to-buffer buff)))

(provide 'kohai-buffer)
;;; kohai-buffer.el ends here

