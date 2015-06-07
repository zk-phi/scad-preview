;;; scad-preview.el --- Preview SCAD models in real-time in Emacs

;; Copyright (C) 2013-2015 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 0.1.0

;;; Commentary:

;; Install `scad-mode' and load this script:
;;
;;   (require 'scad-preview)
;;
;; then call `scad-preview-mode' in a `scad-mode' buffer.
;;
;; You can rotate the preview image with following keybinds:
;;
;; - <right>   scad-preview-rotz+
;; - <left>    scad-preview-rotz-
;; - <up>      scad-preview-dist-
;; - <down>    scad-preview-dist+
;; - C-<left>  scad-preview-roty+
;; - C-<right> scad-preview-roty-
;; - C-<up>    scad-preview-rotx+
;; - C-<down>  scad-preview-rotx-
;; - M-<left>  scad-preview-trnsx+
;; - M-<right> scad-preview-trnsx-
;; - M-<up>    scad-preview-trnsz-
;; - M-<down>  scad-preview-trnsz+
;; - r         scad-preview-reset-camera-parameters
;;
;; You can also use "hjkl" or "npbf" instead of arrow keys.

;;; Change Log:

;; 0.1.0 test release

;;; Code:

(require 'scad "scad-mode")

(defconst scad-preview-version "0.1.0")

;; + customs

(defgroup scad-preview nil
  "Preview SCAD models in real-time in Emacs."
  :group 'emacs)

(defcustom scad-preview-default-camera-parameters '(0 0 0 50 0 20 500)
  "Default parameters for the Gimbal camera."
  :group 'scad-preview)

(defcustom scad-preview-update-delay 1.5
  "Delay in seconds until updating preview."
  :group 'scad-preview)

(defcustom scad-preview-image-size '(450 . 450)
  "Size of preview image."
  :group 'scad-preview)

(defcustom scad-preview-window-position 'right
  "Position of the preview window. The value can be either 'right,
  'left, 'below, or 'above."
  :group 'scad-preview)

(defcustom scad-preview-window-size 65
  "Size in columns(lines) of the preview window."
  :group 'scad-preview)

(defcustom scad-preview-colorscheme "Cornfield"
  "Colorscheme for rendering preview."
  :group 'scad-preview)

;; + core functions/variables

(defvar scad-preview-mode               nil)
(defvar scad-preview--buffer            nil)
(defvar scad-preview--source-buffer     nil)
(defvar scad-preview--camera-parameters nil)
(defvar scad-preview--timer-object      nil)
(defvar scad-preview--modified-flag     nil)

(defun scad-preview--after-change-function (&rest _)
  (setq scad-preview--modified-flag t))

(defun scad-preview-reset-camera-parameters ()
  (interactive)
  (setq scad-preview--camera-parameters
        (copy-sequence scad-preview-default-camera-parameters))
  (scad-preview--update))

(defun scad-preview--increment-camera-parameter (index val)
  (let ((cell (nthcdr index scad-preview--camera-parameters)))
    (setcar cell (+ (car cell) val))
    (scad-preview--update)))

(defun scad-preview--start ()
  (unless scad-preview-mode
    (setq scad-preview-mode           t
          scad-preview--buffer        (get-buffer-create "*SCAD Preview*")
          scad-preview--source-buffer (current-buffer)
          scad-preview--timer-object
          (run-with-idle-timer
           scad-preview-update-delay t
           (lambda ()
             (when scad-preview--modified-flag
               (setq scad-preview--modified-flag nil)
               (scad-preview--update)))))
    (with-selected-window (split-window (selected-window)
                                        (- scad-preview-window-size)
                                        scad-preview-window-position)
      (switch-to-buffer scad-preview--buffer))
    (add-hook 'kill-buffer-hook 'scad-preview--end nil t)
    (add-hook 'after-change-functions 'scad-preview--after-change-function t)
    (with-current-buffer scad-preview--buffer
      (add-hook 'kill-buffer-hook 'scad-preview--end nil t))
    (scad-preview-reset-camera-parameters)))

(defun scad-preview--end ()
  (when scad-preview-mode
    (setq scad-preview-mode nil)
    (when (timerp scad-preview--timer-object)
      (cancel-timer scad-preview--timer-object))
    (when (buffer-live-p scad-preview--buffer)
      (when (cdr (window-list))
        (mapc 'delete-window (get-buffer-window-list scad-preview--buffer)))
      (kill-buffer scad-preview--buffer))
    (when (buffer-live-p scad-preview--source-buffer)
      (with-current-buffer scad-preview--source-buffer
        (remove-hook 'kill-buffer-hook 'scad-preview--end t)
        (remove-hook 'after-change-functions 'scad-preview--after-change-function t)))
    (setq scad-preview--buffer        nil
          scad-preview--source-buffer nil
          scad-preview--timer-object  nil)))

(defun scad-preview--update ()
  (with-current-buffer scad-preview--source-buffer
    (let ((infile (make-temp-file "scad_" nil ".scad"))
          (outfile (concat temporary-file-directory (make-temp-name "scad_")  ".png")))
      (save-restriction
        (widen)
        (write-region (point-min) (point-max) infile nil 'nomsg))
      (save-window-excursion
        (condition-case nil
            (set-process-sentinel
             (start-process
              "scad process" nil scad-command
              "-o" outfile
              (concat "--imgsize="
                      (number-to-string (car scad-preview-image-size)) ","
                      (number-to-string (cdr scad-preview-image-size)))
              (concat "--camera="
                      (mapconcat 'number-to-string scad-preview--camera-parameters ","))
              (concat "--colorscheme=" scad-preview-colorscheme)
              infile)
             `(lambda (p _)
                (delete-file ,infile)
                (when (file-exists-p ,outfile)
                  (when (buffer-live-p scad-preview--buffer)
                    (with-current-buffer scad-preview--buffer
                      (fundamental-mode)
                      (erase-buffer)
                      (insert-file-contents ,outfile)
                      (scad-preview--image-mode)))
                  (delete-file ,outfile))))
          (error (progn (delete-file infile)
                        (scad-preview--end)
                        (message "SCAD: Failed to start OpenSCAD process."))))))))

;; + minor-mode for the preview buffer

(defvar scad-preview--image-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "r") 'scad-preview-reset-camera-parameters)
    (define-key keymap (kbd "<right>") 'scad-preview-rotz+)
    (define-key keymap (kbd "l") 'scad-preview-rotz+)
    (define-key keymap (kbd "f") 'scad-preview-rotz+)
    (define-key keymap (kbd "<left>") 'scad-preview-rotz-)
    (define-key keymap (kbd "h") 'scad-preview-rotz-)
    (define-key keymap (kbd "b") 'scad-preview-rotz-)
    (define-key keymap (kbd "<up>") 'scad-preview-dist-)
    (define-key keymap (kbd "k") 'scad-preview-dist-)
    (define-key keymap (kbd "p") 'scad-preview-dist-)
    (define-key keymap (kbd "<down>") 'scad-preview-dist+)
    (define-key keymap (kbd "n") 'scad-preview-dist+)
    (define-key keymap (kbd "j") 'scad-preview-dist+)
    (define-key keymap (kbd "C-<left>") 'scad-preview-roty+)
    (define-key keymap (kbd "C-h") 'scad-preview-roty+)
    (define-key keymap (kbd "C-b") 'scad-preview-roty+)
    (define-key keymap (kbd "C-<right>") 'scad-preview-roty-)
    (define-key keymap (kbd "C-l") 'scad-preview-roty-)
    (define-key keymap (kbd "C-f") 'scad-preview-roty-)
    (define-key keymap (kbd "C-<up>") 'scad-preview-rotx+)
    (define-key keymap (kbd "C-k") 'scad-preview-rotx+)
    (define-key keymap (kbd "C-p") 'scad-preview-rotx+)
    (define-key keymap (kbd "C-<down>") 'scad-preview-rotx-)
    (define-key keymap (kbd "C-n") 'scad-preview-rotx-)
    (define-key keymap (kbd "C-j") 'scad-preview-rotx-)
    (define-key keymap (kbd "M-<left>") 'scad-preview-trnsx+)
    (define-key keymap (kbd "M-h") 'scad-preview-trnsx+)
    (define-key keymap (kbd "M-b") 'scad-preview-trnsx+)
    (define-key keymap (kbd "M-<right>") 'scad-preview-trnsx-)
    (define-key keymap (kbd "M-l") 'scad-preview-trnsx-)
    (define-key keymap (kbd "M-f") 'scad-preview-trnsx-)
    (define-key keymap (kbd "M-<up>") 'scad-preview-trnsz-)
    (define-key keymap (kbd "M-k") 'scad-preview-trnsz-)
    (define-key keymap (kbd "M-p") 'scad-preview-trnsz-)
    (define-key keymap (kbd "M-<down>") 'scad-preview-trnsz+)
    (define-key keymap (kbd "M-n") 'scad-preview-trnsz+)
    (define-key keymap (kbd "M-j") 'scad-preview-trnsz+)
    keymap)
  "Keymap for SCAD preview buffers.")

(defun scad-preview-trnsx+ () (interactive) (scad-preview--increment-camera-parameter 0 10))
(defun scad-preview-trnsx- () (interactive) (scad-preview--increment-camera-parameter 0 -10))
(defun scad-preview-trnsz+ () (interactive) (scad-preview--increment-camera-parameter 2 10))
(defun scad-preview-trnsz- () (interactive) (scad-preview--increment-camera-parameter 2 -10))
(defun scad-preview-rotx+ () (interactive) (scad-preview--increment-camera-parameter 3 20))
(defun scad-preview-rotx- () (interactive) (scad-preview--increment-camera-parameter 3 -20))
(defun scad-preview-roty+ () (interactive) (scad-preview--increment-camera-parameter 4 20))
(defun scad-preview-roty- () (interactive) (scad-preview--increment-camera-parameter 4 -20))
(defun scad-preview-rotz+ () (interactive) (scad-preview--increment-camera-parameter 5 20))
(defun scad-preview-rotz- () (interactive) (scad-preview--increment-camera-parameter 5 -20))
(defun scad-preview-dist+ () (interactive) (scad-preview--increment-camera-parameter 6 100))
(defun scad-preview-dist- () (interactive) (scad-preview--increment-camera-parameter 6 -100))

(define-derived-mode scad-preview--image-mode fundamental-mode "SCADp"
  "Minor mode for SCAD preview buffers."
  ;; suppress messages (http://qiita.com/itiut@github/items/d917eafd6ab255629346)
  (let ((message-log-max nil))
    (with-temp-message (or (current-message) "")
      (image-mode)))
  (use-local-map scad-preview--image-mode-map))

;; + interface

(defun scad-preview-rotate ()
  (interactive)
  (message "Use arrow keys (+[CM]) to rotate image.")
  (set-temporary-overlay-map scad-preview--image-mode-map t))

(defun scad-preview-export ()
  (interactive)
  (let ((infile (make-temp-file "scad_" nil ".scad")))
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) infile nil 'nomsg))
    (condition-case nil
        (set-process-sentinel
         (start-process "scad render" (get-buffer-create " *scad-render*") scad-command
                        "-o" (expand-file-name (read-file-name "Export to: "))
                        infile)
         `(lambda (p _)
            (delete-file ,infile)
            (message "SCAD: Successfully exported.")))
      (error (progn (delete-file infile)
                    (message "SCAD: Failed to start OpenSCAD process."))))))

(defun scad-preview-mode ()
  "Preview SCAD models in real-time in Emacs."
  (interactive)
  (if scad-preview-mode (scad-preview--end) (scad-preview--start)))

;; + provide

(provide 'scad-preview)

;;; scad-preview.el ends here
