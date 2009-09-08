;;; loccur.el --- Perform an occur-like folding in current buffer

;; Copyright (C) 2009 Alexey Veretennikov
;;
;; Author: Alexey Veretennikov <alexey dot veretennikov at gmail dot com>
;; Created: 2009-09-08
;; Version: 1.0
;; Keywords: matching
;; URL: http://loccur.sourceforge.net/
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Add the following to your .emacs file:
;; (require 'loccur)
;; (define-key global-map "\C-o" 'loccur-current)
;; (define-key global-map "\C-\M-o" 'loccur)
;;
;;; Issues:
;; Using with smooth-scrolling.el sometimes
;; gives unexpected jumps in loccur mode
;;
;;; TODO:
;; 1. Highlight matched strings
;; 
;;; Change Log:
;;
;; 2009-09-08 (1.0.0)
;;    Initial Release.
;;
;;; Code:

(eval-when-compile (require 'cl))

(defconst loccur-overlay-property-name 'loccur-custom-buffer-grep)


(or (assq 'loccur-mode minor-mode-alist)
    (nconc minor-mode-alist
		   (list '(loccur-mode loccur-mode))))


(defvar loccur-mode nil) ;; name of the minor mode
(make-variable-buffer-local 'loccur-mode)


(defvar loccur-history nil
  "History of previously searched expressions.")
(make-variable-buffer-local 'loccur-history)


(defvar loccur-overlay-list nil
  "A list of currently active overlays.")
(make-variable-buffer-local 'loccur-overlay-list)


(defun loccur-mode (regex)
  (setq	loccur-mode 
		(if (or loccur-mode
				(null regex)
				(zerop (length regex)))
			nil
		  " Loccur"))
  (force-mode-line-update)
  (loccur-remove-overlays)
  (when loccur-mode
	(loccur-1 regex)))


(defun loccur-current ()
  "Call `loccur' for the current word."
  (interactive)
  (loccur (current-word)))


(defun loccur (regex)
  "Perform a simple grep in current buffer for the regular
expression REGEX

This command hides all lines from the current buffer except those
containing the regular expression REGEX. A second call of the function
unhides lines again"
  (interactive 
   (if loccur-mode
	   (list nil)
	 (list (read-string "Regexp: " (current-word) 'loccur-history))))
  (loccur-mode regex))


(defun loccur-1 (regex)
  (let* ((buffer-lines (loccur-find-match-lines regex))
		 (ovl-bounds (loccur-create-overlay-bounds-btw-lines buffer-lines)))
	(setq loccur-overlay-list 
		  (loccur-create-overlays ovl-bounds))
	(recenter)))


(defun loccur-create-overlays (ovl-bounds)
  (let ((overlays 
		 (map 'list #'(lambda (bnd)
						(make-overlay
						 (first bnd)
						 (second bnd)
						 (current-buffer) t nil))
			  ovl-bounds)))
	(dolist (ovl overlays)
	  (overlay-put ovl loccur-overlay-property-name t)
	  (overlay-put ovl 'invisible t))
	overlays))


(defun loccur-remove-overlays ()
  (remove-overlays (point-min) (point-max) loccur-overlay-property-name t)
  (setq loccur-overlay-list nil))


(defun loccur-create-overlay-bounds-btw-lines (buffer-lines)
  (let ((prev-end (point-min))
		(overlays (list)))
	(when buffer-lines
	  (dolist (line buffer-lines)
		(let ((beginning (first line)))
		  (unless ( = (- beginning prev-end) 1)
			(let ((ovl-start (if (= prev-end 1) 1 prev-end))
				  (ovl-end  (1- beginning)))
			  (push (list ovl-start ovl-end) overlays)))
		  (setq prev-end (second line))))
	  (push (list (1+ prev-end) (point-max)) overlays)
	  (setq overlays (nreverse overlays)))))


(defun loccur-find-match-lines (regex)
  (save-excursion
	;; Go to the beginnig of buffer
	(goto-char (point-min))
	;; Set initial values for variables
	(let ((matches 0)
		  (curpoint nil)
		  (endpoint nil)
		  (lines (list)))
	  ;; Search loop
	  (while (not (eobp))
		(setq curpoint (point))
		;; if something found
		(when (setq endpoint (re-search-forward regex nil t))
		  (save-excursion
			;; Get the start and the and of the matching line
			;; and store it to the overlays array
			(goto-char (match-beginning 0))
			(setq endpoint (line-end-position))
			(push (list (line-beginning-position) endpoint) lines))
		  ;; maybe add some code to highlight matches like in occur-mode?
		  ;; goto the end of line for any case
		  (goto-char endpoint))
		(forward-line 1))
	  (setq lines (nreverse lines)))))

(provide 'loccur)
;;; loccur.el ends here