;;; loccur.el --- Perform an occur-like folding in current buffer

;; Copyright (C) 2009 Alexey Veretennikov
;;
;; Author: Alexey Veretennikov <alexey dot veretennikov at gmail dot com>
;; Created: 2009-09-08
;; Version: 1.1.1
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
;; 
;; (require 'loccur)
;; ;; defines shortcut for loccur of the current word
;; (define-key global-map [(control o)] 'loccur-current)
;; ;; defines shortcut for the interactive loccur command
;; (define-key global-map [(control meta o)] 'loccur)
;; ;; defines shortcut for the loccur of the previously found word
;; (define-key global-map [(control shift o)] 'loccur-previous-match)
;;
;;; Issues:
;; Using with smooth-scrolling.el sometimes
;; gives unexpected jumps in loccur mode
;;
;;; TODO:
;; 
;;; Change Log:
;;
;; 2010-03-07 (1.1.1)
;;    + Default value is taken from prompt instead of an edit area
;;    (thanks to Nathaniel Flath)
;;
;;
;; 2009-10-05 (1.1.0)
;;    + Added highlighting of the matched strings
;;    + Now inserts selected region to the prompt
;;    + Added defun for applying last found regexp(loccur-previous-match)
;;    + Added intangible property together with invisibility
;;
;; 2009-09-08 (1.0.0)
;;    Initial Release.
;;
;;; Code:

(eval-when-compile (require 'cl))

(defconst loccur-overlay-property-name 'loccur-custom-buffer-grep)

(defvar loccur-highlight-matching-regexp t
  "If set to a non-nil value, the part of the line matching the
regex is highlighted. Use loccur-toggle-highlight to modify its
value interactively.")


(defun loccur-toggle-highlight()
  "Toggles the highlighting of the part of the line matching the
regex given in the loccur buffer."
  (interactive)
  (if loccur-highlight-matching-regexp
      (setq loccur-highlight-matching-regexp nil)
    (setq loccur-highlight-matching-regexp t)))


(or (assq 'loccur-mode minor-mode-alist)
    (nconc minor-mode-alist
		   (list '(loccur-mode loccur-mode))))


(defvar loccur-mode nil) ;; name of the minor mode
(make-variable-buffer-local 'loccur-mode)


(defvar loccur-history nil
  "History of previously searched expressions for the prompt")
(make-variable-buffer-local 'loccur-history)

(defvar loccur-last-match nil
  "Last match found")
(make-variable-buffer-local 'loccur-last-match)



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


(defun loccur-previous-match ()
  "Call `loccur' for the previously found word."
  (interactive)
  (loccur loccur-last-match))


(defun loccur (regex)
  "Perform a simple grep in current buffer for the regular
expression REGEX

This command hides all lines from the current buffer except those
containing the regular expression REGEX. A second call of the function
unhides lines again"
  (interactive 
   (if loccur-mode
       (list nil)
     (list (read-string (concat "Regexp<" (loccur-prompt)
                                ">: ") "" 'loccur-history ))))
  (if (string-equal "" regex) (setq regex (loccur-prompt)))
  (loccur-mode regex))


(defun loccur-prompt ()
  "Returns the default value of the prompt.

Default value for prompt is a current word or active region(selection),
if its size is 1 line"
  (let ((prompt
         (if (and transient-mark-mode
                  mark-active)
             (let ((pos1 (region-beginning))
                   (pos2 (region-end)))
               ;; Check if the start and the of an active region is on
               ;; the same line
               (if (= (line-number-at-pos pos1)
                      (line-number-at-pos pos2))
                   (buffer-substring-no-properties pos1 pos2)))
           (current-word))))
    prompt))


(defun loccur-1 (regex)
  (let* ((buffer-matches (loccur-find-matches regex))
		 (ovl-bounds (loccur-create-overlay-bounds-btw-lines buffer-matches)))
	(setq loccur-overlay-list 
		  (loccur-create-invisible-overlays ovl-bounds))
    (setq loccur-overlay-list
          (append loccur-overlay-list
                  (loccur-create-highlighted-overlays buffer-matches)))
    (setq loccur-last-match regex)
	(recenter)))

(defun loccur-create-highlighted-overlays(buffer-matches)
  (let ((overlays 
		 (map 'list #'(lambda (match)
						(make-overlay
						 (nth 1 match)
						 (nth 2 match)
						 (current-buffer) t nil))
			  buffer-matches)))
    ;; !ME! To remove highlighting of the matching regexp
    (if loccur-highlight-matching-regexp
          (mapcar (lambda (ovl) 
                    (overlay-put ovl loccur-overlay-property-name t)
                    (overlay-put ovl 'face 'isearch))
                  overlays))))


(defun loccur-create-invisible-overlays (ovl-bounds)
  (let ((overlays 
		 (map 'list #'(lambda (bnd)
						(make-overlay
						 (car bnd)
						 (cadr bnd)
						 (current-buffer) t nil))
			  ovl-bounds)))
	(mapcar (lambda (ovl) 
              (overlay-put ovl loccur-overlay-property-name t)
              (overlay-put ovl 'invisible t)
              ;; force intangible property if invisible property
              ;; does not automatically set it
              (overlay-put ovl 'intangible t))
            overlays)))


(defun loccur-remove-overlays ()
  (remove-overlays (point-min) (point-max) loccur-overlay-property-name t)
  (setq loccur-overlay-list nil))


(defun loccur-create-overlay-bounds-btw-lines (buffer-matches)
  (let ((prev-end (point-min))
		(overlays (list)))
	(when buffer-matches
	  (mapcar (lambda (line)
                (let ((beginning (car line)))
                  (unless ( = (- beginning prev-end) 1)
                    (let ((ovl-start (if (= prev-end 1) 1 prev-end))
                          (ovl-end  (1- beginning)))
                      (push (list ovl-start ovl-end) overlays)))
                  (setq prev-end (nth 3 line))))
              buffer-matches)
	  (push (list (1+ prev-end) (point-max)) overlays)
	  (setq overlays (nreverse overlays)))))


(defun loccur-find-matches (regex)
  "Returns a list of 4-number tuples, specifying begnning of the line,
1st match begin of a line, 1st match end of a line, end of a line
containing match"
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
            (let ((found-begin (match-beginning 0))
                  (found-end (match-end 0)))
              ;; Get the start and the and of the matching line
              ;; and store it to the overlays array
              (goto-char found-begin)
              (setq endpoint (line-end-position))
              (push (list (line-beginning-position) found-begin found-end endpoint) lines)))
		  ;; maybe add some code to highlight matches like in occur-mode?
		  ;; goto the end of line for any case
		  (goto-char endpoint))
		(forward-line 1))
	  (setq lines (nreverse lines)))))


(provide 'loccur)
;;; loccur.el ends here
