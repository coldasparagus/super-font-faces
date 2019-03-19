;;
;; Copyright (C) 2012 Alexander G. Burchell
;; 
;; Permission to use, copy, modify, distribute, and sell this software
;; and its documentation for any purpose is hereby granted without
;; fee, provided that the above copyright notice appear in all copies
;; and that both that copyright notice and this permission notice
;; appear in supporting documentation.
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT.  IN NO EVENT SHALL ALEXANDER G. BURCHELL BE LIABLE
;; FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;; 
;; Except as contained in this notice, the name of Alexander
;; G. Burchell shall not be used in advertising or otherwise to
;; promote the sale, use or other dealings in this Software without
;; prior written authorization from Alexander G. Burchell.
;; 

;; Portions Copyright (C) 1992, 1993, 1994, 1995, 1996, 1998, 1999,
;;   2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
;;   2011, 2012 Free Software Foundation, Inc.

(eval-when (compile eval)
  (require 'cl))

;; If we have or don't have something provided by another package.
;; This is a strange way to do it however `require' doesn't allow the
;; non-specification of the FILENAME if NOERROR is desired, which
;; would be easier.
(defmacro if-available (var &rest forms)
  `(condition-case ()
       (progn
	 (cond ((listp ,var)
		(dolist (feature ,var)
		  (require feature)))
	       (t
		(require ,var)))
         (progn
           ,@forms
           t))                          ; OOOPS be sure to return `t'
                                        ; upon success for alias
                                        ; `if-available'
     (error nil)))


;;; Here begins 'too-small'/'too-large' finding:

(defun find-faces-by-predicate (predicate-fn)
  "Find faces in `(face-list)' which satistfy a predicate function."
  (cl-flet ((face-satisfies-p (face) (funcall predicate-fn face)))
    (delq nil
	  (loop for face in (face-list) collect
		(when (face-satisfies-p face) face)))))

(defun find-faces-not-with-font-family (family)
  (cl-flet ((face-other-font-p (face)
			    (let* ((face-family (face-attribute face :family)))
			      (not (equal face-family family)))))
    (find-faces-by-predicate #'face-other-font-p)))

(defun collect-faces-not-with-font-family (family)
 (loop for face in (find-faces-not-with-font-family "Anonymous Pro")
       if (not (eq (face-attribute face :family) 'unspecified))
       collect `(,face ,(face-attribute face :family))))


(defun find-faces-with-background (color)
  (cl-flet ((face-background-p (face)
			    (let* ((face-background
				    (face-attribute face :background)))
			      (equal face-background color))))
    (find-faces-by-predicate #'face-background-p)))

(defun find-faces-with-foreground (color)
  (cl-flet ((face-foreground-p (face)
			    (let* ((face-foreground
				    (face-attribute face :foreground)))
			      (equal face-foreground color))))
    (find-faces-by-predicate #'face-foreground-p)))

(defun find-faces-that-are-too-small (min-height)
  (cl-flet ((face-too-small-p (face)
			   (let ((current (face-attribute face :height)))
			     (if (and (numberp current) (integerp current))
			       (< current min-height)
				 nil))))
    (find-faces-by-predicate #'face-too-small-p)))

(defun find-faces-that-are-too-large (max-height)
  (cl-flet ((face-too-large-p (face)
			   (let ((current (face-attribute face :height)))
			     (if (and (numberp current) (integerp current))
			       (> current max-height)
				 nil))))
    (find-faces-by-predicate #'face-too-large-p)))

;;; Adapted from emacs-customize

(defun change-background (color)
  "Change the background color."
  (interactive (list (completing-read "Color: " (defined-colors))))
  (mapcar (lambda (f)
	    (when (not (facep f)) (make-face f))
	    (set-face-background f color)) 
	  '(default fringe border isearch-lazy-highlight-face
	     custom-set-face jde-java-font-lock-pre-face
	     dired-perm-write egg-log-HEAD
	     font-latex-function-name-face
	     font-latex-variable-name-face font-lock-paren-face
	     font-lock-comment-face lazy-highlight
	     minibuffer-depth-indicator
	     nxml-outline-active-indicator nxml-outline-ellipsis
	     nxml-outline-indicator show-paren-match
	     show-paren-mismatch
	     font-lock-special-comment-face
	     font-lock-comment-delimiter-face
	     font-lock-boring-punctuation-face
	     font-lock-doc-face
             font-lock-string-face
	     diredp-dir-priv))
  (set-background-color color)
  (set-face-background 'default color))

;;; OLD (begin)

;; see color themes for example sizes
(when nil
(loop for face in (find-faces-that-are-too-small 175)
      do (set-face-attribute face nil :height '175))
(loop for face in all-mode-line-faces
      do (set-face-attribute face nil :foreground "blue"))
(loop for face in all-mode-line-faces
      do (set-face-attribute face nil :background "white"))
) ; when nil

;;; OLD (end)

;;; On-demand font attribute changing

(defvar super-face-keymap (make-sparse-keymap))

;; XXX need to research a better unused (globally) prefix for key
;; commands (one that doesn't exit emacs if typed backwards!)
(define-key global-map [(control ?c) ?a] super-face-keymap)

(define-key super-face-keymap [?s] 'super-change-size)
(define-key super-face-keymap [?a] 'super-change-factor)
(define-key super-face-keymap [?d] 'super-decrease-size)
(define-key super-face-keymap [?i] 'super-increase-size)
(define-key super-face-keymap [?f] 'super-change-font)
(eval-when (load compile eval)
  (require-no-err 'helm-color)
  (if (featurep 'helm-color)
      (progn
        (define-key super-face-keymap [?c] 'super-change-foreground-helm)
        (define-key super-face-keymap [?b] 'super-change-background-helm))
    (define-key super-face-keymap [?c] 'super-change-foreground)
    (define-key super-face-keymap [?b] 'super-change-background)))
(define-key super-face-keymap [?w] 'super-change-weight)
(define-key super-face-keymap [?l] 'super-change-slant)
(define-key super-face-keymap [?u] 'super-toggle-underline)

;; customization variables

(defvar super-face-factor
  0.05
  "Factor relative to 1.0 to change face size with
  `super-decrease-size' and `super-increase-size'.")

;; supporting functions

(defun super-faces-at-point ()
 (let ((faces (or (get-char-property (point) 'face)
      '(default))))
    (if (not (listp faces))
	(list faces)
      faces)))

(defun super-first-face-at-point ()
  (cl-loop for face in (super-faces-at-point)
           until (not (equal face :inherit))
           finally return face))

(defun loop-do-for-faces (faces fn)
  (loop for face in faces
	do
	(funcall fn face)))  

(defun loop-do-for-all-faces (fn)
  (loop-do-for-faces (face-list) fn))

(defun embolden-all-faces ()
  "Make all faces bold unconditionally."
  (interactive)
  (cl-flet ((embolden-face (face)
                           (set-face-attribute face (selected-frame) :weight 'bold)))
   (loop-do-for-all-faces #'embolden-face)))

(defun super-loop-do-for-faces (fn)
  (loop-do-for-faces (super-faces-at-point) fn))

(defun super-do-for-face (fn)
  (let ((face (super-first-face-at-point)))
    (funcall fn face)))

;; XXX we hack this before I refactor `super-read-attribute' to not
;; depend on a particular face
(defun set-all-faces-to-font (font)
  "Reset the height of all faces to the given font."
  (interactive (super-read-attribute "ALL" :font))
      (cl-flet ((set-face-to-new-font (face)
                                      (set-face-attribute face nil :font font)))
        (loop-do-for-all-faces #'set-face-to-new-font)))

(defun scale-all-faces-to-default ()
  "Reset the height of all faces to the height of the face
`default'."
    (interactive)
    (let ((height (face-attribute 'default :height)))
      (cl-flet ((set-height-to-default (face)
                                       (set-face-attribute face nil :height height)))
        (loop-do-for-all-faces #'set-height-to-default))))
    
;;  From "elisp" info, "38.12.2 Face Attributes": `:height' The height
;; of the font.  In the simplest case, this is an integer in units of
;; 1/10 point.  The value can also be a floating point number or a
;; function, which specifies the height relative to an "underlying
;; face" (i.e., a face that has a lower priority in the list described
;; in "Displaying Faces").

;; HOWEVER, a font like "Anonymous Pro:size=22" has a `:height' of
;; 183.  I still don't quite get the correspondence.  It is a factor
;; of 8.318181818181818...

(defun scale-face (face factor)
  (let* ((attrib (face-attribute face :height))
         (current-height (if (equal attrib 'unspecified)
                             ;; Give it the `default' face's height if
                             ;; it has none
                             (face-attribute 'default :height)
                           ;; else of course its own
                           attrib))
         new-height)
    (cond
     ;; normal face height such as 115
     ((and (numberp current-height) (integerp current-height))
      (setq new-height (round (* current-height factor))))
     ;; a "scaling factor" for the face height, leave it
     ((floatp current-height)
      (setq new-height current-height))
     ((symbolp current-height)
      ;; Seems some heights are variables?
      ;;(message "What's wrong with face \"%s\"?" face)
      (setq new-height current-height))
     (t
      (error "Unknown problem with :height of face \"%s\"" face)))
    ;;(message "Setting face \"%s\" to height %s" face new-height)
    (set-face-attribute face nil :height new-height)))

(defun super-change-factor (factor)
  "Multiply the current height of the face by `factor' and set
the face to that new height."
  (interactive)
  (super-loop-do-for-faces
   (lambda (face)
     (scale-face face factor))))

(defun scale-all-faces (factor)
  "Multiply the current height of each face in (face-list) by
`factor' and set each face to that new height."
  (interactive "nFactor: ")
  (loop-do-for-all-faces
   (lambda (face)
     (scale-face face factor))))

(defun scale-faces (faces factor)
  "Multiply the current height of each face in `faces' by
`factor' and set each face to that new height."
  (loop-do-for-faces faces
   (lambda (face)
     (scale-face face factor))))

;; Interactively bound commands

(defun super-change-size ()
  "TODO"
  (interactive)
  (super-loop-do-for-faces
   ;; TODO
   (lambda (face)
       (message "%s" (format "%s" face)))))

(defun super-change-size-1 (arg fn)
  "Helper function for `super-increase-size' and `super-decrease-size'."
  (super-change-factor
   (funcall fn (if arg
		   (* super-face-factor arg)
		 super-face-factor))))

(defun super-decrease-size (arg)
  "Decrease the size of face(s) at point by `super-face-factor'."
  (interactive "p")
  (super-change-size-1 arg #'(lambda (n) (- 1.0 n))))

(defun super-increase-size (arg)
  "Increase the size of face(s) at point by `super-face-factor'."
  (interactive "p")
  (super-change-size-1 arg #'(lambda (n) (+ 1.0 n))))

(defun super-change-foreground-helm (arg)
  "Interactively change the foreground color of the face at point
using helm-colors (or read a face name when called with prefix
arg)."
  (interactive "P")
  (super-change-color-attribute-helm arg :foreground "foreground"))

(defun super-change-background-helm (arg)
  "Interactively change the background color of the face at point
using helm-colors (or read a face name when called with prefix
arg)."
  (interactive "P")
  (super-change-color-attribute-helm arg :background "background"))

(defun super-change-color-attribute-helm (arg attribute attribute-string)
  "Internal function."
  (let* ((face
          (if (not arg)
              (super-first-face-at-point)
            (make-face (intern (completing-read "Face: " (face-list))))))
         (color   (helm :sources '(helm-source-colors helm-source-customize-face)
                        :buffer "*helm colors*"
                        :prompt (format "Changing %s for `%s': "
                                        attribute-string
                                        face))))
    (if color
        (set-face-attribute (make-face face) (selected-frame) 
		            attribute color)
      (message "Not changed."))))

(defun super-change-foreground (color)
  "Interactively change the foreground color of the face at point."
  (interactive (super-read-attribute (super-first-face-at-point) :foreground))
  (super-change-color-attribute :foreground color))

(defun super-change-background (color)
  "Interactively change the background color of the face at point."
  (interactive (super-read-attribute (super-first-face-at-point) :background))
  (super-change-color-attribute :background color))

(defun super-change-color-attribute (attribute color)
  "Internal function."
  (set-face-attribute (super-first-face-at-point) (selected-frame) 
		      attribute (or color 'unspecified)))

(defun super-change-font (font)
  "Interactively change the font of the face at point."
  (interactive (super-read-attribute (super-first-face-at-point) :font))
  (set-face-attribute (super-first-face-at-point) (selected-frame) 
		      :font (or font 'unspecified)))

(defun super-change-weight (weight)
  "Interactively change the :weight attribute of the face at point.

Useful with console fonts."
  (interactive (super-read-attribute (super-first-face-at-point) :weight))
  (set-face-attribute (super-first-face-at-point) (selected-frame) 
		      :weight (or weight 'unspecified)))

(defun super-change-slant (slant)
  "Interactively change the :slant attribute of the face at point."
  (interactive (super-read-attribute (super-first-face-at-point) :slant))
  (set-face-attribute (super-first-face-at-point) (selected-frame) 
		      :slant (or (list slant) 'unspecified)))

;; XXX some faces don't toggle initially, see definition for
;; `set-face-underline-p'.
(defun super-toggle-underline ()
  "Toggle the underlining of the face at point."
  (interactive)
  (let* ((face (super-first-face-at-point))
	 (ul (face-attribute face :underline)))
    (set-face-underline-p face (not ul))))

;; From faces.el `read-face-and-attribute'
(defun super-read-attribute (face attribute)
  (cond ((eq attribute :font)
	 (let* ((prompt (format "Set font-related attributes of \"%s\"" face))
		(font (read-face-font face (selected-frame))))
	   (list font)))
	(t
	 (let* ((attribute-name (face-descriptive-attribute-name attribute))
		(prompt (format "Set %s of \"%s\"" attribute-name face))
		(new-value
		 (read-face-attribute face attribute (selected-frame))))
	   ;; XXX icicles adds text properties to color names because
	   ;; it shows the name of the new color read in that color,
	   ;; using properties
           (when (stringp new-value)
             (set-text-properties 0 (length new-value) nil new-value))
	   (list new-value)))))

;;; HERE IS CODE (quite old)

;; This code has been undeprecated
(defun mode-line-stuff ()
  (change-faces-height-by-factor all-mode-line-faces 1.05)
  (set-face-font 'mode-line-keyword-face "Georgia:size=20:weight=Normal:slant=Normal"))

;; This utility function works with some color-theme problems relating
;; to paren-match
(defun fix-paren ()
  (interactive)
  (let ((bg (cdr (assoc 'background-color
			(frame-parameters (selected-frame))))))
    (set-face-foreground 'show-paren-match "navy")
    (set-face-foreground 'show-paren-mismatch "red")
    (set-face-background 'show-paren-match bg)
    (set-face-background 'show-paren-mismatch bg)))

(defvar all-mode-line-faces '(ecb-mode-line-data-face
			      ecb-mode-line-prefix-face
			      ecb-mode-line-win-nr-face
			      mode-line
			      mode-line-buffer-id
			      mode-line-emphasis
			      mode-line-function-name-face
			      mode-line-highlight
			      mode-line-inactive
			      mode-line-keyword-face
			      org-mode-line-clock
			      org-mode-line-clock-overrun
			      web-vcs-mode-line
			      web-vcs-mode-line-inactive)
  "All the current known mode-line faces.")

(defvar all-font-lock-faces '(font-lock-boring-punctuation-face
			      font-lock-brace-face
			      font-lock-builtin-face
			      font-lock-character-face
			      font-lock-comment-delimiter-face
			      font-lock-comment-face
			      font-lock-constant-face
			      font-lock-doc-face
			      font-lock-doc-string-face
			      font-lock-function-call-face
			      font-lock-function-name-face
			      font-lock-keyword-face
			      font-lock-macro-face
			      font-lock-negation-char-face
			      font-lock-number-face
			      font-lock-other-type-face
			      font-lock-paren-face
			      font-lock-preprocessor-face
			      font-lock-punctuation-face
			      font-lock-reference-face
			      font-lock-regexp-grouping-backslash
			      font-lock-regexp-grouping-construct
			      font-lock-special-comment-face
			      font-lock-string-face
			      font-lock-type-face
			      font-lock-variable-name-face
			      font-lock-warning-face)
  "All the font-lock faces.")

(defvar all-nxml-faces
  '(nxml-attribute-colon
    nxml-attribute-local-name
    nxml-attribute-prefix
    nxml-attribute-value
    nxml-attribute-value-delimiter
    nxml-cdata-section-CDATA
    nxml-cdata-section-content
    nxml-cdata-section-delimiter
    nxml-char-ref-delimiter
    nxml-char-ref-number
    nxml-comment-content
    nxml-comment-delimiter
    nxml-delimited-data
    nxml-delimiter
    nxml-element-colon
    nxml-element-local-name
    nxml-element-prefix
    nxml-entity-ref-delimiter
    nxml-entity-ref-name
    nxml-glyph
    nxml-hash
    nxml-heading
    nxml-markup-declaration-delimiter
    nxml-name
    nxml-namespace-attribute-colon
    nxml-namespace-attribute-prefix
    nxml-namespace-attribute-value
    nxml-namespace-attribute-value-delimiter
    nxml-namespace-attribute-xmlns
    nxml-outline-active-indicator
    nxml-outline-ellipsis
    nxml-outline-indicator
    nxml-processing-instruction-content
    nxml-processing-instruction-delimiter
    nxml-processing-instruction-target
    nxml-prolog-keyword
    nxml-prolog-literal-content
    nxml-prolog-literal-delimiter
    nxml-ref
    nxml-tag-delimiter
    nxml-tag-slash
    nxml-text)
  "All the NXML faces.")

(defun resize-face (face font new-size)
  (condition-case ()
      (set-face-font face (format "%s:size=%s" font new-size))
    (error (set-face-font 
	    face (format "%s:size=%s" (face-attribute 'default :family) new-size)))))

(defun font-lock-stuff ()
  (resize-faces all-font-lock-faces +2 "Georgia"))

(provide 'super-font-faces)
