;;;; -*- Mode: Lisp -*-

(defpackage #:superparen
  (:use #:cl)
  (:export #:*superparen-readtable*))
(in-package #:superparen)

;;;; Special variables and constants

;;; Bind these specials around calls to the reader to use different
;;; parentheses. :)
(defparameter *open-paren-char* #\()
(defparameter *close-paren-char* #\))
(defparameter *open-bracket-char* #\[)
(defparameter *close-bracket-char* #\])

;;; Bind this to T to accept forms delimited by
;;; *CLOSE-BRACKET-CHAR* that do not begin with
;;; *OPEN-BRACKET-CHAR*.
(defparameter *allow-unmatched-bracket-p* nil)

(defvar *paren-depth* 0)
(defvar *bracket-depth* 0)
(defvar *nearest-enclosing-paren-type* nil) ; PAREN | BRACKET | NIL

(unless (boundp '+dot+)
  (defconstant +dot+ (gensym "THE-DOT-OBJECT")))

(defun dotp (thing) (eq thing +dot+))


;;;; Conditions

(define-condition bracket-reader-error (reader-error)
  ())

(define-condition match-error (bracket-reader-error)
  ((char :initarg :char :reader match-error-char)))

(define-condition unmatched-paren-error (match-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Unmatched parenthesis ~S on ~A"
	     (match-error-char condition)
	     (stream-error-stream condition)))))

(define-condition mismatched-paren-error (match-error)
  ((expected-char :initarg :expected-char
		  :reader mismatched-paren-error-expected-char))
  (:report
   (lambda (condition stream)
     (format stream "Mismatched parenthesis on ~A: got ~S, expected ~S"
	     (stream-error-stream condition)
	     (match-error-char condition)
	     (mismatched-paren-error-expected-char condition)))))

(define-condition dot-error (simple-condition bracket-reader-error) ())


;;;; Error signalling interfaces

(defun match-error (stream char &optional expected-char)
  (if expected-char
      (error 'mismatched-paren-error :stream stream
				     :char char
				     :expected-char expected-char)
      (error 'unmatched-paren-error :stream stream
				    :char char)))

(defun dot-error (stream kind)
  (let ((format-control
	  (ecase kind
	    (NOTHING-BEFORE
	     "Malformed dotted list on ~S: nothing appears before dot")
	    (NOTHING-AFTER
	     "Malformed dotted list on ~S: nothing appears after dot")
	    (TOO-MANY-AFTER
	     "Malformed dotted list on ~S: too many forms after dot")
	    (COMMA-SPLICING
	     "Malformed dotted list on ~S: comma-splicing after dot")
	    (OUTSIDE-LIST
	     "Dot outside list on ~S"))))
    (error 'dot-error
	   :stream stream
	   :format-control format-control
	   :format-arguments (list stream))))


;;;; Character macro functions

(declaim (inline peek-next-char read-next-char read-next-obj))
(defun peek-next-char (stream) (peek-char t stream t nil t))
(defun read-next-char (stream) (read-char stream t nil t))
(defun read-next-obj (stream) (read stream t nil t))

(defgeneric paren-mate (char)
  (:method (char) (error "~S is not a valid parenthesis character" char))
  (:method ((char (eql *open-bracket-char*))) *close-bracket-char*)
  (:method ((char (eql *open-paren-char*))) *close-paren-char*)
  (:method ((char (eql *close-bracket-char*))) *open-bracket-char*)
  (:method ((char (eql *close-paren-char*))) *open-paren-char*))

(defgeneric read-list (stream char))

(defmethod read-list :around (stream (char (eql *open-bracket-char*)))
  (let ((*nearest-enclosing-paren-type* 'bracket)
	(*bracket-depth* (1+ *bracket-depth*)))
    (call-next-method)))

(defmethod read-list :around (stream (char (eql *open-paren-char*)))
  (let ((*nearest-enclosing-paren-type* 'paren)
	(*paren-depth* (1+ *paren-depth*)))
    (call-next-method)))

(defmethod read-list (stream char)
  (when *read-suppress*
    (read-list/read-suppress stream char))
  (let ((accumulator (list)))
    (labels
	((collect (value)
	   (push value accumulator))
	 (succeed (&optional tail)
	   (return-from read-list (append (nreverse accumulator) tail)))
	 (unmatched-paren-handler (condition)
	   (when (unmatched-paren-false-alarm-p condition (paren-mate char))
	     (succeed))))
      (handler-bind ((unmatched-paren-error #'unmatched-paren-handler))
	(when (eql (peek-next-char stream) #\.)
	  (dot-error stream 'nothing-before))
	(loop
	  (dispatch-on-char stream char (peek-next-char stream) #'succeed)
	  (dispatch-on-obj stream
			   char
			   (read-next-obj stream)
			   #'collect
			   #'succeed))))))

(defun read-list/read-suppress (stream char)
  (inclusive-chomp-to-char stream
			   (paren-mate char)
			   (and (eql char *open-paren-char*)
				*close-bracket-char*))
  (values))

(defun inclusive-chomp-to-char (stream end-char &rest more-end-chars)
  (loop
    (let ((next-char (read-next-char stream)))
      (when (or (eql next-char end-char)
		(and more-end-chars
		     (some (lambda (char) (eql char next-char))
			   more-end-chars)))
	(return-from inclusive-chomp-to-char)))))

(defgeneric unmatched-paren-false-alarm-p (condition expected-char)
  (:method ((condition unmatched-paren-error)
	    (expected-char (eql *close-bracket-char*)))
    (eql (match-error-char condition) expected-char))
  (:method ((condition unmatched-paren-error)
	    (expected-char (eql *close-paren-char*)))
    (or (eql (match-error-char condition) expected-char)
	(and (eql (match-error-char condition) *close-bracket-char*)
	     (> *bracket-depth* 0)))))

(defgeneric dispatch-on-char (stream open-char dispatch-char succeed)
  (:method (stream open-char (dispatch-char (eql #\.)) succeed)
    (read-next-char stream)
    (funcall succeed (read-after-dot stream (paren-mate open-char))))
  ;; [ ... ]
  (:method (stream
	    (open-char (eql *open-bracket-char*))
	    (dispatch-char (eql *close-bracket-char*))
	    succeed)
    (read-next-char stream)
    (funcall succeed))
  ;; ( ... )
  (:method (stream
	    (open-char (eql *open-paren-char*))
	    (dispatch-char (eql *close-paren-char*))
	    succeed)
    (read-next-char stream)
    (funcall succeed))
  ;; ( ... ]
  (:method (stream
	    (open-char (eql *open-paren-char*))
	    (dispatch-char (eql *close-bracket-char*))
	    succeed)
    (if (or (> *bracket-depth* 0) *allow-unmatched-bracket-p*)
	;; Leave bracket in the stream - at least one more open paren
	(funcall succeed)
	(match-error stream
		     *close-bracket-char*
		     *close-paren-char*)))
  ;; Default case:
  (:method (stream open-char dispatch-char succeed)
    (return-from dispatch-on-char)))

(defgeneric dispatch-on-obj (stream open-char obj collect succeed)
  (:method (stream open-char (obj (eql +dot+)) collect succeed)
    ;; If a comment (; + newline, #+nil, #-t, etc.) precedes a dot in
    ;; a dotted list, we can't detec t the dot without explicitly
    ;; handling the comment. Instead, we READ along merrily but set
    ;; #\. in our readtable to a non-terminating macro char function
    ;; that returns a DOT object, which we handle here.
    #+DEBUG (format t"READ-OPEN-BRACKET: Detected reified DOT~%")
    (funcall succeed (read-after-dot stream (paren-mate open-char))))
  (:method (stream open-char obj collect succeed)
    (funcall collect obj)))


(defun read-after-dot (stream end-char)
  #+DEBUG (format t "~&READ-AFTER-DOT~%")
  (let ((*after-dot-p* t))		;READ-COMMA grovels this
    (declare (special *after-dot-p*))
    (handler-bind ((match-error
		     (lambda (c)
		       #+DEBUG
		       (format t "READ-AFTER-DOT: Looking for ~S, saw ~S~%"
			       end-char (match-error-char c))
		       (let ((char (match-error-char c)))
			 (if (eql char end-char)
			     (dot-error stream 'nothing-after)
			     (match-error stream char end-char))))))
      (%read-after-dot stream
		       end-char
		       (read-next-obj stream)
		       (peek-next-char stream)))))

(defgeneric %read-after-dot (stream end-char list-tail next-char)
  (:method (stream
	    (end-char (eql *close-paren-char*))
	    list-tail
	    (next-char (eql *close-paren-char*)))
    (read-next-char stream)
    list-tail)
  (:method (stream
	    (end-char (eql *close-paren-char*))
	    list-tail
	    (next-char (eql *close-bracket-char*)))
    (if (or (> *bracket-depth* 0)
	    *allow-unmatched-bracket-p*)
	list-tail
	(match-error stream *close-bracket-char* *close-paren-char*)))
  (:method (stream
	    (end-char (eql *close-bracket-char*))
	    list-tail
	    (next-char (eql *close-bracket-char*)))
    (read-next-char stream)
    list-tail)
  (:method (stream
	    (end-char (eql *close-bracket-char*))
	    list-tail
	    (next-char (eql *close-paren-char*)))
    (match-error stream next-char))
  (:method (stream end-char list-tail next-char)
    #+DEBUG (format t "%READ-AFTER-DOT: LIST-TAIL = ~S~%" list-tail)
    (let ((forms-after-dot 1))
      (loop
	(handler-bind ((unmatched-paren-error
			 (lambda (c) (declare (ignore c)) (return))))
	  (read-next-obj stream))
	(incf forms-after-dot))
      (if (> forms-after-dot 1)
	  (dot-error stream 'too-many-after)
	  list-tail))))

(defun read-close-bracket (stream char)
  (unless (or (and *allow-unmatched-bracket-p*
		   (> *paren-depth* 0))
	      *read-suppress*)
    (match-error stream char))
  (values))

(defun read-close-paren (stream char)
  (match-error stream char))

(defun read-dot (stream char)
  (declare (ignore char))
  (cond (*read-suppress* (values))
	;; inside some kind of paren?
	(*nearest-enclosing-paren-type* +dot+)
	(t (dot-error stream 'outside-list))))

(defun read-comma (stream char)
  (declare (special *after-dot-p*))
  (let ((default-comma-reader #'#.(get-macro-character #\,))
	(after-dot-p (and (boundp '*after-dot-p*) *after-dot-p*)))
    (cond (*read-suppress*
	   (return-from read-comma (values)))
	  (after-dot-p
	   (case (peek-next-char stream)
	     ((#\. #\@) (dot-error stream 'comma-splicing)))))
    (funcall default-comma-reader stream char)))

#+DEBUG
(defun break* (&optional stream &rest ignorables)
  (declare (ignore ignorables))
  (break "Within ~A: Paren depth: ~A, Bracket depth: ~A, Next char: ~S"
	 (or *nearest-enclosing-paren-type* "no parens")
	 *paren-depth*
	 *bracket-depth*
	 (if stream (peek-char nil stream nil :eof) "N/A"))
  (values))

(defparameter *superparen-readtable*
  (let ((*readtable* (copy-readtable nil)))
    ;; Symbols so we don't have to re-evaluate this defparameter form
    ;; every time one of these reader functions is changed.
    (set-macro-character #\( 'read-list)
    (set-macro-character #\[ 'read-list)
    (set-macro-character #\] 'read-close-bracket)
    (set-macro-character #\) 'read-close-paren)
    (set-macro-character #\. 'read-dot t)
    (set-macro-character #\, 'read-comma)
    #+DEBUG
    (set-macro-character #\$ #'break*)
    *readtable*))

#+DEBUG
(defun tests ()
  (let ((*readtable* *superparen-readtable*)
	(*package* (find-package "SUPERPAREN"))
	(tests-run 0)
	(tests-succeeded 0))
    (macrolet ((test (compare &rest args)
		 `(progn
		    (incf tests-run)
		    (with-simple-restart (continue "Proceed to next test")
		      (assert (,compare ,@args))
		      (incf tests-succeeded)))))
      (test equalp
	    (read-from-string "[]")
	    nil)
      (test equalp
	    (read-from-string "[(]")
	    '(()))
      (test equalp
	    (read-from-string "[a]")
	    '(a))
      (test equalp
	    (read-from-string "[a b]")
	    '(a b))
      (test equalp
	    (read-from-string "[a . b]")
	    '(a . b))
      (test equalp
	    (read-from-string "()")
	    nil)
      (test equalp
	    (read-from-string "(a)")
	    '(a))
      (test equalp
	    (read-from-string "(a . b)")
	    '(a . b))
      (test equalp
	    (READ-FROM-STRING "([])")
	    '(()))
      (test equalp
	    (read-from-string "([a])")
	    '((a)))
      (test equalp
	    (read-from-string "([a b])")
	    '((a b)))
      (test equalp
	    (read-from-string "([(a (b (c] [(d (e (f])")
	    '(((a (b (c)))) ((d (e (f))))))
      (test equalp
	    (read-from-string "(a . (b (c))) ")
	    '(a b (c)))
      (test equalp
	    (read-from-string "[a . (b (c] ")
	    '(a b (c)))
      (test equalp
	    (read-from-string "([] . [])")
	    '(nil))
      (test equalp
	    (read-from-string "[[] . []]")
	    '(nil))
      (test equalp
	    (read-from-string "[() . ()]")
	    '(nil))
      (test equalp
	    (read-from-string "(a b c . [d])")
	    '(a b c d))
      (test equalp
	    (read-from-string "[a b c . (d]")
	    '(a b c d))
      (test equalp
	    (read-from-string "[a b c . (d)]")
	    '(a b c d))
      (test equalp
	    (read-from-string "[a b c . (d)]")
	    '(a b c d))
      (test equalp
	    (read-from-string "(a . (b . (c)))")
	    '(a b c))
      (test equalp
	    (read-from-string "[a . (b . (c]")
	    '(a b c))
      (test equalp
	    (read-from-string "(a #|comment|# b)")
	    '(a b))
      (test equalp
	    (read-from-string "(a b #|comment|# )")
	    '(a b))
      (test equalp
	    (read-from-string "(a . #|comment|# b)")
	    '(a . b))
      (test equalp
	    (read-from-string "(a . b #|comment|# )")
	    '(a . b))
      (test equalp
	    (read-from-string "(a b #+nil x c)")
	    '(a b c))
      (test equalp
	    (read-from-string "(a b #+nil c)")
	    '(a b))
      (test typep
	    (nth-value 1 (ignore-errors
			  (read-from-string "(snork . #+nil foo)")))
	    'dot-error)
      (format t "~&TESTS: ~A/~A succeeded.~%" tests-succeeded tests-run))))
