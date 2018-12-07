;;;; BASIC in common lisp
;;;;
;;;; There isn't a real good reason for this, except as an exercise in lisp.
;;;; The language is patterned after BASIC64 and HP2000 Time Shared BASIC.
;;;; The first full test case is the 1975 version of the HP TSB "Oregon Trail" game.
;;;; (First successful runs - 2011-12-26)
;;;;
;;;; Some of the more HP/TSB system-specific and arcane things are left out:
;;;;  - CTL
;;;;  - TYP/LOC
;;;;  - MAT anything
;;;;
;;;; TODO:
;;;;   - First, verify that parsing is done correctly for the cases implemented so far
;;;;    - array refs and substrings
;;;;   - test substring assignment and input
;;;;   - verify eval-expression produces the right results with the new grammatical elements
;;;;   - test print functions SPA TAB LIN
;;;;   - test computed goto/gosub
;;;;   - test INPUT
;;;;   - test ENTER
;;;; 
;;;;   That should be enough to run oregon1975.bas
;;;; 
;;;;   Then:
;;;; 
;;;;   - test MIN/MAX ops
;;;;   - test LINPUT
;;;;    - *invoke-debugger-hook*
;;;;   - test the rest of the standard functions
;;;;   - add repl-level PRINT (for debugging at STOP)
;;;;   - PRINT-USING/IMAGE
;;;;   - additional string functions (LEFT$, RIGHT$, etc.)
;;;;   - consider integer variables/constants.  Check MS, IBM and Apple for syntax.
;;;;   - CONVERT
;;;;   - files
;;;;   - matrices
;;;; 
;;;;  Later extensions to consider:
;;;;   - ON ERROR GOTO
;;;;   - multicharacter variable/user-defined function names
;;;;   - block structured IF
;;;;   - SELECT/CASE
;;;;   - WHILE/WEND
;;;;   - REPEAT/UNTIL
;;;;   - alternate REM syntax (' .* $)
;;;;   - OPTION
;;;;   - CALL/SUB/LOCAL
;;;;   - structures/records - possible syntax: TYPE <name> create: DIM <var> AS NEW <typename>

(in-package :lbasic)

(defparameter *trace-flag* 0)
(defmacro tracef (str &rest args)
  `(if (= *trace-flag* 1)
       (format t ,str ,@args)))

;;; This is the parsed program, used for editing and display
;;; It is of the form:
;;; '(( "raw text read from file or terminal" (lineno ( statements )))
;;;   ( "raw text read from file or terminal" (lineno ( statements )))
;;;  ...)
(defparameter *program-lines* nil)

;;; After "processing" (preparation for interpretation) program is represented as a list of these.
(defstruct basic-statement
  lineno				; an integer
  kind					; a token/keyword
  args					; list of statement components
  body)					; implementation - set by process-ast
(defparameter *program-statement-list* nil)
(defparameter *current-statement-position* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; All of the program variables are contained in a hash-table of these.
(defstruct basic-var
  name
  (type 'BASIC-TYPE-FLOAT)		; BASIC-TYPE-INTEGER BASIC-TYPE-STRING
  (value 0.0)
  initial-value
  limit					; if non-nil indicates this is a FOR-loop variable
  (step-function #'(lambda (n) (1+ n)))
  resume-statement			; this is where NEXT goes to
)
(defparameter *program-variables* nil)

(defun is-string-var-name (n)
  (eql #\$ (elt n (1- (length n)))))

;;; Yes, I know -- if this were a class, then I could use an :after method with initialize-instance().
(defun basic-var-default-value (vname)
  (cond ((is-string-var-name vname) "")
	;; if we ever do integers, put them here
	(T 0.0)))

(defun basic-type-of (value)
  (cond
    ((typep value 'INTEGER) 'BASIC-TYPE-INTEGER)
    ((typep value 'FLOAT) 'BASIC-TYPE-FLOAT)
    ((typep value 'STRING) 'BASIC-TYPE-STRING)
    ((typep value 'basic-var) (basic-var-type value))
    ((and (listp value) (eql 'VAR (car value)))
     (let ((v (var-ref (cadr value))))
       (if v
	   (basic-var-type v)
	   (error 'basic-error
		  :statement (car *current-statement-position*)
		  :text (format nil "Variable ~a is not defined" (cadr value))))))
    (T (error 'basic-error :statement (car *current-statement-position*) :text "Variable value has unknown type."))))

;;; XXX -- OK -- need to implement scopes.  BASIC uses dynamic binding.
;;; Right now, the only thing that requires scopes is the parameter to a DEF FNx(N).
(defparameter *scope-stack* nil)

(defun enter-scope ()
  (push (make-hash-table :test #'equal) *scope-stack* ))

(defun leave-scope ()
  (pop *scope-stack*))

;;; XXX -- possibly need indices as an optional param
(defun var-ref (vname &key (define-flag nil))
  (let (v)
    (dolist (scope *scope-stack*)
      (setf v (gethash vname scope))
      (when v
	(return-from var-ref v)))
    (when define-flag
      (setf v (var-set vname (basic-var-default-value vname))))
    v))

;;; The indicies here are for selecting the value to set, not define the variable.
(defun var-set (arg value &optional indices)
  (let ((vname (cond ((typep arg 'basic-var) (basic-var-name arg))
		     ((listp arg) (cadr arg))
		     (T arg))))
    ;;(format t "var-set ~S ~S ~s~%" vname value indices)
    (let ((v (var-ref vname)))
      (if v
	  (if indices
	      ;; use indices to chose a value in the array to set
	      ;; XXX - if arrays can ever contain anything other than numbers,
	      ;;       we will need a type-check here.
	      ;; XXX -- broken for multiple indices
	      (setf (aref (basic-var-value v) (floor indices)) value)
	      (setf (basic-var-value v) value))
	  (let ((vtype (basic-type-of value)))
	    ;;(format t "var-set vtype ~S~%" vtype)
	    (if indices
		;; XXX -- HP TSB allowed non-DIM'd arrays of length < 10.
		(error 'basic-error :statement
		       *current-statement-position*
		       :text "You must DIM an array variable before using it")
		(setf v (setf (gethash vname (car *scope-stack*))
			      (make-basic-var :name vname :type vtype :value value))))))
      ;;(format t "v is ~s and var-ref ~S is ~S~%" v vname (var-ref vname))
      v)))

;;; this is to support DIM ...
(defun define-basic-array (vname vtype vindices)
  (if (var-ref vname)
      (error 'basic-error
	     :statement (car *current-statement-position*)
	     :text (format nil "variable ~a has already been defined" vname))
      (let ((v (setf (gethash vname *program-variables*) ; regular variables are all global (for now)
		     (make-basic-var
		      :name vname
		      :type (if (eql 'STRING vtype) 'BASIC-TYPE-STRING 'BASIC-TYPE-FLOAT)
		      :value (make-array (if (listp vindices)
					     (mapcar #'1+ vindices)
					     (1+ vindices)) ; XXX -- OPTION BASE applies here
					 :adjustable nil
					 :element-type (if (eql 'STRING vtype) 'CHARACTER 'FLOAT))))))
	;;(format t "defined array with dimensions ~s -> ~s~%" vindices v)
	v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun do-sgn (x)
  (if (zerop x) x (float-sign x)))

(defun do-abs (n)
  (abs n))

(defun do-int (n)
  (floor n))

;;; From 2000C TSB BASIC:
;;;  "A sequence of random numbers generated by RND is repeatable if it
;;;   follows a call to RND with a given negative argument."
(defparameter *basic-random-state* (make-random-state))
(defun do-rnd (n)
  (when (< n 0)
    (setf *basic-random-state* (sb-ext:seed-random-state n)))
  (random 1.0 *basic-random-state*))

(defun do-sin (n)
  (sin n))

(defun do-cos (n)
  (cos n))

(defun do-tan (n)
  (tan n))

(defun do-atn (n)
  (atan n))

(defun do-exp (n)
  (exp n))

;;; only defined for argument > 0.0
(defun do-log (n)
  (log n))

;;; only defined for argument > 0.0
(defun do-sqr (n)
  (sqrt n))

(defun do-len (str)
  (length str))

(defun do-pos (haystack needle)
  (if (null haystack)
      1
      (let ((pos (search needle haystack)))
	(if pos pos 0))))

(defun do-num (str)
  (char-code (elt str 0)))

;;; see ~/lisp/int-char.lisp -- INT-CHAR was deprecated/removed between ClTl2 and ANSI
(defun do-chr$ (n)
  (make-string 1 :initial-element (code-char n)))

(defun do-ups$ (str)
  (map 'string #'char-upcase str))

;;;
;;; TIM(0) = current minutes (~ to 59)
;;; TIM(1) = current hour (0 to 23) 
;;; TIM(2) = current day (1 to 366)
;;; TIM(3) = current year (0 to 99)
(defun is-leap-year (year)
  (and (= 0 (mod year 4)) (not (= 0 (mod year 100))))) ; will work until 2500AD

(defun day-of-year (year month mday)
  (let ((days-per-month #(31 28 31 30 31 30 31 31 30 31 30 31))
	(days 0))
    (dotimes (i (1- month))
      (incf days (elt days-per-month i)))
    (+ days mday (if (and (> month 2) (is-leap-year year)) 1 0))))

(defun do-tim (n)
  (multiple-value-bind (secs mins hours mday month year) (decode-universal-time (get-universal-time))
    (declare (ignore secs))
    (case n
      (0 mins)
      (1 hours)
      (2 (day-of-year year month mday))
      (3 (- year 2000)))))

;;; XXX - there isn't anything that actually checks this value, yet...
(defparameter *break-requested* nil)
(defparameter *break-value* 1)
(defun do-brk (n)
  (cond ((< n 0) *break-value*)
	((= 0 n) (setf *break-value* 0))
	(T (setf *break-value* n))))

(defparameter *program-functions* nil)

(defstruct basic-fn
  name
  param-name				; non-nil if user-defined
  value-type
  body)

(defparameter *function-init-list*
  `(
    ("SGN" ,#'do-sgn)
    ("ABS" ,#'do-abs)
    ("INT" ,#'do-int)
    ("RND" ,#'do-rnd)
    ("SIN" ,#'do-sin)
    ("COS" ,#'do-cos)
    ("TAN" ,#'do-tan)
    ("ATN" ,#'do-atn)
    ("EXP" ,#'do-exp)
    ("LOG" ,#'do-log)
    ("SQR" ,#'do-sqr)
    ("LEN" ,#'do-len)
    ("POS" ,#'do-pos)
    ("NUM" ,#'do-num)
    ("CHR$" ,#'do-chr$)
    ("UPS$" ,#'do-ups$)
    ("TIM" ,#'do-tim)
    ("BRK" ,#'do-brk)

    ;; These are pretty system-specific (but useful)
    ;; XXX -- probably will handle them in the function table instead of keywords, anyway.
    ;;(KW-SYSTEM)
    ;;(KW-CHAIN)
    ;;(KW-COM)
    ))

(defun function-table-init ()
  (let ((hash (make-hash-table :test #'equal)))
    (dolist (fn *function-init-list*)
      (setf (gethash (car fn) hash)
	    (make-basic-fn :name (car fn) :body (cadr fn))))
    hash))

(defun basic-function-lookup (fname)
  (unless *program-functions* (setf *program-functions* (function-table-init)))
  (gethash fname *program-functions*))

(defun basic-function-define (fname argname fexp)
  (unless *program-functions* (setf *program-functions* (function-table-init)))
  (setf (gethash fname *program-functions*)
	(make-basic-fn :name fname :body fexp :param-name argname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; XXX -- This needs to be seriously rethought.
;;; I think this kind of thing might benefit from some CLOS.
;;; Need to add type-checks and/or automatic conversions (if they exist in BASIC).
(defun eval-expression (expr)
  ;;(format t "eval-expression: ~s~%" expr)
  (when expr
    (let ((result 
	   (cond ((atom expr) expr)
		 ;; XXX -- should change funcall use to (apply (car expr) (mapcar #'eval-expression (cdr expr)))
		 ((functionp (first expr)) (funcall (first expr) (eval-expression (second expr))))
		 (t (case (first expr)
		      (VAR
		       (let ((v (var-ref (second expr))))
			 (if v
			     (basic-var-value v)
			     (error 'basic-error
				    :statement (car *current-statement-position*)
				    :text (format nil "variable ~a is not defined.~%" (second expr))))))
		      ;; Either an array or sub-string reference, or a function call
		      ;; The subscripts are in a list, even if there is only one argument.
		      (TOK-SUBSCRIPTED-ID
		       (let ((v (var-ref (second expr)))
			     (fn (basic-function-lookup (second expr))))
			 ;;(format t "fname is ~s and fn is ~s~%" (second expr) fn)
			 (cond ((not (null v))
				;; OK -- it's a string sub-sequence or an array reference
				(if (typep (basic-var-value v) 'STRING)
				    ;; XXX -- HP TSB filled with spaces to the right
				    (subseq (basic-var-value v) (third expr))
				    ;; XXX -- this has to be re-done for multiple indices
				    (aref (basic-var-value v) (eval-expression (third expr)))))
			       ((not (null fn))
				;; OK -- it's a user-defined or built-in function call
				(if (basic-fn-param-name fn)
				    ;; bind/call of user-defined function
				    (let (val)
				      (enter-scope)
				      ;;(format t "binding function param ~a to ~s~%" (basic-fn-param-name fn) (eval-expression (third expr)))
				      (var-set (basic-fn-param-name fn) (eval-expression (third expr)))
				      (setf val (eval-expression (basic-fn-body fn)))
				      ;;(format t "function call evaluated to ~s~%" val)
				      (leave-scope)
				      val)
				    (eval-expression (list (basic-fn-body fn) (eval-expression (third expr))))))
			       (T (error 'basic-error
					 :statement (car *current-statement-position*)
					 :text (format nil "undefined function or variable ~a" (second expr)))))))

		      (TOK-PLUS     (+    (eval-expression (second expr)) (eval-expression (third expr))))
		      (TOK-MINUS    (if (null (third expr))
					(- 0  (eval-expression (second expr)))
					(-    (eval-expression (second expr)) (eval-expression (third expr)))))
		      (TOK-ASTERISK (*    (eval-expression (second expr)) (eval-expression (third expr))))
		      (TOK-SLASH    (/    (eval-expression (second expr)) (eval-expression (third expr))))
		      (TOK-UPARROW  (expt (eval-expression (second expr)) (eval-expression (third expr))))

		      (OP>      (if (>        (eval-expression (second expr)) (eval-expression (third expr))) 1.0 0.0))
		      (OP>=     (if (>=       (eval-expression (second expr)) (eval-expression (third expr))) 1.0 0.0))
		      (OP<      (if (<        (eval-expression (second expr)) (eval-expression (third expr))) 1.0 0.0))
		      (OP<=     (if (<=       (eval-expression (second expr)) (eval-expression (third expr))) 1.0 0.0))
		      (OP=      (if (equalp   (eval-expression (second expr)) (eval-expression (third expr))) 1.0 0.0))
		      (OP<>     (if (not (equalp (eval-expression (second expr)) (eval-expression (third expr)))) 1.0 0.0))
		      (TOK-HASH (if (not (equalp (eval-expression (second expr)) (eval-expression (third expr)))) 1.0 0.0))

		      (OP-MIN (min (eval-expression (second expr)) (eval-expression (third expr))))
		      (OP-MAX (max (eval-expression (second expr)) (eval-expression (third expr))))

		      (KW-AND
		       (labels ((eval-elist (el)
				  (if el
				      (if (not (= 0.0 (eval-expression (car el))))
					  (eval-elist (cdr el))
					  nil)
				      T)))
			 (if (eval-elist (cdr expr)) 1.0 0.0)))
		      (KW-OR
		       (labels ((eval-elist (el)
				  (if el
				      (if (not (= 0.0 (eval-expression (car el))))
					  T
					  (eval-elist (cdr el)))
				      nil)))
			 (if (eval-elist (cdr expr)) 1.0 0.0)))

		      (otherwise nil))))))
      ;;(format t "eval-expression: result ~s~%" result)
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Data for the BASIC DATA statements
(defparameter *program-data* nil)

;;; Restore from the given lineno, or from the beginning if lineno is nil.
(defun process-data-statements (lineno)
  (setf *program-data* nil)
  ;; This allows you to select a data-set based on an expression.
  (when lineno
    (setf lineno (eval-expression lineno)))
  (dolist (statement *program-statement-list*)
    (when (or (null lineno) (>= lineno (basic-statement-lineno statement)))
      (when (eql 'KW-DATA (basic-statement-kind statement))
	(dolist (datum (basic-statement-args statement))
	  (let ((cooked (eval-expression datum)))
	    (push (if (typep cooked 'STRING) cooked (coerce cooked 'FLOAT)) *program-data*))))))
  (setf *program-data* (nreverse *program-data*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *print-column* 0)
(defparameter *print-spacing* 1)

(defun print-element (statement arg-value)
  ;;(format t "PRINT-ELEMENT ~s~%" arg-value)
  (let ((str "")
	(value (eval-expression arg-value)))
    ;;(format t "PRINTING element ~s~%" value)

    ;; Numeric values are left justified in a field whose width is
    ;; determined by the magnitude of the number. The smallest field is six characters.
    (cond ((typep value 'INTEGER) (setf str (format nil "~6A" value)))
	  ((typep value 'FLOAT) (setf str (format nil "~6A" value)))
	  ((typep value 'STRING) (setf str (format nil "~A" value)))
	  (T (error 'basic-error :statement statement :text (format nil "PRINT OF UNKNOWN TYPE VALUE ~s" arg-value)) nil))
    ;;(format t "PRINTING ELEMENT ~s STR ~s~%" value str)

    (when (and (> *print-spacing* 1) (> *print-column* 0))
      (let ((spaces (- *print-spacing* (mod *print-column* *print-spacing*))))
	(when (> spaces 0)
	  (format t "~va" spaces "")
	  (incf *print-column* spaces))))
    (princ str)
    (incf *print-column* (length str))))

(defun do-print (statements)
  (let* ((statement (car statements))
	 (printline (basic-statement-args statement)))
    ;;(format t "PRINT ~s~%" printline)
    (if (null printline)
	(progn
	  (terpri)
	  (setf *print-column* 0))
	(let (last-element)
	  (dolist (element printline)
	    ;;(format t "PRINTING ~s~%" element)
	    ;; An element is either a semicolon, a comma, or a list
	    (cond 
	      ;; From the BASIC docs:
	      ;;  The output line is divided into five consecutive fields:
	      ;;  four fields of 15 characters each and one field of 12 characters, for a total of 72 characters.
	      ;;  When a comma separates items, each item is printed starting at the beginning of a field.
	      ;;  When a semicolon separates items, each item is printed immediately following the preceding item.
	      ;;  In either case, if there is not enough room left in the line to print the entire item,
	      ;;  printing of the item begins on the next line.
	      ((eql element 'TOK-SEMICOLON)
	       (setf *print-spacing* 1))

	      ((eql element 'TOK-COMMA)
	       (if (member last-element '(KW-SPA KW-TAB))
		   (setf *print-spacing* 1)
		   (setf *print-spacing* 15)))

	      ((listp element)
	       (cond
		 ;; This is an approximation.
		 ;; XXX -- this isn't quite right.  The docs say:
		 ;;   The numeric expression is evaluated and rounded to an integer.
		 ;;   If the value is positive, the value of the expression specifies
		 ;;   the number of line feeds to be generated.
		 ;;   If the value is negative, the absolute value of the numeric expression
		 ;;   will be used to determine the number of line feeds and the initial
		 ;;   carriage return is suppressed.
		 ((eql (car element) 'KW-LIN)
		  (loop repeat (eval-expression (cadr element)) do (format t "~%")))

		 ;; XXX - COMMA following SPA is treated like SEMICOLON.  Need a test for this.
		 ((eql (car element) 'KW-SPA)
		  (loop repeat (eval-expression (cadr element)) do (format t " ")))

		 ;; XXX - more rules.
		 ;;   The numeric expression is evaluated and rounded to an integer
		 ;;   an integer to obtain the destination column. Print columns are numbered 0 to 71.
		 ;;   If the numeric expression evaluates to a column position lower than the current
		 ;;   print position, the TAB function is ignored.
		 ;;   If the numeric expression evaluates to more than 71, the print position is moved
		 ;;   to the beginning of the next line 
		 ;;   Commas following TAB functions in print statements are treated as semicolons. XXX
		 ((eql (car element) 'KW-TAB)
		  (loop for i from *print-column* to (eval-expression (cadr element))
		     do (format t " ") (incf *print-column*)))

		 (T			; must be an expression
		  (print-element statement element))))
	      (t (print-element statement element)))

	    (setf last-element element))
	  (when (not (member last-element '(TOK-SEMICOLON TOK-COMMA)))
	    (format t "~%")
	    (setf *print-column* 0)))))
  nil)

(defun is-white-space (c)
  (find c #( #\space #\cr #\nl #\tab #\ff )))

(defun split-input-args (line)
  ;; Minimal BASIC doesn't support embedded escaped #\", but I implemented it anyway.
  ;; If you didn't need to support that, then something like the following might suffice:
  ;;     (mapcar #'(lambda (s) (string-trim " " s)) (split-sequence #\, line))
  (let (args current-arg in-string in-escape)
    (when (> (length line) 0)
      (dotimes (i (length line))
	(let ((c (elt line i)))
	  (cond ((char= c #\")
		 (if in-escape
		     (setf current-arg (concatenate 'string current-arg "\""))
		     (setf in-string (not in-string)))
		 (setf in-escape nil))
		((char= c #\,)
		 (if in-string
		     (setf current-arg (concatenate 'string current-arg ","))
		     (progn
		       (push current-arg args)
		       (setf current-arg nil)))
		 (setf in-escape nil))
		;; next arg
		((char= c #\\)
		 (if in-escape
		     (setf current-arg (concatenate 'string current-arg "\\")))
		 (setf in-escape (not in-escape)))
		(T 
		 ;; accumulate this character into the current arg
		 (if (or (not (is-white-space c)) in-string)
		     (setf current-arg (concatenate 'string current-arg (make-string 1 :initial-element c))))
		 (setf in-escape nil)
		 ))))
      (push current-arg args))
    (nreverse args)))

(defun safely-read-from-string (str &rest read-from-string-args)
  "Read an expression from the string STR, with *READ-EVAL* set
to NIL. Any unsafe expressions will be replaced by NIL in the
resulting S-Expression."
  (let ((*read-eval* nil))
    (ignore-errors
      (apply 'read-from-string str read-from-string-args))))

;;; Another case where CLOS might help.
(defun convert-input-value (varname value)
  ;;(format t "converting ~s for variable ~s~%" value varname)
  (let ((v (var-ref varname :define-flag t)))
    (case (basic-var-type v)
      (BASIC-TYPE-FLOAT
       (let ((numeric-value (safely-read-from-string value)))
	 (let ((float-value
		(cond ((typep numeric-value 'FLOAT) numeric-value)
		      ((typep numeric-value 'INTEGER) (coerce numeric-value 'FLOAT))
		      (T (error 'basic-error
				:statement (car *current-statement-position*)
				:text (format nil "~a is not a valid floating-point number!~%" value))))))
	   (setf (basic-var-value v) float-value))))
      (BASIC-TYPE-STRING
       (setf (basic-var-value v) value))
      (BASIC-TYPE-INTEGER
       (setf (basic-var-value v)
	     (handler-case (parse-integer value)
	       (sb-int:simple-parse-error (c) (format t "~a~%" c) nil)
	       (type-error (c) (format t "~a~%" c) nil))))
      (otherwise (error 'basic-error
			:statement (car *current-statement-position*)
			:text (format nil "Unknown type in assignment on input value ~a~%" varname))))))

(defun do-input (statements)
  (let ((stmt-args (basic-statement-args (car statements))))

    ;; Later BASICs allow: INPUT "Ask a question";N
    (when (eql 'INCLUDES-STRING (car stmt-args))
      (princ (cadr stmt-args))
      (setf stmt-args (cddr stmt-args)))

    (princ "? ")
    (finish-output *standard-output*)
    (let ((line (read-line *standard-input*)))
      ;; Special-case a single string variable
      ;; XXX -- may need to trim the final newline -- check the docs
      (if (and (= 1 (length stmt-args)) (is-string-var-name (car stmt-args)))
	  (let ((v (var-ref (car stmt-args) :define-flag t)))
	    (setf (basic-var-value v) line))

	  ;; Not a single string value
	  (let ((input-args (split-input-args line)))
	    (if (not (= (length input-args) (length stmt-args)))
		(error 'basic-error :statement (car statements) :text "Wrong number of input values")
		(dotimes (i (length input-args))
		  ;;(format t "Processing input var ~s with value ~s~%" (nth i stmt-args) (nth i input-args))
		  (convert-input-value (nth i stmt-args) (nth i input-args))))))))
  nil)

(defun do-linput (statements)
  (let* ((stmt-args (basic-statement-args (car statements)))
	 (vname (first stmt-args)))
    (if (is-string-var-name vname)
	(let ((v (var-ref vname)))
	  (if v
	      (let ((line (read-line *standard-input*)))
		(var-set vname line))
	      (error 'basic-error :statement (car statements) :text "You must DIMension a string variable before use")))))
  nil)

;;; This takes three inputs:
;;;  timeout limit value in seconds
;;;  variable that is set indicating the number of seconds the user took to respond, or -256 for timeout
;;;  variable that is set to the value that they input
(defun do-enter (statements)
  (let* ((statement (car statements))
	 (stmt-args (basic-statement-args statement))
	 (limit-varname (first stmt-args))
	 (elapsed-varname (second stmt-args))
	 (value-varname (third stmt-args)))
    ;;(format t "limit-varname ~s timeout-varname ~s value-varname ~s~%" limit-varname elapsed-varname value-varname)
    (let ((limit-var (var-ref limit-varname))
	  (elapsed-time-var (var-ref elapsed-varname :define-flag t))
	  (value-var (var-ref value-varname :define-flag t)))
      (if (null limit-var)
	  (error 'basic-error :statement statement :text "Time-limit variable not defined")
	  (let ((limit (basic-var-value limit-var)))
	    ;; Check the parameters
	    (cond ((and (is-string-var-name value-varname) (null value-var))
		   (error 'basic-error :statement statement :text "Input string variable not DIMensioned"))
		  ((is-string-var-name limit-varname)
		   (error 'basic-error :statement statement :text "Time limit variable given as string"))
		  ((is-string-var-name elapsed-varname)
		   (error 'basic-error :statement statement :text "Elapsed-time variable given as string"))
		  ((or (> 0 limit) (> limit 255))
		   (error 'basic-error :statement statement :text (format nil "limit ~a is out of range" limit)))
		  (t 
		   ;; Prompt with timeout.  Str is nil on timeout.
		   (let* ((orig-time (sb-ext:get-time-of-day))
			  (str (handler-case
				   (sb-ext:with-timeout limit (read-line *standard-input*))
				 (sb-ext:timeout (c) (declare (ignore c)) nil)))
			  (elapsed-time-in-seconds (- (sb-ext:get-time-of-day) orig-time)))
		     (if str
			 (progn (convert-input-value value-varname str)
				(setf (basic-var-value elapsed-time-var) elapsed-time-in-seconds))
			 (setf (basic-var-value elapsed-time-var) -256)))))))))
  nil)

(defun do-read (statements)
  (let* ((statement (car statements))
	 (varlist (basic-statement-args statement)))
    (dolist (var varlist nil)
      (if (null *program-data*)
	  (error 'basic-error :statement statement :text (format nil  "~%ERROR on line ~D: out of data~%" (basic-statement-lineno statement)))
	  (var-set var (pop *program-data*)))))
  nil)

(defun do-let (statements)
  (let ((arglist (basic-statement-args (car statements))))
    ;;(format t "LET: ~s~%" stmt-args)
    (let ((value (eval-expression (car arglist))))
      (dolist (varname (cdr arglist))
	;;(format t "LET ~S = ~S~%" varname value)
	(if (listp varname)
	    ;; XXX - this is a hack.  It won't work for multiple dimensions.
	    (var-set (second varname) value (eval-expression (third varname)))
	    (var-set varname value)))))
  nil)

(defun do-def (statements)
  (let ((args (basic-statement-args (car statements))))
    (basic-function-define (first args) (second args) (third args)))
  nil)

(defun do-dim (statements)
  (let ((args (basic-statement-args (car statements))))
    (dolist (dim-arg args)
      (let ((vname (second dim-arg))
	    (indices (third dim-arg)))
	(define-basic-array vname (if (is-string-var-name vname) 'STRING 'ARRAY) indices))))
  nil)

;;; This has to return the cons that points to the statement, actually.
;;; That allows you to continue execution from there (and after there).
(defun statement-at-lineno (lineno)
  (labels ((test-line (slist)
	     (if (null slist) nil
		 (let ((l (basic-statement-lineno (car slist))))
		   (when (and l (= lineno l)) (return-from statement-at-lineno slist))
		   (test-line (cdr slist))))))
    (test-line *program-statement-list*)))

(defun do-if (statements)
  (let ((args (basic-statement-args (car statements))))
    (if (= 1.0 (eval-expression (car args)))
	(let ((target-lineno (eval-expression (cadr args))))
	  ;;(format t "IF: stmt-args is ~s target lineno is ~s~%" stmt-args target-lineno)
	  (let ((s (statement-at-lineno target-lineno)))
	    ;;(format t "~d: TRANSFER TO LINE ~d~%" (basic-statement-lineno (car statements)) target-lineno)
	    (if s (return-from do-if s)			; next statement to execute
		(error 'basic-error
		       :statement (car statements)
		       :text (format nil "missing line ~d" target-lineno)))))))
  nil)				; fall through

;;; Either:
;;;   GOTO/GOSUB <expression> <lineno-list>
;;;   GOTO/GOSUB <lineno>
(defun do-goto (statements)
  (let* ((args (basic-statement-args (car statements)))
	 (value (round (eval-expression (car args))))
	 (linenos (cadr args)))
    ;; (if linenos
    ;;     (format t "~d: ~a ~s OF ~s~%" (basic-statement-lineno (car statements)) (basic-statement-kind (car statements)) value linenos)
    ;;     (format t "~d: ~a ~s~%" (basic-statement-lineno (car statements)) (basic-statement-kind (car statements)) value))
    (if linenos
	(if (and (> value 0) (<= value (length linenos)))
	    (statement-at-lineno (elt linenos (1- value))) ; nil on out-of-bounds
	    (error 'basic-error :statement (car statements)
		   :text (format nil "selector out of range at line ~d~%" (basic-statement-lineno (car statements)))))
	(statement-at-lineno value))))

;;; GOSUB is handled using a stack of return-locations.
;;; The "standard" (at least for 1964) didn't allow nested GOSUB,
;;; but it is easy to implement, so here it is.
(defparameter *gosub-stack* nil)

;;; Push the return position on the gosub stack.
(defun do-gosub (statements)
  (push (cdr statements) *gosub-stack*)
  (do-goto statements))

(defun do-return (statements)
  ;; (if *gosub-stack* (format t "~d: RETURN to line ~d~%" (basic-statement-lineno (car statements))
  ;; 			    (basic-statement-lineno (car *gosub-stack*))))
  (if *gosub-stack*
      (pop *gosub-stack*)
      (error 'basic-error :statement (car statements)
	     :text (format nil "extra RETURN on line ~a~%" (basic-statement-lineno (car statements))))))

(defun do-rem (statements)
  (declare (ignore statements))
  nil)

(defun do-data (statements)
  (declare (ignore statements))
  nil)

(defun do-restore (statements)
  (process-data-statements (cadr (basic-statement-args (car statements))))
  nil)

(defun do-for (statements)
  (let ((args (basic-statement-args (car statements))))
    ;; XXX need check that types of lb, ub and var are same
    (let ((varform (first args))
	  (lower-bound (eval-expression (second args)))
	  (upper-bound (eval-expression (third args)))
	  (stepform (eval-expression (fourth args))))
      ;;(format t "FOR: var is ~s lb ~s ub ~s step ~s~%" varform lower-bound upper-bound stepform)
      ;; XXX -- need a check here that the initial state is possible;
      ;;   e.g. FOR I=1 TO 0 STEP 2 should probably just skip past the NEXT. Check the standard.
      (let ((v (var-set varform lower-bound)))
	(setf (basic-var-initial-value v) lower-bound)
	(setf (basic-var-limit v) upper-bound)
	(let ((inc-value (if stepform stepform 1)))
	  (setf (basic-var-step-function v)
		#'(lambda (n) (+ n inc-value))))
	(setf (basic-var-resume-statement v) (cdr statements)))))
  nil)

(defun do-next (statements)
  (let* ((args (basic-statement-args (car statements)))
	 (v (var-ref (car args)))
	 (old-value (basic-var-value v)))
    (var-set v (funcall (basic-var-step-function v) old-value))
    (let ((iv (basic-var-initial-value v))
	  (lim (basic-var-limit v))
	  (val (basic-var-value v)))
      (declare (ignorable iv))
      ;;(format t "NEXT: iv ~s lim ~s val ~s~%" iv lim val)
      (let ((s
	     (cond ((> val old-value)	; step function is increasing
		    (when (<= val lim)
		      (basic-var-resume-statement v)))
		   (T
		    (when (>= val lim)
		      (basic-var-resume-statement v))))))
	;; (format t "~d: NEXT ~a TO LINE ~d~%" (basic-statement-lineno (car statements))
	;; 	  (car args) (if s (basic-statement-lineno (car s)) -1))
	s))))

(defun do-proc-call (statements)
  (let ((args (basic-statement-args (car statements))))
    (let ((fn (basic-function-lookup (first args))))
      (if fn
	  (if (basic-fn-param-name fn)
	      (error 'basic-error :statement (car statements) :text "XXX implement bind/call of user-defined function here")
	      (funcall (basic-fn-body fn) (eval-expression (cdr args))))
	  (error 'basic-error :statement (car statements) :text (format nil "undefined function or variable ~a" (first args))))))
  nil)

(defun do-stop (statements)
  (format t "PROGRAM STOPPED AT LINE ~a~%" (basic-statement-lineno (car statements)))
  T)

(defun do-end (statements)
  (declare (ignore statements))
  T)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sort-lines (lines)
  (let ((v (make-array 1 :fill-pointer 0 :adjustable t)))
    (loop for f in lines
       do (vector-push-extend f v))
    (setf v (sort v #'< :key #'caadr))
    (loop for i upto (1- (length v)) collect (elt v i))))

;;; Parse a range of integers.
;;; Returns two values, indicating the extrema.
;;; If the lower bound is unspecified, nil is returned.
;;; If the upper bound is unspecified, nil is returned.
;;; If the upper bound is open, 0 is returned.
;;; If both bounds are specified, they are returned (even if they are the same).
(defun parse-numeric-range (str)
  ;; ugh -- special cases
  (if (or (equal "-" str) (= (length str) 0))
      (values nil nil)
      (let ((lvalue 0) rvalue)
	(loop for i to (1- (length str))
	   do (let ((c (elt str i)))
		(cond ((char= #\- c)
		       (if (= i 0)
			   (setf lvalue nil))
		       (setf rvalue 0))
		      ((digit-char-p c)
		       (let ((cnum (digit-char-p c)))
			 (if rvalue
			     (setf rvalue (+ (* 10 rvalue) cnum))
			     (setf lvalue (+ (* 10 lvalue) cnum)))))
		      (t (return-from parse-numeric-range (values nil nil))))))
	(values lvalue rvalue))))

;;; COMMANDS
;;;
;;; TSB:
;;; =======
;;; LIST
;;; SAVE
;;; UNSAVE
;;; CATALOG
;;; GET
;;; NEW
;;; OLD
;;; SCRATCH
;;; PURGE
;;; DELETE
;;; RENAME
;;;
;;; GW-BASIC:
;;; =========
;;; AUTO - enables auto-lineno mode when entering a program.  Ctrl-C exits this mode.
;;; BLOAD, BSAVE - load/save binary at an address
;;; CHDIR, MKDIR, RMDIR
;;; CLEAR, PCOPY, SCREEN - clears BASIC environment, sets memory/stack limits
;;; CONT - continue after Ctrl-C or STOP or END.  If break during INPUT, INPUT is restarted (with prompt).
;;; DELETE [lineno] - [lineno]  (20-40 or -40 or 40-)
;;; EDIT <lineno>
;;; FILES [pathname] - get a directory listing
;;; KILL filename - delete a file
;;; LIST/LLIST [lineno]-[lineno][,filename] - line#'s are as with DELETE.
           ;; List program (optionally to file (LIST) or printer (LLIST)
;;; LOAD [filename[,r]] - Seems to imply NEW, unless ,r is specified.
;;;      if ,r is specified, pragram is run after it is loaded and now NEW is performed.
;;; MERGE filename - adds ASCII file containing program to current program, replacing overlapping lines.
;;; NAME oldfname AS newfname - file rename.  Destination file must not already exist.
;;; NEW - remove the current program from memory (doesn't affect disk).  Doesn't appear to have an "Are you sure?" if modified.
;;; RENUM [starting-from],[begin-at-this-line-in-current-program][,increment] - default is 10,,10
;;; RESET (similar to fcloseall(); system("sync"))
;;; RUN <lineno|filename>[,r] - execution begins at greatest line <= given one.  ,r is as with LOAD.
;;; SAVE filename [,a] [,p] - save file.  ,a means "in ASCII". ,p disables EDIT/LIST. If no extension, ".BAS" is appended.
;;; SHELL [string]
;;; SYSTEM (apparent the same as SHELL with no arguments)

(defun program-reset ()
  (setf *program-variables* (make-hash-table :test #'equal))
  (setf *scope-stack* (list *program-variables*)) ; establish global scope
  (setf *gosub-stack* nil)
  (setf *program-statement-list* nil)
  (setf *program-functions* nil)
  (setf *current-statement-position* nil))

(defun do-cmd-new (args)
  (declare (ignore args))
  (program-reset)
  (setf *program-lines* nil)
  t)

(defun basic-statement-function (kind)
  (case kind
    (KW-END #'do-end)
    (KW-STOP #'do-stop)
    (KW-REM #'do-rem)
    (KW-DATA #'do-data)
    (KW-RESTORE #'do-restore)
    (KW-READ #'do-read)
    (KW-PRINT #'do-print)
    (KW-LET #'do-let)
    (KW-DEF #'do-def)
    (KW-DIM #'do-dim)

    ;; XXX -- How would we ever get here?  Are there any functions with side-effects?  POKE()?
    ;; How about BRK(n)?  or RND(-1234)?
    (TOK-SUBSCRIPTED-ID #'do-proc-call)
    (KW-INPUT #'do-input)
    (KW-LINPUT #'do-linput)
    (KW-ENTER #'do-enter)

    ;; For the next group, the value of the form determines the control flow
    (KW-IF #'do-if)
    (KW-GOTO #'do-goto)
    (KW-GOSUB #'do-gosub)
    (KW-RETURN #'do-return)
    (KW-FOR #'do-for)			; actually, always returns nil
    (KW-NEXT #'do-next)

    ;; Formatted I/O
    ;;(KW-CONVERT) ; like sprintf
    ;;(KW-PRINTUSING)
    ;;(KW-IMAGE)

    ;;Not sure how far I'll go with files.  The full glory looks tedious.
    ;;(KW-OPEN)
    ;;(KW-CLOSE)
    ;;(KW-FILES)
    ;;(KW-ASSIGN)
    ;;(KW-ADVANCE)
    ;;(KW-UPDATE)
    ;;(KW-CREATE)
    ;;(KW-LOCK)
    ;;(KW-UNLOCK)
    ;;(KW-PRINT#)
    ;;(KW-INPUT#)

    ;; I don't think these are worth the trouble
    ;;(KW-MAT)	; assignment, +, *, INPUT, READ, PRINT, PRINT USING, ...
    ;;(KW-ZER)
    ;;(KW-CON)
    ;;(KW-IDN)
    ;;(KW-TRN)
    ;;(KW-INV)

    (otherwise nil)))

;;; Execute the first statement in the given list of statements.
;;; The return value from here is interpreted as follows:
;;;  nil - execute the next statement in the program list
;;;  T - stop execution
;;;  otherwise, the value is the next struct basic-statement to be executed
(defun execute-statement (statements)
  (let ((statement (car statements)))
    (tracef "~d: ~a~%" (basic-statement-lineno statement) (basic-statement-kind statement))
    (funcall (basic-statement-body statement) statements)))

;;; If the next line to be executed is just the next line of the program,
;;; executing a statment returns:
;;;   - T if program execution should stop,
;;;   - nil if the next statement of the program should be executed, or
;;;   - the next statement to be executed (GOTO/GOSUB/NEXT, etc.)
(defun do-continue (starting-lineno)
  (handler-case (do
		 ((more-statements (if starting-lineno (statement-at-lineno starting-lineno) *program-statement-list*)
				   (if result result (cdr more-statements)))
		  result)
		 ((or (null more-statements) (eql T result)))
		  ;; XXX -- this is the place to poll whether Ctrl-C has been pressed.
		  (setf *current-statement-position* more-statements)
		  (setf result (execute-statement more-statements)))
    (basic-error (c) (format t "~a~%" c)))
  t)

(defun do-cmd-cont (args)
  (declare (ignore args))
  (if *current-statement-position*
      (do-continue (basic-statement-lineno (cadr *current-statement-position*)))
      (format t "Program execution is complete. Type RUN to start again.~%"))
  t)

;;; Turn the "list of lists" form of the program into a list of statement-structs.
;;; A "line" consists of a list with first element the line-number,
;;; and the rest being a list of lists, each component list being
;;; a statement to be executed, along with its arguments.
;;; (linestr (lineno (statement 1) (statement2) ...))
(defun process-ast (ast)
  (let (prg)
    (loop for i to (1-(length ast))
       do (let* ((line (elt ast i))
		 (lineno (caadr line))
		 (stmt-list (cdadr line)))
	    (dolist (stmtform stmt-list)
	      (let ((stmt
		     (make-basic-statement
		      :kind (car stmtform)
		      :args (cdr stmtform)
		      :lineno lineno
		      :body (basic-statement-function (car stmtform)))))
		(push stmt prg)))))
    (nreverse prg)))

(defun do-cmd-run (args)
  (program-reset)
  (setf *program-statement-list* (process-ast *program-lines*))
  ;;(format t "Program is ~s~%~%" *program-statement-list*)
  (process-data-statements nil)
  ;;(format t "Data is ~s~%~%" *program-data*)
  (let ((starting-lineno (and (car args) (handler-case (parse-integer (car args))
					   (sb-int:simple-parse-error (c) (format t "~a~%" c) nil)
					   (type-error (c) (format t "~a~%" c) nil)))))
    (do-continue starting-lineno))
  t)

(defun last-lineno ()
  (if *program-lines*
      (caadr (elt *program-lines* (1- (length *program-lines*))))
      0))

(defun do-cmd-delete (args)
  (multiple-value-bind (first-line last-line) (parse-numeric-range (car args))
    (cond ((and first-line (null last-line)) (setf last-line first-line))
	  ((or (null last-line) (= 0 last-line)) (setf last-line (1+ (last-lineno)))))
    (if (null first-line)
	(setf first-line -1))
    (let (new-prog-lines)
      (dolist (line *program-lines*)
	(cond ((or (< (caadr line) first-line)
		   (> (caadr line) last-line))
	       (push line new-prog-lines))))
      (setf *program-lines* (nreverse new-prog-lines))))
  t)

(defun add-line-work (parsed-line)
  (when parsed-line
    ;; If there is already a line with the given lineno, replace it.  Otherwise, add it.
    (let ((existing-line-pos (position (caadr parsed-line) *program-lines* :key #'caadr)))
      (if existing-line-pos
	  (setf (elt *program-lines* existing-line-pos) parsed-line)
	  (push parsed-line *program-lines*)))))

(defun do-cmd-add-line (line)
 (handler-case (add-line-work (parse-line line))
   (basic-error (c) (format t "~a~%" c)))
  ;; Re-sort the line list
 (setf *program-lines* (sort-lines *program-lines*))
 t)

(defun do-cmd-list (args)
  (multiple-value-bind (first-line last-line) (parse-numeric-range (car args))
    (cond ((and first-line (null last-line)) (setf last-line first-line))
	  ((or (null last-line) (= 0 last-line)) (setf last-line (1+ (last-lineno)))))
    (if (null first-line)
	(setf first-line -1))
    (dolist (line *program-lines*)
      (cond ((and (>= (caadr line) first-line)
		  (<= (caadr line) last-line))
	     (format t "~a~%" (car line))
	     ))))
  t)

(defun do-cmd-load (args)
  (let* ((fname (car args))
	 (suffix-pos (search ",r" fname)))
    ;;(format t "LOAD ~s~%" fname)
    (if suffix-pos
	(setf fname (subseq fname 0 suffix-pos))
	(do-cmd-new '("")))
    (unless (search ".bas" fname :test #'CHAR-EQUAL)
      (setf fname (concatenate 'string fname ".bas")))
    (handler-case (dolist (line (parse-file fname))
		    (add-line-work line))
      (basic-error (c) (format t "~a~%" c))))
  ;; Re-sort the line list
  (setf *program-lines* (sort-lines *program-lines*))
  t)

;;; XXX -- need to add support for ,a (actually the opposite, but...)
(defun do-cmd-save (args)
  (with-open-file (stream (car args)
			  :direction :output
			  :if-exists :supersede)
    (dolist (line *program-lines*)
      (write-line (car line) stream)))
  t)

(defun do-cmd-trace (args)
  (cond ((or (null args) (= 0 (length (string-trim " " (car args)))))
	 (format t "TRACE IS ~[OFF~;ON~]~%" *trace-flag*))
	((search "ON" (car args) :test #'string-equal) (setf *trace-flag* 1))
	((search "OFF" (car args) :test #'string-equal) (setf *trace-flag* 0))
	(T (format t "Unknown trace cmd ~a~%" (car args))))
  t)

(defun do-cmd-eval (args)
  (print (eval (read-from-string (car args))))
  (terpri)
  t)

(defun do-cmd-quit (args)
  (declare (ignore args))
  nil)

(defstruct basic-command
  name
  fn)

(defparameter *command-list* (make-hash-table :test #'equal))

;;;(maphash #'(lambda (k v) (format t "k ~s v ~s~%" k v)) *command-list*)
(defun init-command-list ()
  (dolist (clent
	    `(("NEW" ,#'do-cmd-new)
	      ("RUN" ,#'do-cmd-run)
	      ("LOAD" ,#'do-cmd-load)
	      ("SAVE" ,#'do-cmd-save)
	      ("DELETE" ,#'do-cmd-delete)
	      ("LIST" ,#'do-cmd-list)
	      ("TRACE" ,#'do-cmd-trace)
	      ("EVAL" ,#'do-cmd-eval)
	      ("CONT" ,#'do-cmd-cont)
	      ("QUIT" ,#'do-cmd-quit)))
    ;;(format t "Adding command ~s~%" clent)
    (setf (gethash (car clent) *command-list*) (make-basic-command :name (car clent) :fn (cadr clent)))))

(defparameter *basic-prompt* "LBASIC] ")

(defun prompt-for-command ()
  (princ *basic-prompt*)
  (princ " ")
  (let ((str (read-line *standard-input*)))
    ;;(format t "GOT '~s'~%" str)
    (let* ((split-args (split-sequence #\space str :remove-empty-subseqs t)))
      (if (null split-args) (list nil nil)
      (let* ((cmdname (string-upcase (car split-args)))
	     (cmd (gethash cmdname *command-list*))
	     (lineno (handler-case (parse-integer cmdname) (sb-int:simple-parse-error () nil))))
	;;(format t "CMDNAME ~s yields ~s~%" cmdname cmd)
	(cond (cmd (list (basic-command-fn cmd) (if (string-equal cmdname "EVAL") str (cdr split-args))))
	      (lineno (if (null (cdr split-args))
			  (list #' do-cmd-delete (list (format nil "~d" lineno)))
			  (list #'do-cmd-add-line str)))
	      (t (format t "???~%") (list nil nil))))))))

(defun basic-repl ()
  (init-command-list)
  (do-cmd-new "")			; start fresh!
  (do ((ret t)
       (cmd-and-args (prompt-for-command) (and ret (prompt-for-command))))
      ((null ret))
    ;;(format t "CMD: ~S~%" cmd-and-args)
    (if (and (listp cmd-and-args) (car cmd-and-args))
	(setf ret (funcall (car cmd-and-args) (cadr cmd-and-args))))
    ;;(format t "Command returned ~s~%" ret)
    ))
