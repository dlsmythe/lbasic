
(in-package :lbasic)

(defparameter *parse-trace* nil)
(defmacro parsetrace (&rest args)
  (if *parse-trace*
    `(format t ,@args)))

(define-parser *basic-parser*
  (:muffle-conflicts (2 0))
  ;;(:print-derives-epsilon t)
  ;;(:print-first-terminals t)
  ;;(:print-states t)
  ;;(:print-goto-graph t)
  ;;(:print-lookaheads t)
  (:start-symbol Program)
  (:terminals
   (ID
    NUM INT STR
    TOK-LINE TOK-PLUS TOK-MINUS TOK-ASTERISK TOK-SLASH TOK-UPARROW TOK-HASH
    TOK-COMMA TOK-SEMICOLON TOK-COLON TOK-LPAREN TOK-RPAREN TOK-NEWLINE

    OP= OP>= OP<= OP< OP> OP<> OP<>
    OP-MIN OP-MAX

    KW-REM KW-LET KW-DIM KW-DEF
    KW-DATA KW-READ KW-RESTORE
    KW-PRINT
    KW-END KW-STOP
    KW-IF KW-THEN KW-GO KW-GOTO KW-GOSUB KW-RETURN KW-ON KW-OF KW-FOR KW-TO KW-STEP KW-NEXT
    KW-AND KW-OR KW-NOT

    ;; use with PRINT
    KW-TAB KW-SPA KW-LIN

    KW-CONVERT

    KW-INPUT
    KW-LINPUT
    KW-ENTER
    KW-USING KW-IMAGE

    KW-PRINT# KW-INPUT# KW-LINPUT#
    KW-OPEN KW-CLOSE KW-FILES KW-ASSIGN KW-ADVANCE KW-UPDATE KW-CREATE KW-LOCK KW-UNLOCK

    KW-SYSTEM KW-CHAIN KW-COM KW-TIM KW-BRK

    KW-MAT KW-ZER KW-CON KW-IDN KW-TRN KW-INV))
  (:precedence ((:left TOK-ASTERISK TOK-SLASH)
		(:left TOK-PLUS TOK-MINUS)
		(:right TOK-UPARROW OP=)))

  ;; A program is a list of the form:
  ;; ( (lineno (stmt1) (stmt2) ...) (lineno (stmt1) (stmt2) ...) ... )
  (Program
   (Lines
    #'(lambda (ls) (nreverse ls))))

  (Lines
   (Lines Line
	  #'(lambda (ls l)
	      (parsetrace "Lines: PARSED Another Line ~s ~s~&" ls l)
	      (push l ls)))
   (Line
    #'(lambda (l)
	(parsetrace "Lines: PARSED Line ~s~&" l)
	(list l))))

  (Line
   (INT Statements TOK-NEWLINE
	#'(lambda (&rest pargs)
	    (parsetrace "Line: PARSED ~S~&" pargs)
	    (cons (first pargs) (nreverse (second pargs))))))

  (Statements
   (Statements TOK-COLON Statement
	       #'(lambda (&rest pargs)
		   ;;(parsetrace "Statements1: PARSED ~S~&" pargs)
		   (cons (third pargs) (first pargs))))
   (Statement
    #'(lambda (&rest pargs)
	(parsetrace "Statement2: PARSED ~S~&" pargs)
	pargs)))

  ;;; Each of these comes out as a list of the form:
  ;;; (kind arg1 arg2 arg3 ...)
  (Statement
   (KW-CLOSE TOK-HASH INT)

    ;; CONVERT A$ TO N [, lineno]  -- convert string to numeric, error GOTO lineno
    ;; (read-from-string A$) -- NB: whole lisp reader is in effect
    ;; CONVERT N TO A$ -- convert number to string
    ;; (write-to-string N)
   (KW-CONVERT Expression KW-TO ID
	       #'(lambda (tk src to dst)
		   (declare (ignorable to))
		   (parsetrace "PARSED: ~s~%" (list tk src to dst))
		   (list tk src dst)))

   ;; (KW-DATA val val val ...)
   (KW-DATA DataConstantList
	    #'(lambda (kw l)
		(declare (ignore kw))
		(parsetrace "PARSED ~s: ~s~%" 'KW-DATA l)
		(cons 'KW-DATA l)))

   ;; (KW-DEF "func-name" "param-name" Expression)
   (KW-DEF ID TOK-LPAREN ID TOK-RPAREN OP= Expression
	   #'(lambda (tk fnidval lp fnparam rp op e)
	       (declare (ignore tk lp rp op))
	       (parsetrace "PARSED: ~s~%" (list 'KW-DEF fnidval fnparam e))
	       (list 'KW-DEF fnidval fnparam e)))

   ;; (KW-DIM ("v1" (<indices>)) ("v2" (indices)) ...)
   (KW-DIM SubscriptedIDList
	   #'(lambda (&rest pargs)
	       (cons 'KW-DIM (nreverse (cadr pargs)))))

   (KW-END
    #'(lambda (&rest pargs)
	(declare (ignorable pargs))
	(parsetrace "KW-END: PARSED ~S~&" pargs)
	(list 'KW-END)))

   ;; ENTER -- see TSB 2000C 3-34
   ;; GENERAL FORM:

   ;; 100 ENTER #V
   ;; 200 ENTER A,B,C$
   ;; 300 ENTER #V,Kl,K2,K3
   ;; 400 ENTER 25,L,Q

   ;; statement number ENTER # variable1
   ;; statement number ENTER expression, variable2, variable3
   ;; statement number ENTER! variable1, expression, variable2, variable3

   ;; PURPOSE

   ;; Allows the program to limit the time allowed for run-time data input,
   ;; to check the actual time taken to respond, to read in one string or
   ;; numeric variable, to determine whether the input is of the correct
   ;; type, and/or to determine the current user's terminal number.

   ;; COMMENTS

   ;; - The form ENTER # sets variab1e1 to the termina1 number (between 0
   ;;   and 31) of the user.
   ;; - Expression sets the time limit; it must have a value between 1 and
   ;;   255 seconds. Timing starts when all previous statements have been
   ;;   executed and all printing at the user terminal is completed.
   ;; - Variable 2 returns the approximate time the user took to respond. If
   ;;   the user's response was of the wrong type, the value is the negative
   ;;   of the response time. If the user failed to respond in time, the
   ;;   value is set to -256.
   ;; - Variable 3, the data input variable, may be either a numeric or a
   ;;   string variable. A character string being entered should not be en-
   ;;   closed in quotes, but may contain quotes, leading blanks and
   ;;   embedded blanks. Only one data item can be entered per ENTER
   ;;   statement.
   ;; - The ENTER statement differs from the INPUT statement in that a "?"
   ;;   is not printed on the user terminal, and the TSB System returns to
   ;;   the program if the user does not respond within a specified time
   ;;   limit. Also, the system does not generate a linefeed after the user
   ;;   types return.

   ;; - A carriage return is a legitimate input to a string.
   ;; - A string that is too long to be assigned to a requested string
   ;;   variable is truncated on the right.
   ;;
   ;; XXX - NB - This version of BASIC only supports the ENTER T1, T2, <variable-name> variant.
   ;;            The parser accepts the ENTER #, ... variant, but discards the first argument.
   (KW-ENTER TOK-HASH ID TOK-COMMA ID TOK-COMMA ID TOK-COMMA ID
	     #'(lambda (&rest pargs)
		 (parsetrace "KW-ENTER: PARSED ~S~&" pargs)
		 (list 'KW-ENTER (nth 4 pargs) (nth 6 pargs) (nth 8 pargs))))

   (KW-ENTER ID TOK-COMMA ID TOK-COMMA ID
	     #'(lambda (&rest pargs)
		 (parsetrace "KW-ENTER: PARSED ~S~&" pargs)
		 (list 'KW-ENTER (nth 1 pargs) (nth 3 pargs) (nth 5 pargs))))

   (KW-FOR ID OP= Expression KW-TO Expression
	   #'(lambda (&rest pargs)
	       (parsetrace "KW-FOR: PARSED ~S~&" pargs)
	       (LIST 'KW-FOR (nth 1 pargs) (nth 3 pargs) (nth 5 pargs))))
   (KW-FOR ID OP= Expression KW-TO Expression KW-STEP Expression
	   #'(lambda (&rest pargs)
	       (parsetrace "KW-FOR: PARSED ~S~&" pargs)
	       (LIST 'KW-FOR (nth 1 pargs) (nth 3 pargs) (nth 5 pargs) (nth 7 pargs))))
   (KW-GOSUB Expression
	     #'(lambda (&rest pargs)
		 (parsetrace "KW-GOSUB: PARSED ~S~&" pargs)
		 (list 'KW-GOSUB (cadr pargs))))
   (KW-GOTO Expression
	    #'(lambda (&rest pargs)
		(parsetrace "KW-GOTO: PARSED ~S~&" pargs)
		(list 'KW-GOTO (cadr pargs))))
   (KW-GO KW-TO Expression
	    #'(lambda (&rest pargs)
		(parsetrace "KW-GO-TO: PARSED ~S~&" pargs)
		(list 'KW-GOTO (cadr pargs))))
   (KW-GOTO Expression KW-OF IntegerList
	    #'(lambda (&rest pargs)
		(parsetrace "KW-GOTO-OF: PARSED ~S~&" pargs)
		(list 'KW-GOTO (second pargs) (fourth pargs))))
   (KW-GOSUB Expression KW-OF IntegerList
	    #'(lambda (&rest pargs)
		(parsetrace "KW-GOSUB-OF: PARSED ~S~&" pargs)
		(list 'KW-GOSUB (second pargs) (fourth pargs))))
   (KW-GO KW-TO Expression KW-OF IntegerList
	    #'(lambda (&rest pargs)
		(parsetrace "KW-GO-TO-OF: PARSED ~S~&" pargs)
		(list 'KW-GOTO (third pargs) (fifth pargs))))

   ;; It's possible (CF ECMA-55 pf 12.5) that the ON/GOTO expression is 1-based (not zero)
   ;; That would mean either differentiating by keyword, or patching in a (1+ Expression) here.
   (KW-ON Expression KW-GOTO IntegerList
	    #'(lambda (&rest pargs)
		(parsetrace "KW-ON-GOTO: PARSED ~S~&" pargs)
		(list 'KW-GOTO (second pargs) (fourth pargs))))
   (KW-ON Expression KW-GOSUB IntegerList
	    #'(lambda (&rest pargs)
		(parsetrace "KW-ON-GOSUB: PARSED ~S~&" pargs)
		(list 'KW-GOSUB (second pargs) (fourth pargs))))

   ;; XXX - May wish to consider this
   (KW-ON KW-ERROR KW-GOTO Expression)

   (KW-IF Expression KW-THEN INT ; Statement or INT in later BASICs.  Did anyone ever do Expression?
	  #'(lambda (&rest pargs)
	      (parsetrace "~s: PARSED ~S~&" (first pargs) (list (second pargs) (fourth pargs)))
	      (list 'KW-IF (second pargs) (fourth pargs))))
   (KW-INPUT IdList
	     #'(lambda (&rest pargs)
		 (parsetrace "KW-INPUT: PARSED ~S~&" pargs)
		 (cons 'KW-INPUT (cadr pargs))))
   (KW-INPUT STR SemicolonOrComma IdList
	     #'(lambda (&rest pargs)
		 (parsetrace "KW-INPUT-STR: PARSED ~S~&" pargs)
		 (cons 'KW-INPUT (cons 'INCLUDES-STRING (cons (second pargs) (fourth pargs))))))

   (KW-INPUT TOK-HASH INT TOK-COMMA IdList)

   ;; The final assignment is moved to the front and the list of L2R assignments
   ;;  is reversed so the runtime can do them in order.
   ;; So:
   ;;  LET V3=V2=V1=<expression>
   ;; becomes:
   ;;  (KW-LET (<expression>) "V1" "V2" "V3" ...)
   (AssignmentList
	   #'(lambda (l)
	       (let ((ll (nreverse l)))
		 (parsetrace "Assignment: ~s~%" (cons 'KW-LET ll))
	       (cons 'KW-LET ll))))
   (KW-LET AssignmentList
	   #'(lambda (kw l)
	       (let ((ll (nreverse l)))
		 (parsetrace "Assignment: ~s~%" (cons kw ll))
	       (cons kw ll))))

   ;;(KW-LINPUT ???)
   (KW-NEXT ID		  ; IdList -- save this complication for later
	    #'(lambda (kw varname)
		(declare (ignorable kw))
		(list 'KW-NEXT varname)))
   (KW-OPEN Value KW-FOR Access KW-AS TOK-HASH INT)
   ;;(KW-POKE ValueList)

   ;; KW-PRINT Expression (TOK-SEMICOLON Expression)*
   (KW-PRINT
    #'(lambda (&rest pargs)
	(declare (ignorable pargs))
	(parsetrace "PARSE KW-PRINT~%")
	(list 'KW-PRINT)))
   (KW-PRINT PrintList
	     #'(lambda (&rest pargs)
		 (parsetrace "PARSE KW-PRINT: ~s~%" (second pargs))
		 (cons 'KW-PRINT (second pargs))))

   (KW-PRINT TOK-HASH INT TOK-COMMA PrintList)
   ;;(KW-PRINT KW-USING Integer)
   ;;(KW-PRINT KW-USING Integer TOK-SEMICOLON PrintUsingList)
   ;;(KW-PRINT KW-USING STR)
   ;;(KW-PRINT KW-USING STR TOK-SEMICOLON PrintUsingList)
   ;;(KW-IMAGE ImageString)
   (KW-READ IdList
	    #'(lambda (kw l)
		(parsetrace "PARSED ~s: ~s~%" kw l)
		(cons kw l)))
   (KW-REM
    #'(lambda (&rest pargs)
	;;(parsetrace "KW-REM: PARSED ~S~&" pargs)
	(cons 'KW-REM pargs)))
   (KW-RESTORE Lineno
    #'(lambda (kw ln)
	(parsetrace "PARSED ~s ~s~%" kw ln)
	(list kw ln)))
   (KW-RESTORE
    #'(lambda (kw)
	(parsetrace "PARSED ~s~%" kw)
	(list kw)))
   (KW-RETURN
    #'(lambda (kw)
	(parsetrace "PARSED ~s~%" kw)
	(list kw)))
   (KW-RUN
    #'(lambda (kw)
	(parsetrace "PARSED ~s~%" kw)
	(list kw)))
   (KW-STOP
    #'(lambda (kw)
	(parsetrace "PARSED ~s~%" kw)
	(list kw))))

  (ExpressionList
   (ExpressionList TOK-COMMA Expression
	       #'(lambda (l c e)
		   (declare (ignore c))
		   (parsetrace "ExpressionList: parsed ~s , ~s~%" l e)
		   (cons e l)))
   (Expression
    #'(lambda (v)
	(parsetrace "ExpressionList: parsed ~s~%" v)
	(list v))))
   
   (SubscriptedIDList
    (SubscriptedIDList TOK-COMMA SubscriptedID
			     #'(lambda (&rest pargs)
				 (cons (third pargs) (first pargs))))
    (SubscriptedID
     #'(lambda (&rest pargs)
	 pargs)))

   (SubscriptedID
    (ID TOK-LPAREN ExpressionList TOK-RPAREN
    #'(lambda (&rest pargs)
	(parsetrace "SubscriptedID: PARSED ~S~&" pargs)
	(cons 'TOK-SUBSCRIPTED-ID (cons (first pargs) (third pargs))))))

   (OptionalLetKW
    (KW-LET
     #'(lambda (kw)
	 (declare (ignore kw))
	 (parsetrace "OptionalLetKW: parsed LET~%")
	 'KW-LET))
    (nil #'(lambda () (parsetrace "LET-less assignment~%") 'KW-LET)))

   (AssignmentList
    (SubscriptedID OP= AssignmentList
	#'(lambda (&rest pargs)
	    (parsetrace "AssignmentList: SubscriptedID = List: ~s ~s~%" (first pargs) (third pargs))
	    (cons (first pargs) (third pargs))))
    (ID OP= AssignmentList
	#'(lambda (&rest pargs)
	    (parsetrace "AssignmentList: ID = List: ~s ~s~%" (first pargs) (third pargs))
	    (cons (first pargs) (third pargs))))
    (SubscriptedID OP= Expression
	#'(lambda (dv op exp)
	    (declare (ignorable op))
	    (parsetrace "AssignmentList: SubscriptedID = Expr: ~s ~s~%" dv exp)
	    (cons dv (if (listp exp) exp (list exp)))))
    (ID OP= Expression
	#'(lambda (idval op exp)
	    (declare (ignore op))
	    (parsetrace "AssignmentList: ID = Expr: ~s ~s~%" idval exp)
	    (cons idval (list exp)))))

  (Access
   (KW-INPUT)
   (KW-OUTPUT))

  (IdList
   (ID TOK-COMMA IdList
       #'(lambda (idval tk l)
	   (declare (ignore tk))
	   (parsetrace "PARSED IdList ~s~%" (cons idval l))
	   (cons idval l)))
   (ID
    #'(lambda (idval)
	(list idval))))

  (ValueList
   (Value TOK-COMMA ValueList
	  #'(lambda (val tk l)
	      (declare (ignore tk))
	      (parsetrace "PARSED ValueList ~s~%" (cons val l))
	      (cons val l)))
   (value
    #'(lambda (v) v)))

  ;;; XXX - This smells.  Review the actual definition in the "standard".
  (DataConstantList
   (NegateNumericConstant TOK-COMMA DataConstantList
	     #'(lambda (val tk l)
		 (declare (ignore tk))
		 (parsetrace "PARSED DataConstantList ~s~%" (cons val l))
		 (cons val l)))
   (NegateNumericConstant
    #'(lambda (v) (list v)))
   (STR
    #'(lambda (v)
	(parsetrace "Constant: passing back ~S~%" (list 'STR v))
	v)))

  (IntegerList
   (INT TOK-COMMA IntegerList
	#'(lambda (val tk l)
	    (declare (ignore tk))
	    (parsetrace "PARSED IntegerList ~s~%" (cons val l))
	    (cons val l)))
   (INT
    #'(lambda (v) (list v))))

  ;; Each item in the list is either:
  ;;  - a semicolon
  ;;  - a comma
  ;;  - an expression
  ;;  - a list of the form (symbol (expression)) where symbol is one of:
  ;;     - KW-TAB
  ;;     - KW-LIN
  ;;     - KW-SPA
  (PrintList
   (PrintList SemicolonOrComma PrintItem
	      #'(lambda (l s e)
		  (parsetrace "PrintList: ~s~%" (append l (cons s e)))
		  (append l (list s e))))
   (PrintList SemicolonOrComma
	      #'(lambda (l s)
		  (parsetrace "PrintList: ~s~%" (append l (list s)))
		  (append l (list s))))
   (PrintItem
    #'(lambda (v)
	(parsetrace "PrintListItem: passing back ~S~%" (list v))
	(list v))))

  (SemicolonOrComma
   (TOK-SEMICOLON #'(lambda (&rest pargs) (declare (ignore pargs)) 'TOK-SEMICOLON))
   (TOK-COMMA     #'(lambda (&rest pargs) (declare (ignore pargs)) 'TOK-COMMA)))

  (PrintItem
   (KW-LIN TOK-LPAREN Expression TOK-RPAREN
    #'(lambda (&rest pargs)
	(parsetrace "PrintItem: passing back ~S~%" (list 'KW-LIN (third pargs)))
	(list 'KW-LIN (third pargs))))
   (KW-TAB TOK-LPAREN Expression TOK-RPAREN
    #'(lambda (&rest pargs)
	(parsetrace "PrintItem: passing back ~S~%" (list 'KW-TAB (third pargs)))
	(list 'KW-TAB (third pargs))))
   (KW-SPA TOK-LPAREN Expression TOK-RPAREN
    #'(lambda (&rest pargs)
	(parsetrace "PrintItem: passing back ~S~%" (list 'KW-SPA (third pargs)))
	(list 'KW-SPA (third pargs))))
   (Expression
    #'(lambda (v)
	(parsetrace "PrintItem: passing back ~S~%" v)
	v)))

  (Expression
   (AndExp KW-OR Expression
	   #'(lambda (&rest pargs)
	       (parsetrace "Expression: passing back ~S~%" (list 'KW-OR (first pargs) (third pargs)))
	       (list 'KW-OR (first pargs) (third pargs))))
   (AndExp
    #'(lambda (v)
	(parsetrace "Expression: passing back AndExp ~S~%" v)
	v)))

  (AndExp
   (NotExp KW-AND AndExp
	   #'(lambda (&rest pargs)
	       (parsetrace "AndExp: passing back ~S~%" (list 'KW-AND (first pargs) (third pargs)))
	       (list 'KW-AND (first pargs) (third pargs))))
   (NotExp
    #'(lambda (v)
	(parsetrace "AndExp: passing back ~s~%" v)
	v)))

  (NotExp
   (KW-NOT CompareExp
	   #'(lambda (&rest pargs)
	       (parsetrace "NotExp: passing back ~S~%" (list 'KW-NOT (first pargs)))
	       (list 'KW-NOT (first pargs))))
   (CompareExp
    #'(lambda (v)
	(parsetrace "NotExp: passing back ~s~%" v)
	v)))

  (CompareExp
   (CompareExp OP= MinMaxExp
	   #'(lambda (&rest pargs)
	       (parsetrace "CompareExp=: passing back ~S~%" (list 'OP= (first pargs) (third pargs)))
	       (list 'OP= (first pargs) (third pargs))))
   (CompareExp OP<> MinMaxExp
	   #'(lambda (&rest pargs)
	       (parsetrace "CompareExp<>: passing back ~S~%" (list 'OP<> (first pargs) (third pargs)))
	       (list 'OP<> (first pargs) (third pargs))))
   (CompareExp OP> MinMaxExp
	   #'(lambda (&rest pargs)
	       (parsetrace "CompareExp>: passing back ~S~%" (list 'OP> (first pargs) (third pargs)))
	       (list 'OP> (first pargs) (third pargs))))
   (CompareExp OP>= MinMaxExp
	   #'(lambda (&rest pargs)
	       (parsetrace "CompareExp>=: passing back ~S~%" (list 'OP>= (first pargs) (third pargs)))
	       (list 'OP>= (first pargs) (third pargs))))
   (CompareExp OP< MinMaxExp
	   #'(lambda (&rest pargs)
	       (parsetrace "CompareExp<: passing back ~S~%" (list 'OP< (first pargs) (third pargs)))
	       (list 'OP< (first pargs) (third pargs))))
   (CompareExp OP<= MinMaxExp
	   #'(lambda (&rest pargs)
	       (parsetrace "CompareExp<=: passing back ~S~%" (list 'OP<= (first pargs) (third pargs)))
	       (list 'OP<= (first pargs) (third pargs))))
   (MinMaxExp
    #'(lambda (v)
	(parsetrace "CompareExp: passing back ~s~%" v)
	v)))

  (MinMaxExp
   (AddExp OP-MIN MinMaxExp
	   #'(lambda (&rest pargs)
	       (parsetrace "MinMaxExp: passing back ~S~%" (list (second pargs) (first pargs) (third pargs)))
	       (list (second pargs) (first pargs) (third pargs))))
   (AddExp OP-MAX MinMaxExp
	   #'(lambda (&rest pargs)
	       (parsetrace "MinMaxExp: passing back ~S~%" (list (second pargs) (first pargs) (third pargs)))
	       (list (second pargs) (first pargs) (third pargs))))
   (AddExp
    #'(lambda (v)
	(parsetrace "MinMaxExp: passing back ~s~%" v)
	v)))

  (AddExp
   (AddExp TOK-PLUS MultExp
	    #'(lambda (&rest pargs)
		(parsetrace "AddExp: passing back ~S~%" (list 'TOK-PLUS (first pargs) (third pargs)))
		(list 'TOK-PLUS (first pargs) (third pargs))))
   (AddExp TOK-MINUS MultExp
	    #'(lambda (&rest pargs)
		(parsetrace "AddExp: passing back ~S~%" (list 'TOK-MINUS (first pargs) (third pargs)))
		(list 'TOK-MINUS (first pargs) (third pargs))))
   (MultExp
    #'(lambda (v)
	(parsetrace "AddExp: passing back ~s~%" v)
	v)))

  (MultExp
   (MultExp TOK-ASTERISK NegateExp
   #'(lambda (&rest pargs)
       (parsetrace "MultExp1: passing back ~S~%" (list 'TOK-ASTERISK (first pargs) (third pargs)))
       (list 'TOK-ASTERISK (first pargs) (third pargs))))

   (MultExp TOK-SLASH NegateExp
	      #'(lambda (&rest pargs)
		  (parsetrace "MultExp2: passing back ~S~%" (list 'TOK-SLASH (first pargs) (third pargs)))
		  (list 'TOK-SLASH (first pargs) (third pargs))))
   (NegateExp
    #'(lambda (v)
	(parsetrace "MultExp3: passing back ~s~%" v)
	v)))

  (NegateExp
   (TOK-MINUS PowerExp
    #'(lambda (&rest pargs)
	(parsetrace "NegateExp1: passing back ~S~%" (list 'TOK-MINUS (first pargs)))
	(list 'TOK-MINUS (second pargs))))
   (PowerExp
    #'(lambda (v)
	(parsetrace "NegateExp2: passing back ~s~%" v)
	v)))

  (PowerExp
   (PowerExp TOK-UPARROW Value
    #'(lambda (&rest pargs)
	(parsetrace "PowerExp: passing back ~S~%" (list 'TOK-UPARROW (first pargs) (third pargs)))
	(list 'TOK-UPARROW (first pargs) (third pargs))))
   (Value
    #'(lambda (v)
	(parsetrace "PowerExp: passing back ~S~%" v)
	v)))

  (Value
   (TOK-LPAREN Expression TOK-RPAREN
	       #'(lambda (lp v rp)
		   (declare (ignore lp rp))
		   (parsetrace "Value: PARSED paren-exp: ( ~s )~%" v)
		   v))
   (ID
    #'(lambda (v)
	(parsetrace "Value: PARSED-ID: ~s~%" v)
	(list 'VAR v)))
   (SubscriptedID ; an array reference, substring or fn call
       #'(lambda (id)
	   (parsetrace "Value: PARSED-SUBSCRIPTED-ID: ~s~%" id)
	   id))
   (Constant
    #'(lambda (v)
	(parsetrace "Value: passing back ~S~%" v)
	v)))

  (NegateNumericConstant
   (NumericConstant
    #'(lambda (v)
	(parsetrace "NegateNumericConstant: passing back ~S~%" v)
	v))
   (TOK-MINUS NumericConstant
	      #'(lambda (tk v)
		  (declare (ignore tk))
		  (parsetrace "NegateNumericConstant: passing back ~S~%" (list 'TOK-MINUS v))
		  (list 'TOK-MINUS v))))

  (NumericConstant
   (NUM
    #'(lambda (v)
	(parsetrace "NumericConstant: passing back ~S~%" v)
	v))
   (INT
    #'(lambda (v)
	(parsetrace "NumericConstant: passing back ~S~%" v)
	v)))

  (Constant
   (NUM
    #'(lambda (v)
	(parsetrace "Constant: passing back ~S~%" v)
	(coerce v 'FLOAT)))
   (INT
    #'(lambda (v)
	(parsetrace "Constant: passing back ~S~%" v)
	v))
   (STR
    #'(lambda (v)
	(parsetrace "Constant: passing back ~S~%" v)
	v)))
  )

(defun parse-line (line &key lineno)
  (let ((line-nl (format nil "~a~%" line)))
    ;;(parsetrace "parse-line: ~s~%" (let ((s line-nl)) (loop for i to (1- (length s)) collect (elt s i))))
    (let ((result (handler-case (parse-with-lexer (*basic-lexer* line-nl) *basic-parser*)
		    (YACC-PARSE-ERROR (c)
		      (declare (ignorable c))
		      (error 'basic-error :text (format nil "~a~a" c (if lineno (format nil " on input line ~d" lineno))))
		      (return-from parse-line nil)))))
      ;;(parsetrace "result ~S~%" result)
      (cons line result))))

(defun parse-file (fname)
  (let ((lineno 0)
	program)
    (handler-case
	(with-open-file (in-stream fname :direction :input)
	  (do ((line (read-line in-stream nil nil) (read-line in-stream nil nil)))
	      ((null line))
	    (when (null line) (return-from parse-file program))
	    (incf lineno)
	    (let ((trimmed-line (string-trim " " line)))
	      (when (< 0 (length trimmed-line))
		(let ((result (handler-case (parse-line trimmed-line :lineno lineno)
				(yacc-parse-error (c)
				  (error 'basic-error :text (format nil "Parse error on file line ~d: ~a~%" lineno c) nil)))))
		  (if result (push result program)))))))
      (SB-INT:SIMPLE-FILE-ERROR (c) (format t "~a~%" c)))
    (nreverse program)))

;; This is debugging code

(defun list-lexer (list)
  #'(lambda ()
      (let ((value (pop list)))
	(if (null value)
	    (values nil nil)
	    (values (first value) (second value))))))

(defun parse-list (prg)
  (parse-with-lexer (list-lexer prg) *basic-parser*))
