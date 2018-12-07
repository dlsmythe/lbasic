
(in-package :lbasic)

(defparameter *scanner-trace* nil)
(defmacro scantrace (&rest args)
  (when *scanner-trace*
    `(format t ,@args)))

(deflexer *basic-lexer*
  ;; Primitive Values
  ("\\n"
     ;;(scantrace "SCANNED NEWLINE~%")
     (return (values 'TOK-NEWLINE nil)))
  ("[0-9]+([.][0-9]+([Ee][0-9]+)?)"
   (return (values 'NUM (num %0))))
  ("[.][0-9]+([Ee][0-9]+)?"
   (return (values 'NUM (num %0))))
  ("[0-9]+"
   (scantrace "SCANNED INT ~s~%" %0)
   (return (values 'INT (int %0))))
  ("\"[^\"]*\""
   (scantrace "SCANNED string ~s~%" %0)
   (return (values 'STR (subseq %0 1 (1- (length %0))))))

  ;; Tokens
  ("[+]" (return (values 'TOK-PLUS 'TOK-PLUS)))
  ("[-]" (return (values 'TOK-MINUS 'TOK-MINUS)))
  ("[*]" (return (values 'TOK-ASTERISK 'TOK-ASTERISK)))
  ("/" (return (values 'TOK-SLASH 'TOK-SLASH)))
  ("\\^" (return (values 'TOK-UPARROW 'TOK-UPARROW)))
  ("#" (return (values 'TOK-HASH 'TOK-HASH)))
  ("," (return (values 'TOK-COMMA 'TOK-COMMA)))
  (";"
   ;;(scantrace "SCANNED TOK-SEMICOLON~%")
   (return (values 'TOK-SEMICOLON 'TOK-SEMICOLON)))
  (":" (return (values 'TOK-COLON 'TOK-COLON)))
  ("[(]" (return (values 'TOK-LPAREN 'TOK-LPAREN)))
  ("[)]" (return (values 'TOK-RPAREN 'TOK-RPAREN)))

  ;; Operators
  (">=" (return (values 'OP>= 'OP>=)))
  ("<=" (return (values 'OP<= 'OP<=)))
  ("<>" (return (values 'OP<> 'OP<>)))
  ("><" (return (values 'OP<> 'OP<>)))
  ("<"
   (scantrace "SCANNED: ~s~%" %0)
   (return (values 'OP< 'OP<)))
  (">" (return (values 'OP> 'OP>)))
  ("=" (return (values 'OP= 'OP=)))

  ;; Keywords
  ("GOTO"
   ;;(scantrace "SCANNED GOTO~%")
   (return (values 'KW-GOTO 'KW-GOTO)))
  ("GOSUB" (return (values 'KW-GOSUB 'KW-GOSUB)))
  ("GO"
   ;;(scantrace "SCANNED GO~%")
   (return (values 'KW-GO 'KW-GO)))
  ("REM.*"
   ;;(scantrace "SCANNED REM")
   (return (values 'KW-REM 'KW-REM)))
  ("DATA" (return (values 'KW-DATA 'KW-DATA)))
  ("READ" (return (values 'KW-READ 'KW-READ)))
  ("RESTORE" (return (values 'KW-RESTORE 'KW-RESTORE)))
  ("PRINT"
   ;;(scantrace "SCANNED PRINT~%")
   (return (values 'KW-PRINT 'KW-PRINT)))
  ("END"
   ;;(scantrace "SCANNED END~%")
   (return (values 'KW-END 'KW-END)))
  ("STOP" (return (values 'KW-STOP 'KW-STOP)))
  ("LET" (return (values 'KW-LET 'KW-LET)))
  ("IF"
   (scantrace "SCANNED 'KW-IF~%")
   (return (values 'KW-IF 'KW-IF)))
  ("THEN" (return (values 'KW-THEN 'KW-THEN)))
  ("RETURN" (return (values 'KW-RETURN 'KW-RETURN)))
  ("FOR" (return (values 'KW-FOR 'KW-FOR)))
  ("TO" (return (values 'KW-TO 'KW-TO)))
  ("STEP" (return (values 'KW-STEP 'KW-STEP)))
  ("NEXT" (return (values 'KW-NEXT 'KW-NEXT)))
  ("DEF" (return (values 'KW-DEF 'KW-DEF)))
  ("DIM" (return (values 'KW-DIM 'KW-DIM)))
  ("MIN" (return (values 'OP-MIN 'OP-MIN)))
  ("MAX" (return (values 'OP-MAX 'OP-MAX)))
  ("AND" (return (values 'KW-AND 'KW-AND)))
  ("OR" (return (values 'KW-OR 'KW-OR)))
  ("NOT" (return (values 'KW-NOT 'KW-NOT)))
  ("TAB" (return (values 'KW-TAB 'KW-TAB)))
  ("SPA" (return (values 'KW-SPA 'KW-SPA)))
  ("LIN" (return (values 'KW-LIN 'KW-LIN)))
  ("ON" (return (values 'KW-ON 'KW-ON)))
  ("OF" (return (values 'KW-OF 'KW-OF)))
  ("CONVERT" (return (values 'KW-CONVERT 'KW-CONVERT)))
  ("INPUT" (return (values 'KW-INPUT 'KW-INPUT)))
  ("LINPUT" (return (values 'KW-LINPUT 'KW-LINPUT)))
  ("ENTER" (return (values 'KW-ENTER 'KW-ENTER)))
  ("USING" (return (values 'KW-USING 'KW-USING)))
  ("IMAGE" (return (values 'KW-IMAGE 'KW-IMAGE)))
  ("OPEN" (return (values 'KW-OPEN 'KW-OPEN)))
  ("CLOSE" (return (values 'KW-CLOSE 'KW-CLOSE)))
  ("FILES" (return (values 'KW-FILES 'KW-FILES)))
  ("ASSIGN" (return (values 'KW-ASSIGN 'KW-ASSIGN)))
  ("ADVANCE" (return (values 'KW-ADVANCE 'KW-ADVANCE)))
  ("UPDATE" (return (values 'KW-UPDATE 'KW-UPDATE)))
  ("CREATE" (return (values 'KW-CREATE 'KW-CREATE)))
  ("LOCK" (return (values 'KW-LOCK 'KW-LOCK)))
  ("UNLOCK" (return (values 'KW-UNLOCK 'KW-UNLOCK)))
  ("SYSTEM" (return (values 'KW-SYSTEM 'KW-SYSTEM)))
  ("CHAIN" (return (values 'KW-CHAIN 'KW-CHAIN)))
  ("COM" (return (values 'KW-COM 'KW-COM)))
  ("MAT" (return (values 'KW-MAT 'KW-MAT)))
  ("ZER" (return (values 'KW-ZER 'KW-ZER)))
  ("CON" (return (values 'KW-CON 'KW-CON)))
  ("IDN" (return (values 'KW-IDN 'KW-IDN)))
  ("TRN" (return (values 'KW-TRN 'KW-TRN)))
  ("INV" (return (values 'KW-INV 'KW-INV)))

  ;; Variable reference (possibly a user-defined function reference - XXX)
  ("[:alpha:][:alnum:]*[$]?"
   (scantrace "SCANNED ID ~s~%" %0)
   (return (values 'ID %0)))
  ("[:space:]+")
  )
