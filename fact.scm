(define (fact n)
  (if (= n 0) 1 (* n (fact (- n 1)))))
(fact 3)

;;Trace of the code:

;;
;; Evaluating the define statement
;;

;;1. The REPL prompt waits for input and the define s-expression is entered.

;;2. The define s-exp is passed into to eval, with the global environment,
;;which is initially empty

;;3. Eval detects that this is a "define" statement and dispatches to
;;eval-definition

;;4. eval-definition will evaluate the definition statement and then call
;;define-variable to bind the result.  In this case, we will eventually bind
;;the procedure in the global environment, but first we have to get the
;;definition-value and then eval the result.  In this case, in
;;definition-value, the definition is not a symbol, and make-lambda is called,
;;which adds the lambda prefix to the expression.

;;5. eval is called again, this time on the newly build lambda expression.
;;;5.a. This time make-procedure is called which returns a list consisting of
;;;the procedure name, the parameters of the function, the function of the
;;;body, and the lexical environment of the function.

;;6. With the definition-value now evaluated as a procedure, define-variable!
;;is now called with the name of the procedure, the evaluated procedure object,
;;and the global environment.  This procedure is responsible for the actual
;;binding to the environment.  In this case, it will not find the variable
;;defined in the first frame of the global environment and will bind it there.

;;7. Eval has completed and the REPL will print 'ok and loop back to waiting
;;for input.

;;
;; Evaluating the function call
;;

;; 1.(fact 3) is entered into the REPL as described above and eval is called.

;; 2.  eval detects this is an application and calls apply.  But first, it
;; evaluates the operator, which retrieves the binding for "fact" in the global
;; environment, which is a procedure.  It then passes that and the "3" to
;; apply.

;; 3. fact is a compound procedure, since the list starts with procedure.  This
;; will eventually call eval-sequence, but first the environment is extended by
;; adding a new frame to the global environment.  Then the body of fact is
;; evaluated in this new environment.
;;;3.a. Each expression of the factorial is evaluated in turn.  (= n 0) is
;;;false since "=" is now a primitive expression, which calls the scheme "="
;;;operator, therefore it evaluates (- n 1), again a primitive operator now to
;;;produce 2.
;;;3.b. The statement (fact 2) is now evaluated to be a procedure call, and it
;;;is applied.  This process is recursive to what was described above, but the
;;;new environment is extended to the first fact frame.
;;;3.c. This repeats, extending frames and applying fact until n is bound to 0,
;;;which causes a 1 to be returned in the base case.  The recursive call stack
;;;is "unwounded" and we now multiple the results: (* 3 2 1).  This returns a 6
;;;and that result is printed in the REPL to finish the code.
