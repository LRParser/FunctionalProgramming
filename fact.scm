(define (fact n)
  (if (= n 0) 1 (* n (fact (- n 1)))))
(fact 3)

;;Trace of the code:

;;
;; Evaluating the define statement
;;

;;1. The REPL prompt waits for input and the define s-expresion is entered.

;;2. The define s-exp is passed into to eval, with the global environment,
;;which is initially empty

;;3. Eval detects that this is a "define" statment and dispatchs to
;;eval-defintion

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
;;binding to the environemnt.  In this case, it will not find the variable
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

;; 3. fact is a compound procedure, since the list starts with procedure
