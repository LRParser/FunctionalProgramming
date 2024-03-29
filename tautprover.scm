;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Implements a tautology prover for the propositional calculus in scheme
;;;;
;;;;    Name:       scheme2.scm
;;;;    Purpose:    Implements a tautology prover for the propositional calculus
;;;;
;;;;    Authors:    Jon Boone
;;;;		    Joshua Datko
;;;;		    Paul DeMicco
;;;;		    Joseph Heenan
;;;;
;;;;    Created:    05/25/2013
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; load the boolean-evaluator implementation
(load "beval.scm")

;; walks the tautology expr to find all the variables, then
;; sets up environments matching all possible combinations
;; of truth values for the variables.  Then the expr is
;; passed to (beval ...) with each environmnet in turn.
;; The results are collected and checked to ensure they are
;; all #t
(define (tautology-prover tautology)
  (let* ((symbols (find-variables tautology))
         (envs (generate-envs symbols '()))
         (results (map (tautology-map tautology) envs)))
    (if (all-true? results)
        (begin (write-string "Tautology ")
               (write tautology)
               (write-string " proven.")
               (newline)
               #t)
        (begin
          (write tautology)
          (write-string " is not a tautology.")
          (newline)
          #f))))

;;; all-true? - checks a list to see if all values are the literal #t
(define (all-true? list)
  (if (null? list)
      #t
      (let ((first-element (car list))
            (rest (cdr list)))
        (and (eq? first-element #t)
             (all-true? rest)))))


;;; tautology-map - returns a HOF that is usable by map against the envs
(define (tautology-map tautology)
  (lambda (x) (beval tautology x)))


;; takes a list potentially containing sub-lists and
;; returns all values in a single top level list
(define (flatten expr)
  (let ((flat-expr '()))
    (if (null? expr)
        flat-expr
        (let ((first-element (car expr))
              (rest (cdr expr)))
          (if (list? first-element)
              (append (append (flatten first-element) flat-expr)
                      (flatten rest) flat-expr)
              (cons first-element (flatten rest)))))))


;; check symbols against reserved words
;; returns #f for reserved words; otherwise
;; returns the symbol itself
(define (non-reserved symbol)
  (cond
   ((eq? symbol 'and) #f)
   ((eq? symbol 'or) #f)
   ((eq? symbol 'not) #f)
   ((eq? symbol 'imply) #f)
   ((eq? symbol 'equiv) #f)
   (else symbol)))

;; returns #t if the value is not the literal #f
(define (not-false? x)
  (not (eq? x #f)))

;; eliminates redundant symbols in a list
(define (unique list)
  (if (null? list)
      '()
      (let ((seen-symbols (unique (cdr list)))
            (first-symbol (car list)))
        (if (not (memq first-symbol seen-symbols))
            (cons first-symbol seen-symbols)
            seen-symbols))))
        
                  
;; finds the variables used in a boolean expr
(define (find-variables expr)
  (let* ((flat-expr (flatten expr))
         (symbol-list (filter not-false? (map non-reserved flat-expr))))
    (unique symbol-list)))


;; add a given symbol and value to the given envs
(define (add-to-envs symbol value envs)
  (if (null? envs)
      (cons (list (list symbol value)) envs)
      (map (lambda (x) (cons (list symbol value) x)) envs)))

;; for a given symbol, generate #t value for half of 
;; the environments and #f for the other half
(define (generate-values symbol envs)
  (let ((num-values (/ (length envs) 2))
        (new-envs))
    (if (null? envs)
        (set! new-envs (append (add-to-envs symbol #t envs)
                               (add-to-envs symbol #f envs)))
        (do ((i 0 (+ i 1)))
            ((= i num-values))
          (set! new-envs (append (add-to-envs symbol #t envs)
                                 (add-to-envs symbol #f envs)))))
    new-envs))

;; generate environments with all possible combinations 
;; of #t/#f settings for the variables in the boolean expr
(define (generate-envs symbols envs)
  (if (null? symbols) '()
      (let ((symbol (car symbols)))
        (generate-values symbol (generate-envs (cdr symbols) envs)))))
