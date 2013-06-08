;;Exercise 4.68.  Define rules to implement the reverse operation of exercise
;;2.18, which returns a list containing the same elements as a given list in
;;reverse order. (Hint: Use append-to-form.)
;;
;;Can your rules answer both (reverse (1 2 3) ?x) and (reverse ?x (1 2 3)) ?


(load "ch4-query.scm")
(initialize-data-base microshaft-data-base)
(query-driver-loop)

;;load the append-to-form (from the book)
(assert! (rule (append-to-form () ?y ?y)))

(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
	       (append-to-form ?v ?y ?)))

;; The reverse of the empty list is the empty list
(assert! (rule (reverse () ())))

;; The rest (cdr) of the list, when reversed and appended to the first (car) is
;; the reversed list.
(assert! (rule (reverse (?first . ?rest) ?reversed)
	       (and (reverse ?rest ?rest-rev)
		    (append-to-form ?rest-rev (?first) ?reversed))))

;;Can your rules answer both (reverse (1 2 3) ?x) and (reverse ?x (1 2 3)) ?

;;; Only (reverse (1 2 3) ?x) works since the other form (with ?x first)
;;; results in an infinite loop.  Since the unbounded variable is in the left
;;; most position and the search is depth first search, it will hit a stack
;;; overflow from the constant recursion.

;; See SICP_4_68.pl for the prolog version
