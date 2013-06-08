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


(assert! (rule (reverse () ())))

(assert! (rule (reverse (?first . ?rest) ?reversed)
	       (and (reverse ?rest ?rest-rev)
		    (append-to-form ?rest-rev (?first) ?reversed))))


;;TODO: Check if rules can answer both below:

;;Can your rules answer both (reverse (1 2 3) ?x) and (reverse ?x (1 2 3)) ?

;;TODO: Do the same in Prolog.
