;;SICP Question 4.59:

(assert! (meeting administration (Friday 1pm)))
(assert! (meeting whole-company (Wednesday 4pm)))
(assert! (meeting accounting (Monday 9am)))
(assert! (meeting administration (Monday 10am)))
(assert! (meeting computer (Wednesday 3pm)))

;;a.
(meeting ?x (Friday . ?y))

;; b.
(assert! (rule (meeting-time ?person ?day-and-time)
	       (or (meeting whole-company ?day-and-time)
		   (and (meeting ?division ?day-and-time)
			(job ?person (?division . ?rest))))))

;;c.
(meeting-time (Hacker Alyssa P) ?x)

;;; Query results:
;; (meeting-time (hacker alyssa p) (wednesday 4pm))
;; (meeting-time (hacker alyssa p) (wednesday 3pm))
