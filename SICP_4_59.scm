;;SICP Question 4.59:
;;a.
(meeting ?x (Friday . ?y))

;; b.
(rule (meeting-time ?person ?day-and-time)
      (or (meeting whole-company ?day-and-time)
	  (and (meeting ?division ?day-and-time)
	       (job ?person (?division . ?rest)))))

;;c.
(meeting-time (Hacker Alyssa P) ?x)
