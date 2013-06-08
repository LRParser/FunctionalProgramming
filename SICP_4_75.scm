(load "query-unique.scm")
(initialize-data-base microshaft-data-base)
(query-driver-loop)

;;; Query input:
(unique (job ?x (computer wizard)))

;;; Query results:
;; (unique (job (bitdiddle ben) (computer wizard)))

;;; Query input:
(unique (job ?x (computer programmer)))

;;; Query results:

;;; Query input:
(and (job ?x ?j) (unique (job ?anyone ?j)))

;;; Query results:
;; (and (job (aull dewitt) (administration secretary)) (unique (job (aull dewitt) (administration secretary))))
;; (and (job (cratchet robert) (accounting scrivener)) (unique (job (cratchet robert) (accounting scrivener))))
;; (and (job (scrooge eben) (accounting chief accountant)) (unique (job (scrooge eben) (accounting chief accountant))))
;; (and (job (warbucks oliver) (administration big wheel)) (unique (job (warbucks oliver) (administration big wheel))))
;; (and (job (reasoner louis) (computer programmer trainee)) (unique (job (reasoner louis) (computer programmer trainee))))
;; (and (job (tweakit lem e) (computer technician)) (unique (job (tweakit lem e) (computer technician))))
;; (and (job (bitdiddle ben) (computer wizard)) (unique (job (bitdiddle ben) (computer wizard))))


;; Write a query that lists all people who supervise precisely one person:

;;; Query input:
(and (supervisor ?x ?y)
     (unique (supervisor ?anyone ?y)))

;;; Query results:
;; (and (supervisor (cratchet robert) (scrooge eben)) (unique (supervisor (cratchet robert) (scrooge eben))))
;; (and (supervisor (reasoner louis) (hacker alyssa p)) (unique (supervisor (reasoner louis) (hacker alyssa p))))
