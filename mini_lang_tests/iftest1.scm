;;i:=5;
;;if i then return := 1 else return := 0 fi

(define iftest1
  '((assign i 5)
    (if i ((1)) ((0)))))


(meval iftest1)
