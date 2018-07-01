(define (make-family father son-1 son-2)
	(list father son-1 son-2))
(define (get-father family)
	(car family))
(define (get-son1-family family)
	(cadr family))
(define (get-son2-family family)
	(caddr family))
(define (decendents? family)
	(not (and (null? (get-son1-family family)) (null? (get-son2-family family)))))
(define (empty-family? family)
	(null? family))
(define (desendent-person-families person family)
	(cond ((empty-family? family) '())
		((equal? (get-father family) person) (list (get-son1-family family) (get-son2-family family)))
		(else (append (desendent-person-families  person (get-son1-family family)) (desendent-person-families person (get-son2-family family))))))
(define (members family)
    (cond ((null? family) '())
          ((not (list? family)) (list family))
          (else (append (members (get-father family))
                   (members (cdr family))))))
(define (main1 person family)
	(members (desendent-person-families person family)))
(define (decendents-nosons family )
	 (cond ((empty-family? family) '()) 
	 	((not (decendents? family)) (list (get-father family)))
	 	(else (append (decendents-nosons (get-son1-family family)) (decendents-nosons (get-son2-family family))))))

(define (main2 person family)
(append (decendents-nosons (car (desendent-person-families person family))) (decendents-nosons (cadr (desendent-person-families person family))) ))
; '(10 (3 (5 (1 () ()) (2 () ())) ()) (1 () ()))