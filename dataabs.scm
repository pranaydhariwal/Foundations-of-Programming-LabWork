(define (make-rat1 p q)
   (cons (list p) (list '/ (list q))))
(define (get-nr1 lst)
   (caar lst)) 
(define (get-dr1 lst)
   (car(caddr lst))) 
(define (make-rat2 p q)
   (cons p (list q)))    
(define (get-nr2 lst)
    (car lst))
(define (get-dr2 lst)
    (cadr lst))
(define (plus-rat r1 r2)
     (make-rat2 ( + (* (get-nr2 r1) (get-dr2 r2)) (* (get-nr2 r2) (get-dr2 r1))) (* (get-dr2 r1) (get-dr2 r2))))
(define (mul-rat r1 r2)
     (make-rat1 (* (get-nr1 r1) (get-nr1 r2)) (* (get-dr1 r1) (get-dr1 r2))))
(define (print-rat r1)
       r1)         
(define (equal-rat1? r1 r2)
  (equal? (mul-rat r1 (make-rat1 (gcd (get-nr1 r2) (get-dr1 r2)) (gcd (get-nr1 r2) (get-dr1 r2)))) (mul-rat r2 (make-rat1 (gcd (get-nr1 r1) (get-dr1 r1)) (gcd (get-nr1 r1) (get-dr1 r1))))))
                
(define (get-roll lst)
  (car lst))
(define (get-marks lst)
  (cadr lst))
(define (get-firstperson lst)
  (car lst))
(define (rest-oflist lst)
  (cdr lst))  
(define (equalmks-roll lst r)
  (cond ((null? lst) '())
         ((equal-rat1? (get-marks (get-firstperson lst)) r) (cons (get-roll (get-firstperson lst)) (equalmks-roll (rest-oflist lst) r)))
         (else (equalmks-roll (rest-oflist lst) r))))