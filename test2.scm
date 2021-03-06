(define (valid lst)
         (cond ((list? (member (cadr lst) '(1 3 5 7 8 10 12))) (list? (member (car lst) '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31))))
                ((list? (member (cadr lst) '(4 6 9 11))) (list? (member (car lst) '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30))))
                ((= (cadr lst) 2) (list? (member (caddr lst) '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28))))
                (else #f)))

(define (check lst1 lst2)
    (define a (cadr lst1))
    (define b (cadr lst2))
       (cond ((> (caddr a) (caddr b)) lst1)
              ((< (caddr a) (caddr b)) lst2)
              ((> (cadr a) (cadr b)) lst 1)
              ((< (cadr a) (cadr b)) lst 2)
              ((> (car a) (car b)) lst1)
              (else lst2)))

(define (remove lst ele)
      (cond ((null? lst) '())
             ((equal? (car lst) ele) (remove (cdr lst) ele))
             (else (cons (car lst) (remove (cdr lst) ele)))))
      
(define (main lst)
     (cond ((null? lst) 'invalid_dates)
            ((and (= (length lst) 1) (valid (cadar lst))) (caar lst))
            ((= (length lst) 1) 'invalid_dates)
            ((and (valid (cadar lst)) (valid (cadar(cdr lst)))) (main (remove lst (check (car lst) (cadr lst)))))
            ((not (valid (cadar lst))) (main (remove lst (car lst))))
            (else (main (remove lst (cadr lst)))))) 
            
