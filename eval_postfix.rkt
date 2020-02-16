#lang racket

(define stack (list -1))

(define (push st x)
  (set! stack (cons x st)))

(define (pop st)
  (let ((result (car st)))
    (set! stack (cdr st))
    result))

(define (top st)
  (car st)
 )

(define operators (list #\+ #\- #\*))

(define input '(1 2 #\+ 4 #\- 4 #\+)) ; '(1 2 #\+ 3 4 #\* 5 #\- #\*)

(define (evalPostfixExpr lis)
  (cond
    [(empty? lis)
         (top stack)
     ]
    [(member (car lis) operators)
     (cond
       [
        (equal? (car lis) #\+)
          (begin
            (let
             ([n (+ (pop stack) (pop stack))])
             (push stack n)
            (evalPostfixExpr (cdr lis))
            )
           )
        ]
       [
        (equal? (car lis) #\*)
          (begin
            (let
             ([n (* (pop stack) (pop stack))])
             (push stack n)
            (evalPostfixExpr (cdr lis))
            )
           ) 
        ]
       [
        else
          (begin
            (let
             ([n (- (- (pop stack) (pop stack)))])
             (push stack n)
            (evalPostfixExpr (cdr lis))
            )
           ) 
        ]
       )
     ]
    [else
     (begin
       (push stack (car lis))
       (evalPostfixExpr (cdr lis))
       )
     ]))

(evalPostfixExpr input)
