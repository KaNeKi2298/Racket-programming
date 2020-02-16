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

(define operators (list #\+ #\- #\* #\( #\)))
(define math_ops (list #\+ #\- #\*))
(define input (list #\( 1 #\+ 2 #\) #\* #\( 3 #\* 4 #\- 5 #\))) ; (list #\( 1 #\+ 2 #\) #\* #\( 3 #\- 4 #\)) ; ; (list 1 #\+ 2 #\- 3 #\+ 4)

(define (infixToPostfix lis)
  (cond
    [(empty? lis)
         (if (equal? (top stack) -1)
             empty ; if top of stack == (
             (cons (pop stack) (infixToPostfix lis))
         )
     ]
    [(member (car lis) operators)
     (if (equal? (car lis) #\) )         ; if current symbol == )
            (let ((temp (pop stack)))
              (if (equal? temp #\( )
               (infixToPostfix (cdr lis)) ; if top of stack == (
               (cons temp (infixToPostfix lis)) ; else
               )
              )
           (cond ; else
             [
              (and (member (top stack) math_ops)  ; check for precedence here
                   (or (equal? (car lis) #\+) (equal? (car lis) #\-)))
               (cons (pop stack) (infixToPostfix lis))
              ]
             [
              else (begin
                     (push stack (car lis))
                     (infixToPostfix (cdr lis))
                   )
             ]
          ) 
         )
     ]
    [else (cons (car lis)
                (infixToPostfix (cdr lis)))]))

(display (infixToPostfix input))
