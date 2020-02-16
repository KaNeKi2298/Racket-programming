#lang racket

(define lis (list 5 3 6 9 2 4))

(define tree (list 1 (list 2 (list 3) (list 4 (list 8) (list 9)) (list 5)) (list 6 (list 7))))

tree

(define (myTDisplay l)
  (for ([i l])
    (display i)(display " "))
  (display "\n")
  )

(define (printPathsTailRecursive tree [path (list)])
  (if (null? (cdr tree)) (myTDisplay (reverse (cons (car tree) path)))
      (for ([i (in-range 1 (length tree))])
        (printPathsTailRecursive (list-ref tree i) (cons (car tree) path)))))

(define (myDisplay l)
 (for([i l])
    (for ([j (reverse i)]) (display j) (display " ")) (display "\n"))
  )

(define (func l m)
  (for/list ([i l])
    (append i (list m)))
  )

(define (printPathsRecursive tree)
  (define x (list))
  (if (null? (cdr tree)) (list (list (car tree)))
      (for ([i (in-range 1 (length tree))])
        (for ([j (func (printPathsRecursive (list-ref tree i)) (car tree))]) (set! x (cons j x))) ))
  (if (null? (cdr tree)) (list (list (car tree))) x)
  )

(printPathsTailRecursive tree)

(display "\n")

(myDisplay (printPathsRecursive tree))