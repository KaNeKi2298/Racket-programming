#lang racket

(define lis (list 5 3 6 9 2 4 7 11 8 1 2 5 4 3))

;(split-at lis (quotient (length lis) 2))



(define (merge left right)
  (if (null? left) right
      (if (null? right) left ; If either the left or the right lists are empty, return the other one
         (if (< (car left) (car right))
             (cons (car left) (merge (cdr left) right)) ; If first element of left list is empty, add it as a first element to merged list of (cdr left) and right, this happens in O(1), as only head of list is changed
             (cons (car right) (merge left (cdr right)))
             )
         )
      )
  )
      

(define (mergesort li )
  (define-values (lli rli) (split-at li (quotient (length li) 2))) ; Splits the input list into two halves in O(n) time  
  (if (null? li) li (if (null? (cdr li)) li  ; Return the list as it is if it has only one or zero elements               
      (merge (mergesort lli) (mergesort rli)) ; Run mergesort on the left and right halves and merge the returned lists in O(n)
  )
))

;In essence, the merge and mergesort call run in O(n) time, and mergesort is called O(logn) times, so the total complexity is O(nlogn) 

(mergesort lis)