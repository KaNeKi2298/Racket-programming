#lang racket

(define (makeTree . arg)
   arg
)

(makeTree 1 
(makeTree 2
(makeTree 6)
(makeTree 7
(makeTree 8)
)
)
(makeTree 3
(makeTree 9 (makeTree 10 ) (makeTree 11))
)
(makeTree 4)
(makeTree 5)
)