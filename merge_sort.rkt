#lang racket 

; Merge Sort

; generic way of defining comparisson 
; in this case sorting is done on numbers
; change according to your needs 
(define cmp <) 

; merge two sorted lists
; the criterium for sorting is cmp 
(define (merge l1 l2) 
  (cond
    ((and (null? l1) (null? l2)) null) 
    ((null? l1) l2)
    ((null? l2) l1)
    ((cmp (car l1) (car l2)) (cons (car l1)  (merge (cdr l1) l2)))
    (else (cons (car l2)  (merge l1 (cdr l2))))
  )
)

; compute the position of the element in the middle in order to divide
; the original problem in subproblems (divide et impera)
; if the list has an odd number of elements the position is shifted with
; one to the left 
(define (middle l)
  (if (null? l) 
      0
      (round (/ (length l) 2))))

; returns the first half of l, [first, middle] 
(define (divide-left l)
  (if (null? l)
      null
      (take l (middle l))))

; returns the other half of the list, [middle+1, last) 
(define (divide-right l)
  (if (null? l)
      null
      (take-right l (- (length l) (middle l)))))

; merge sort
; divide et impera
; tree recursion
(define (merge-sort l)
  (if (equal? (length l) 1)
      l
      (merge (merge-sort (divide-left l)) (merge-sort (divide-right l)))
      ))

(merge-sort '(1123 1 123 12 132 1232))
(merge-sort '(9 8 7 6 5 4 3 2 1) )