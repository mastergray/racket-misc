#lang racket
;; Attempt at defining elementary set operations using Racket (and some help from StackOverflow...)

;; Define a procedure for set membership: x e s OR x c s (since equal? can apply to atoms or lists)
(define (isMemberOf x set)
  (if (null? set)
      #f
      (if (equal? (car set) x)
          #t
          (isMemberOf x (cdr set)))))

;; Define a procedure for the intersection set operation: a n b = {x e a * x e b : isect}
(define (isect setA setB)
  (filter (lambda (elem) (isMemberOf elem setB)) setA))

;; Defines a procedure for the relative differnce set operation: a - b: (x e a * x !e b: rdiff}
(define (rdiff setA setB)
  (filter (lambda (elem) (not (isMemberOf elem setB))) setA))

;; Define a procedure for set union operation: a u b = {x e a ^ x e b: union}
(define (union setA setB)
  (remove-duplicates (append setA setB)))

;; Define procedure for a cartesian product: |A x B| = {(x e A, y e B) : cprod}
(define (cprod setA setB)
  ;; Iterates over each element x of A to create a new pair 
  (foldr
   (lambda (x finalResult)
     (foldr
      ;; create new pair (x, y e B) and add it to the final result list
      (lambda (y result)
        (cons (list x y) result))
      ;; Accumalator when iterating over each element y of Set B
      finalResult
      setB))
   ;; Accumalator starts with empty list to store all pairs from
   '()
   setA))

;; Returns the powerset of a given list (https://stackoverflow.com/questions/20622945/how-to-do-a-powerset-in-drracket)
(define (powerset aList)
  (if (empty? aList)
      '(()) 
      (let ((rst (powerset (cdr aList))))
      (append
        (map
         (lambda (x)
           (cons (car aList) x))
          rst)
       rst))))
