;; file for toy example generators and fitness functions

;; the generator for the chromosome constructor
;; this returns a new chromosome with an additional gene
;; this specific toy-generator would just generate 1/3rds as 1
(define (one-third-generator)
  ;; this function can be implemented without branching
  (if (occured 1 3) 1 0))

(define (random-generator-3)
  (rand-range 0 3))

;; a fitness function
;; that just counts the amount of 1 alleles
(define (count-one-fitness chromosome)
  (cond ((eq? chromosome '()) 0)
        ((= (car chromosome) 0) (count-one-fitness (cdr chromosome)))
        (else (+ 1 (count-one-fitness (cdr chromosome))))))

(define (num-cross-magic population-size)
  (truncate (/ population-size 5)))

(define (num-mutate-magic population-size)
  (truncate (/ population-size 5)))

(define (degree-mutate-magic chromosome-size)
  (truncate (/ chromosome-size 5)))

;; define a preset experiment using the above toy functions
(load "lib.scm")
(define x (create-genetic-experiment 100 100 one-third-generator count-one-fitness num-cross-magic num-mutate-magic degree-mutate-magic))
(define y (create-genetic-experiment 100 100 random-generator-3 count-one-fitness num-cross-magic num-mutate-magic degree-mutate-magic))

