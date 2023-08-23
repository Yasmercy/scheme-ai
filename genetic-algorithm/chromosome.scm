;; an implementation of the chromosome functional type

(load "aux.scm")

;; defining a chromosome 'constructor'
(define (new-chromosome size generator)
  (cond ((= size 0) '())
        (else 
          (cons (generator)
                (new-chromosome (- size 1) generator)))))

;; mutating a chromosome at ~n loci
(define (mutate-chromosome chromosome n)
  (let ((size (len chromosome)))
    ;; swaps the gene from 0->1 or 1->0
    (define (mutate-gene gene)
      (if (= gene 0) 1 0))
    ;; returns whether it should mutate
    (define (should-mutate)
      (occured n size))
    ;; helper function to recurse through chromosome
    (define (recur c)
      (cond ((eq? c '()) '())
            ((should-mutate)
             (cons (mutate-gene (car c))
                   (recur (cdr c))))
             (else 
               (cons (car c)
                     (recur (cdr c))))))
    ;; returns the mutated chromosome
    (recur chromosome)))

;; crossover
;; precondition: len(c1) = len(c2)
(define (crossover chromosome-1 chromosome-2)
  (define locus (rand-range 0 (len chromosome-1)))
  (let ((h1 (take chromosome-1 locus))
        (l1 (drop chromosome-1 locus))
        (h2 (take chromosome-2 locus))
        (l2 (drop chromosome-2 locus)))
    ;; return new pairs with h1-l2 and l2-l1
    (list 
      (append h1 l2)
      (append h2 l1))))
