;; this is where the public API functions will be stored

(load "aux.scm")
(load "chromosome.scm")

;; creating a new genetic experiment with 
;; the amount of individuals, the amount of genes within a chromosome, and the fitness function
;; the chromosomes inside the experiment would always be in sorted order of fitness
(define (create-genetic-experiment pop-size chrom-size generator fitness num-cross num-mutate degree-mutate)
  ;; an experiment is just a list of chromosomes
  ;; creating a random list of new chromosomes
  (define (create-population n)
    (cond ((= n 0) '())
          (else (cons (new-chromosome chrom-size generator)
                      (create-population (- n 1))))))
  ;; return the data of the current state for an experiment
  (list
    ;; the chromsomes within the population in sorted order
    (merge-sort (create-population pop-size) (lambda (a b) (>= (fitness a) (fitness b))))
    ;; the current generation, as well as data to create
    0 pop-size chrom-size generator fitness num-cross num-mutate degree-mutate))

;; iterate n steps in a generic experiment
(define (iterate-generation experiment n)
  ;; the function that iterates the generation 1 step
  (define (iterate exper)
    ;; defining a function to cross over two random pairs
    ;; and return the resultant experiment
    (define (do-crossover chromosomes)
      (let*
         ((pop-size (len chromosomes))
         (pop1 (pop chromosomes (rand-range 0 pop-size))) ;; popping from original
         (pop2 (pop (cadr pop1) (rand-range 0 (- pop-size 1)))) ;; popping from popped
         (crossed (crossover (car pop1) (car pop2))))
        ;; returning the new experiment with new individuals
        ;; these are not necessarily in order
        (append (cadr pop2) crossed)))

    ;; define a function to mutate the set of chromosomes a degree
    (define (do-mutate chromosomes degree generator)
      (let*
        ((pop-size (len chromosomes))
         (pop1 (pop chromosomes (rand-range 0 pop-size)))
         (mutated (mutate-chromosome (car pop1) (degree (len (car pop1))) generator)))
        (cons mutated (cadr pop1))))

    ;; defining a function to repopulate the population
    ;; by pruning the lower half, and duplicating the upper half
    (define (repopulate chromosomes fitness-func)
      (let* ((pop-size (len chromosomes))
             (middle (truncate (/ pop-size 2)))
             (chromos (merge-sort chromosomes (lambda (a b) (>= (fitness-func a) (fitness-func b))))))
        ;; (bkpt (drop chromos middle))
        (append (take chromos middle) (take chromos (- pop-size middle)))))

    ;; create the new generation
    (let ((pop-size (index exper 2))
          (generator (index exper 4))
          (fitness-func (index exper 5))
          (amount-cross (index exper 6))
          (amount-mutate (index exper 7))
          (degree-mutate (index exper 8))
          (generation (index exper 1)))
      (let* ((functions
               (list
                 (lambda (chromosomes) (feed-forward chromosomes do-crossover (amount-cross pop-size)))
                 (lambda (chromosomes) (feed-forward chromosomes (lambda (c) (do-mutate c degree-mutate generator)) (amount-mutate pop-size)))
                 (lambda (chromosomes) (repopulate chromosomes fitness-func))))
             ;; chromosomes within the next generation
             (next-chromosomes (apply-all (index exper 0) functions)))
        ;; put together the next generation
        ;; (bkpt next-chromosomes (cons (+ generation 1) (cddr exper)))
        (append (list next-chromosomes) (cons (+ generation 1) (cddr exper))))))

  ;; feeding-forward the iterate n times
  ;; returning the elements in sorted fitness
  (let* ((end (feed-forward experiment iterate n))
         (fitness (index end 5))
         (sorted-chromosomes (merge-sort (car end) (lambda (a b) (>= (fitness a) (fitness b))))))
    (append (list sorted-chromosomes) (cdr end))))


;; get the highest fitness value of an experiment
(define (most-fit experiment)
  (caar experiment))
