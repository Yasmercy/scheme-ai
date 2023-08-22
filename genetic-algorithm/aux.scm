;; a module for useful iterator and non-iterator functions

;; define increment
(define (++ x) (+ x 1))

;; define decrement
(define (-- x) (- x 1))

;; list of length 0
(define (empty? arr) (equal? arr '()))

;; takewhile
(define (takewhile arr pred)
  (cond 
    ((empty? arr) arr)
    ((pred (car arr)) (cons (car arr) (takewhile (cdr arr) pred)))
    (else '())))

;; take
;; preconditon: length of arr > n
(define (take arr n)
  (cond 
    ((= n 0) '())
    (else (cons 
            (car arr)
            (take (cdr arr) (- n 1))))))

;; dropwhile
(define (dropwhile arr pred)
  (if (empty? arr)
    arr
    (if (pred (car arr))
      (dropwhile (cdr arr) pred)
      arr)))

;; drop
;; preconditon: length of arr > n
(define (drop arr n)
  (cond 
    ((= n 0) arr)
    (else (drop (cdr arr) (- n 1)))))

;; indexing a linked list
;; precondition: len(arr) > n
(define (index arr n)
  (if (= n 0)
    (car arr)
    (index (cdr arr) (- n 1))))

;; length of a linked list
(define (len arr)
  (if (empty? arr)
    0
    (+ 1 (len (cdr arr)))))

;; split array into two equal halves
;; the halves can have a length that differs by 0
(define (split arr)
  ;; find the middle index
  (let* ((size (len arr))
         (middle (truncate (/ size 2))))
    ;; take while element is not index
    ;; drop while element is not index
    (list (take arr middle) (drop arr middle))))

;; pop the element at index n (of a linked list)
;; returns a pair of the popped element, and the resultant linked list
;; precondition: len(arr) > n
(define (pop arr n)
  (cond
    ((= n 0) (list (car arr) (cdr arr)))
    (else 
      (let ((res (pop (cdr arr) (- n 1))))
        (list (car res) (cons (car arr) (cadr res)))))))

;; merging two sorted sublists
(define (merge arr1 arr2 f)
  (cond 
    ((empty? arr1) arr2)
    ((empty? arr2) arr1)
    ((f (car arr1) (car arr2))
     (cons (car arr1) (merge (cdr arr1) arr2 f)))
    (else
      (cons (car arr2) (merge arr1 (cdr arr2) f)))))

;; merge sort (on a linked list)
(define (merge-sort arr f)
  ;; base case
  (cond 
    ((empty? arr) arr)
    ((empty? (cdr arr)) arr)
    (else 
      (let
       ((halved (split arr)))
       (merge
         (merge-sort (car halved) f)
         (merge-sort (cadr halved) f)
         f)))))

;; random within the range [low, high)
(define (rand-range low high)
  (+ low (random (- high low))))

;; return whether the event happened
;; with the probability of numerator / denominator
(define (occured num denom)
  (< (rand-range 0 denom) num))

;; repeat a function call n times
;; piping the ouput of the previous into the next
(define (feed-forward start f n)
  (cond ((= n 0) start)
        (else
          (feed-forward (f start) f (- n 1)))))

;; apply all functions in the list of functions
;; onto a given start element
(define (apply-all start fs)
  (cond ((eq? fs '()) start)
        (else (apply-all ((car fs) start) (cdr fs)))))
