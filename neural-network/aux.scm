;; module for iterator methods

;; mapping a function on every element of an array
;; and then returning the output
(define (map f arr)
  (cond ((empty? arr) arr)
        (else (cons (f (car arr)) (map f (cdr arr))))))

;; zipping together two lists of the same size
(define (zip arr1 arr2)
  (cond ((empty? arr1) '())
        (else (cons (list (car arr1) (car arr2)) (zip (cdr arr1) (cdr arr2))))))

;; general methods
(define (len arr) 
  (cond ((empty? arr) 0)
        (else (+ 1 (len (cdr arr))))))

(define (empty? arr) (eq? arr '()))
