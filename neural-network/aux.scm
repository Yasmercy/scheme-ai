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

;; returns a new list with n repeated that many times
(define (repeat ele n)
  (cond ((= n 0) '())
        (else (cons ele (repeat ele (- n 1))))))

;; map a function on every element of a two-window
;; precondition: length of arr is >= 1
(define (map-windows-2 f arr)
  (cond ((empty? (cdr arr)) '())
        (else (cons
                (f (car arr) (cadr arr))
                (map-windows-2 f (cdr arr))))))

;; do all functions in the list onto an input
;; pipes the output of the prev function into the next
(define (apply-all fs input)
  (cond ((empty? fs) input)
        (else (apply-all (cdr fs) ((car fs) input)))))

;; returns the last element from a linked list
;; precondition: arr is not empty
(define (last arr)
  (cond ((empty? (cdr arr)) (car arr))
        (else (last (cdr arr)))))

;; vector subtraction of two arrays
(define (sub arr1 arr2)
  (map (lambda (x) (- (car x) (cadr x))) (zip arr1 arr2)))

(define (add arr1 arr2)
  (map (lambda (x) (+ (car x) (cadr x))) (zip arr1 arr2)))

;; general methods
(define (len arr) 
  (cond ((empty? arr) 0)
        (else (+ 1 (len (cdr arr))))))

(define (empty? arr) (eq? arr '()))

(define (index arr n)
  (cond ((= n 0) (car arr))
        (else (index (cdr arr) (- n 1)))))
