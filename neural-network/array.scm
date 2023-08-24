;; a module for a 2d cons-list and its methods
(load "aux.scm")

;; creating a 2d matrix with row-length and col-length
(define (make-matrix n m generator)
  ;; helper to create a 1d matrix of size
  (define (make-1d size generator)
    (cond ((= size 0) '())
          (else (cons (generator) (make-1d (- size 1) generator)))))

  ;; concatenating n rows together
  (cond ((= n 0) '())
        (else (cons (make-1d m generator) (make-matrix (- n 1) m generator)))))

;; create a diagonal matrix from a linked list
(define (make-diagonal arr)
  (define (create-vector ele i n)
    (append (repeat 0 i) (cons ele (repeat 0 (- n i 1))))) 
  (define (helper arr i n)
    (cond ((= i n) '())
          (else (cons (create-vector (car arr) i n)
                      (helper (cdr arr) (+ i 1) n)))))
  (helper arr 0 (len arr)))

;; size getters
(define (num-rows matrix) (len matrix))
(define (num-cols matrix) (len (car matrix))) ;; precondition: array is 2d

;; indexing
;; precondition: num-rows(matrix) > i, num-cols(array) > j
(define (index matrix i j)
  ;; helper for 1d matrixs
  (define (index-1d arr n)
    (cond ((= n 0) (car arr))
          (else (index-1d (cdr arr) (- n 1)))))
  
  ;; returning matrix[i][j]
  (index-1d (index-1d matrix i) j))
  
;; reversing all elements of the matrix
(define (rev arr)
  (define (rev-helper arr acc)
    (cond ((empty? arr) acc)
          (else (rev-helper (cdr arr) (cons (car arr) acc)))))
  (rev-helper arr '()))


;; defining operations
(define (transpose matrix)
  ;; prepend an array of elements to a 2d out
  (define (prepend-arr add out)
    (map (lambda (c) (cons (car c) (cadr c))) (zip add out)))
  
  ;; repeating the prepend-arr across each row
  (define (transpose-reversed acc arr)
    (cond ((empty? arr) acc)
          (else 
            (transpose-reversed (prepend-arr (car arr) acc) (cdr arr)))))

  ;; returning the transposed matrix
  (map rev (transpose-reversed (make-matrix (num-cols matrix) 0 '()) matrix)))

;; method for matrix multiplication
;; precondition: num-col(matrix1) = num-row(matrix2)
(define (@ matrix1 matrix2)
  ;; define dot products of two vectors
  (define (dot-product arr1 arr2)
    ;; precondition: len(arr1) = len(arr2)
    (cond ((empty? arr1) 0)
          (else (+ (* (car arr1) (car arr2)) (dot-product (cdr arr1) (cdr arr2))))))

  ;; helper method
  ;; takes in the current row of arr1
  ;; and fills in the dot-products with each col of arr2 
  ;; usage: matrix should be arr2.T
  (define (fill-row vec matrix)
    (cond ((empty? matrix) '())
          (else (cons (dot-product vec (car matrix)) (fill-row vec (cdr matrix))))))

  ;; do the matrix multiplication across each row with an accumulator
  (define (do-mult m1 m2-transposed)
    (cond ((empty? m1) '())
          (else (cons (fill-row (car m1) m2-transposed) (do-mult (cdr m1) m2-transposed)))))

  ;; return the output of the multiplication
  (do-mult matrix1 (transpose matrix2)))

(define (matrix-sub m1 m2)
  (cond ((empty? m1) '())
        (else
          (cons (sub (car m1) (car m2))
                (matrix-sub (cdr m1) (cdr m2))))))
