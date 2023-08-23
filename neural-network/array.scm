;; a module for a 2d cons-list and its methods
(load "aux.scm")

;; creating a 2d matrix with row-length and col-length
(define (make-matrix n m default)
  ;; helper to create a 1d matrix of size
  (define (make-1d size default)
    (cond ((= size 0) '())
          (else (cons default (make-1d (- size 1) default)))))

  ;; concatenating n rows together
  (cond ((= n 0) '())
        (else (cons (make-1d m default) (make-matrix (- n 1) m default)))))

;; size geteters
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

  ;; reversing all elements of the matrix
  (define (rev arr)
    (define (rev-helper arr acc)
      (cond ((empty? arr) acc)
            (else (rev-helper (cdr arr) (cons (car arr) acc)))))
    (rev-helper arr '()))

  ;; returning the transposed matrix
  (map rev (transpose-reversed (make-matrix (num-cols matrix) 0 '()) matrix)))

(define (@ matrix1 matrix2) -1)
