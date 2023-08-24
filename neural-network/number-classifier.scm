;; module for creating 6x6 bit-maps for numbers
(load "aux.scm")
(load "lib.scm")

;; read the probability map and return a bitmap
(define (read-map bitmap)
  (cond ((empty? bitmap) '())
        (else
          (cons (if (< (random 1.0) (car bitmap)) 1 0)
                (read-map (cdr bitmap))))))

;; define the probability maps for 0-3
(define zero-map
  (list 
    '(0 0)
    (list
    0 0 1 1 0 0
    0 1 0 0 1 0
    1 0 0 0 0 1
    1 0 0 0 0 1
    0 1 0 0 1 0
    0 0 1 1 0 0
    )))

(define one-map
  (list
    '(0 1)
    (list
    0 0 0 1 0 0
    0 0 0 1 0 0
    0 0 0 1 0 0
    0 0 0 1 0 0
    0 0 0 1 0 0
    0 0 0 1 0 0
    )))

(define two-map
  (list
    '(1 0)
    (list
    0 1 1 1 0 0
    1 0 0 0 0 1
    0 0 0 0 1 0
    0 0 1 1 0 0
    0 1 0 0 0 0 
    1 1 1 1 1 1
    )))

(define three-map
  (list
    '(1 1)
    (list
    0 1 1 1 1 0
    1 0 0 0 1 1
    0 0 0 0 1 1
    0 0 1 1 1 1
    0 0 0 1 1 1
    1 1 1 1 1 0
    )))

;; (define four-map
;;   (list
;;     0 0 0 1 0 0
;;     0 0 1 1 0 0
;;     0 1 0 1 0 0
;;     1 1 1 1 1 1
;;     0 0 0 1 0 0
;;     0 0 0 1 0 0
;;     ))

;; create a neural network
(define (weight-generator)
  (- (random 1.0) 0.5))

(define (random-map)
  (let ((chosen (index (list zero-map one-map two-map three-map) (random 4))))
    (list (car chosen) (read-map (cadr chosen)))))

(define (make-dataset n)
  (cond ((= n 0) '())
        (else (cons (random-map) (make-dataset (- n 1))))))

(define nn (make-network 2 36 3 3 weight-generator))
