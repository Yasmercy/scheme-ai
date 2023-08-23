;; module for implementing a neural network

;; implementation details:
;; each layer within a neural network is a matrix
;; that transforms the (nx1) input column array
;; into the (mx1) output array
;; thus, the layer must be of shape (mxn) -> (mxn) (nx1) => (mx1)
(load "array.scm")
(load "aux.scm")

;; a neural network is a list of the 
;; weights, activations, pre-activated outputs, error, and the change in weights since previous generation
(define (make-network num-out num-in size-hidden num-hidden weight-generator)
  (let ((sizes (append (list num-in) (repeat size-hidden num-hidden) (list num-out)))
        (null-generator (lambda () 0))
        (make-layers (lambda (dimensions generator)
                       (map-windows-2 (lambda (n m) (make-matrix m n generator)) dimensions))))
    (list
      (make-layers sizes weight-generator)      ;; weights
      (make-layers (cdr sizes) null-generator)  ;; activations
      (make-layers (cdr sizes) null-generator)  ;; error
      (make-layers (cdr sizes) null-generator)  ;; delta weights
)))

;; define getters for network
(define (get-weights network) (index network 0))
(define (get-activations network) (index network 1))
(define (get-error network) (index network 3))
(define (get-delta-weights network) (index network 4))

;; property: get the last output
(define (get-last-output network)
  (last (get-activations network)))

;; feeding forward inputs through a neural network
;; this returns a new neural network with updated activations / outputs
(define (feed-forward- network inputs)
  (let ((inputs (transpose (list inputs)))
        (fs (map (lambda (layer) (lambda (in) (@ layer in))) (get-weights network))))
    (apply-all fs inputs)))

(define (feed-forward network inputs)
  ;; method to return the new activations
  (define (feed layers inputs)
    (cond ((empty? layers) '())
          (else (let ((transformed (@ (car layers) inputs)))
                  (begin
                    (display transformed)
                    (newline))
                  (cons transformed (feed (cdr layers) transformed))))))

  ;; putting together the updated neural network (error and dweights not changed)
  (cons (get-weights network) (cons (feed (get-weights network) (transpose (list inputs))) (cddr network))))


