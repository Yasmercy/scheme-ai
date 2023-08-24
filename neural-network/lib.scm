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
(define (get-error network) (index network 2))
(define (get-delta-weights network) (index network 3))

;; property: get the last output
(define (get-last-output network)
  (last (get-activations network)))

;; feeding forward inputs through a neural network
;; this returns a new neural network with updated activations / outputs
(define (feed-forward network inputs)
  ;; method to return the new activations
  (define (feed layers inputs)
    (cond ((empty? layers) (list inputs))
          (else (let ((transformed (@ (car layers) inputs)))
                  (cons inputs (feed (cdr layers) transformed))))))

  ;; putting together the updated neural network (error and dweights not changed)
  (cons (get-weights network) (cons (feed (get-weights network) (transpose (list inputs))) (cddr network))))


;; a method to update the weights, error, and delta weights
;; of a neural network
(define (back-propagation network outputs expected)
  ;; to backpropagate through the network, we have to actually visit each weight/activation in reversed order

  ;; backpropagate the errors across the network
  (define (prop-errors weights activations prev-errors)
    ;; propagate one layer at a time
    (define (prop w o e)
      ;; diagonal matrix of activations @ weight @ error
      (@ (@ o (make-diagonal (map car w))) e))

    ;; return the array of arrays
    (cond ((empty? weights) (list prev-errors))
          (else
            (let ((errors (prop (car weights) (car activations) prev-errors)))
              (cons prev-errors (prop-errors (cdr weights) (cdr activations) errors))))))

  ;; return the propagated errors
  (let ((weights (rev (get-weights network)))
        (activations (cdr (rev (get-activations network))))
        (prev-errors (map list (sub expected (car (transpose outputs))))))
    (rev (prop-errors weights activations prev-errors))))

(define (calculate-gradient weights activations errors)
  (cond ((empty? activations) '())
        (else (cons (component-mult-m (car activations) (car errors))
                    (calculate-gradient (cdr activations) (cdr errors))))))

(define (iterate-network network inputs expected)
  (let* ((new-network (feed-forward network inputs))
         (outputs (get-last-output new-network))
         (errors (back-propagation new-network outputs expected))
         (gradient (calculate-gradient (get-activations new-network) errors)))
    (list
      (matrix-sub (get-weights network) (map transpose gradient))
      (get-activations new-network)
      errors
      gradient)))


(define test (make-network 2 3 2 1 (lambda () 1)))
(define y (feed-forward test '(1 2 3)))
(define out (get-last-output y))
