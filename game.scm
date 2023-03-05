(use-modules (ice-9 pretty-print))
(define (inc n) (+ n 1))
(define (dec n) (- n 1))
;; like in javascript
(define (enumerate proc seq)
  (map proc (range 0 (length seq)) seq) )
(define (catimes n proc)
  (map (lambda (i) (proc)) (range 0 n)))
(define (dotimes n proc)
  (when (not (= n 0))
    (proc)
    (dotimes (dec n) proc)))
;; NOTE dont try this at home
(define (str x)
  (cond ((number? x) (number->string x))
        ((list? x) (list->string x))
        (else x)))
(define (nth n l)
  (if (or (> n (length l)) (< n 0))
    (error "Index out of bounds.")
    (if (eq? n 0)
      (car l)
      (nth (- n 1) (cdr l)))))
(define (range start end)
  (define (range-iter start end ag)
    (if (>= start end)
        ag
        (range-iter (inc start) end (cons start ag))))
  (reverse (range-iter start end '())))

(define (print-board board)
  (for-each
   (lambda (row)
     (for-each
      (lambda (col)
        (display (if (= 0 col)
                     " "
                     "X")))
      row)
     (newline))
   board)
  (dotimes (length (car board))
           (lambda () (display "-")))
  (newline))

(define (get-cell board x y)
  (cond ((or (< x 0) (<= (length board) x)) 0)
        ((or (< y 0) (<= (length (car board)) y)) 0)
        (else (nth y (nth x board)))))

(define (neighbors board x y)
  (map
   (lambda (coords)
     (get-cell board
               (+ x (car coords))
               (+ y (cadr coords))))
   ;; surrounding cells
   '((-1 -1)
     (-1 0)
     (-1 1)
     (0 -1)
     (0 1)
     (1 -1)
     (1 0)
     (1 1))))

(define (live-neighbors board x y)
  (length (filter (lambda (n) (= n 1)) (neighbors board x y))))

(define (rules board x y)
  (let ((lns (live-neighbors board x y))
        (live? (= 1 (get-cell board x y))))
    (if live?
        (if (or (= 2 lns) (= 3 lns))
            1
            0)
        (if (= 3 lns)
            1
            0))))

(define (generate board)
  (enumerate
   (lambda (x row)
     (enumerate
      (lambda (y col)
        (rules board x y))
      row))
   board))

(define* (play board #:key (depth 0) (usecs 100000))
  (print-board board)
  (usleep usecs)
  (let ((next (generate board)))
    (if (equal? board next)
        (cons depth board)
        (play (generate board) #:depth (inc depth)))))


(define (build-random-board w h)
  (catimes h (lambda () (catimes w (lambda () (random 2))))))
