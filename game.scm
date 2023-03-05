(define (inc n) (+ n 1))
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
   board))

(define (get-cell board x y)
  (cond ((or (< x 0) (<= (length board) x)) 0)
        ((or (< y 0) (<= (length (car board)) y)) 0)
        (else (nth y (nth x board)))))

(define (neighbors board x y)
  (map
   (lambda (coords)
     ;; (display coords)
     (get-cell board
               (+ x (car coords))
               (+ y (cadr coords))))
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
  (map
   (lambda (x row)
     (map
      (lambda (y col)
        (rules board x y))
      (range 0 (length row))
      row))
   (range 0 (length board))
   board))

(define (play board)
  (sleep 1)
  (print-board board)
  (newline)
  (let ((next (generate board)))
    (if (equal? board next)
        "DEAD"
        (play (generate board)))))


(define start '((0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 1 0 0 0)
                (0 0 0 0 1 0 0 0 0)
                (0 0 0 0 1 0 0 0 0)
                (0 0 0 0 0 1 0 0 0)
                (0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0)))
