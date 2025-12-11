#lang racket
(require "util.rkt")

(define day "9")
(define ex1 "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
")

(define (parse s)
  (map ints (lines s)))

(define (segs L)
  "Create a dictionary of vertical segments at key x and a dictionary
of horizontal segments at key y."
  (let ([verts-at-x (make-hash)]
        [hors-at-y (make-hash)])
    (for ([a L]
          [b (append (cdr L) (list (car L)))])
      (match-let ([(list x1 y1 x2 y2)
                   (append a b)])
        (if (= y1 y2)
            (hash-update! hors-at-y y1
                          (λ (val) (cons (sort (list x1 x2) <) val)) '())
            (hash-update! verts-at-x x1
                          (λ (val) (cons (sort (list y1 y2) <) val)) '()))))
    (values verts-at-x hors-at-y)))

(define (2->4pt p1 p2)
  "Given points P1 and P2, find the other two points to make a rectangle.
Return the points starting at top left and going clockwise."
  (match-let* ([(list x1 y1 x2 y2)
                (append p1 p2)]
               [(list xmin xmax) (sort (list x1 x2) <)]
               [(list ymin ymax) (sort (list y1 y2) <)])
    (list (list xmin ymin)
          (list xmax ymin)
          (list xmax ymax)
          (list xmin ymax))))

(define (check-right pta ptb verts)
  (for*/or ([i (in-range (first pta) (add1 (first ptb)))]
            #:when (hash-ref verts i #f)
            [vs (hash-ref verts i)])
    (<= (first vs) (second pta) (second vs))))

(define (check-box pts verts hors)
  (match-let ([(list a b c d) pts])
    0))

(define (part1 s)
  (let ([L (parse s)])
    (for*/fold ([mx 0])
               ([comb (in-combinations L 2)])
      (match-let ([(list (list a b) (list c d)) comb])
        (max mx (* (add1 (abs (- a c)))
                   (add1 (abs (- b d)))))))))

(define (part2 s)
  (let ([L (parse s)])
    (let-values ([(v h) (segs L)])
      v)
    ))

(time
 (let ([input (file->string (format "~a.txt" day))])
   ;; (printf "Part 1: ~a\n"
   ;;         (list (part1 ex1)
   ;;               (part1 input)
   ;;               ))
   (printf "Part 2: ~a\n"
           (list (part2 ex1)
                 ;; (part2 input)
                 ))
   ))
