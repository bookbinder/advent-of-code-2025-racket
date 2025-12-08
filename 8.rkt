#lang racket
(require "util.rkt")

(define day "8")
(define ex1 "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
")

(define (parse s)
  (map ints (lines s)))

(define (dist a b)
  (sqrt
   (+ (expt (abs (- (first a) (first b))) 2)
      (expt (abs (- (second a) (second b))) 2)
      (expt (abs (- (third a) (third b))) 2))))

(define (dist-dict L)
  "Make a dictionary of distances between each pair of pts in L."
  (let ([D (make-hash)])
    (let loop ([L1 L])
      (unless (null? L1)
        (let loop2 ([L2 (cdr L1)])
          (if (null? L2)
              (loop (cdr L1))
              (begin
                (hash-set! D (list (first L1) (first L2))
                           (dist (first L1) (first L2)))
                (loop2 (cdr L2)))))))
    D))

(define (run s part1? [example? #f])
  (let* ([input (parse s)]
         [D (dist-dict input)]
         [D2 (for/list ([k (hash-keys D)])
               (list k (hash-ref D k)))]
         [sets-D (let ([tmp (make-hash)])
                   (for ([x input])
                     (hash-set! tmp x x))
                   tmp)]
         [sizes (let ([tmp (make-hash)])
                   (for ([x input])
                     (hash-set! tmp x 1))
                   tmp)]
         [components (length input)])

    (define (find x)
      (let ([parent (if (equal? x (hash-ref sets-D x))
                        x
                        (find (hash-ref sets-D x)))])
        (let loop ([node x])
          (unless (equal? (hash-ref sets-D x) parent)
            (let ([cur (hash-ref sets-D x)])
              (hash-set! sets-D node parent)
              (loop cur))))
        parent))

    (define (union x y)
      (let ([rootx (find x)]
            [rooty (find y)])
        (unless (equal? rootx rooty)
          (cond ((< (hash-ref sizes rooty) (hash-ref sizes rootx))
                 (hash-update! sizes (hash-ref sets-D rootx)
                               (λ (val) (+ val (hash-ref sizes rooty))))
                 (hash-set! sets-D rooty rootx))
                (else
                 (hash-update! sizes (hash-ref sets-D rooty)
                               (λ (val) (+ val (hash-ref sizes rootx))))
                 (hash-set! sets-D rootx rooty)))
          (set! components (sub1 components)))))
    
    (let/cc ret
      (for ([x D2]
            [_ (if (and part1? example?)
                   10
                   (if part1? 1000 (in-naturals)))])
        (let ([a (first (first x))]
              [b (second (first x))])
          (union a b)
          (when (= 1 components)
            (ret (* (first a) (first b))))))
      (sort (hash-values sizes) >)
      (apply * (take (sort (hash-values sizes) >) 3)))))

(time
 (let ([input (file->string (format "~a.txt" day))])
   (printf "Part 1: ~a\n"
           (list (run ex1 #t #t)
                 (run input #t #f)
                 ))
   (printf "Part 2: ~a\n"
           (list (run ex1 #f #t)
                 (run input #f #f)
                 ))
   ))
