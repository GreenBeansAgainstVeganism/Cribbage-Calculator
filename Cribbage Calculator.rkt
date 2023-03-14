#lang racket

;(require math/number-theory)

; list of valid ranks and suits for cards
; A card is represented as a pair of rank and suit
(define ranks `(ace ,@(range 2 11) jack queen king))
(define suits '(hearts spades diamonds clubs))


; Conversion functions from ranks to values
(define (values lst)
  (map value lst))
(define (value card)
  (let ([rank (car card)])
    (cond
      [(number? rank) rank]
      [else (cond
              [(eq? rank 'ace) 1]
              [else 10])])))

; generate a new deck in sorted order
(define (new-deck)
  (foldl (lambda (suit lst)
           (append lst (map
                        (lambda (rank) (cons rank suit))
                        ranks)))
         '() suits))

; generate a random hand of n cards for testing purposes
(define (new-hand size) (take (shuffle (new-deck)) size))


; Compare methods

; Compare by ascending rank
(define (rank-compare a b)
  (< (index-of ranks (car a)) (index-of ranks (car b))))
; Compare by suit
(define (suit-compare a b)
  (< (index-of suits (cdr a)) (index-of suits (cdr b))))
; Compare by ascending rank first, then suit
(define (card-compare a b)
  (let ([rankdif (- (index-of ranks (car a))
                    (index-of ranks (car b)))])
    (or (< rankdif 0) (and (= rankdif 0) (suit-compare a b)))))


; count the number of subsets of a given hand whose values sum to 15
(define (count-15s hand)
  (foldl
   (lambda (sublist tally)
     (if (eq? (foldl + 0 sublist) 15)
         (add1 tally)
         tally))
   0
   (combinations (values hand))))

; count the number of pairs in a given hand
(define (count-pairs hand)
  (count-pairs-helper
   (map car (sort hand rank-compare))
   1))
(define (count-pairs-helper vals streak)
  (cond
    [(and
      (nor (empty? vals) (empty? (cdr vals)))
      (eq? (car vals) (cadr vals)))
     (count-pairs-helper (cdr vals) (add1 streak))]
    [else
     (+
      (apply + (range 1 streak)) ; equivalent to number of pairs per streak size
      (cond
        [(nor (empty? vals) (empty? (cdr vals)))
         (count-pairs-helper (cdr vals) 1)]
        [else 0]))
     ]))

; Count the number of runs of each size in a given hand
; and return the results in a list.
; The list indices indicate the size of run and the values
; indicate how many were counted.
(define (count-runs hand)
  (let ([results
         (sort
          (count-runs-helper
           (map car (sort hand rank-compare))
           1 1 1)
          <
          #:key car)])
    (cond
      [(empty? results) '()]
      [else
       (foldl
        (lambda (item output)
          (cond
            [(eq? (car item) (caar output))
             `((,(car item) . ,(+ (cdr item) (cdar output))) ,@(cdr output))]
            [else (cons item output)]))
        (list (car results))
        (cdr results))])
    ))
(define (count-runs-helper vals streak mult runlen)
  (cond
    [(nor (empty? vals) (empty? (cdr vals))) ; vals has >1 elements left
     (cond
       ; following element is the same rank
       [(eq? (car vals) (cadr vals))
        (count-runs-helper (cdr vals) (add1 streak) mult runlen)
        ]
       ; following element is next rank in sequence
       [(eq? (add1 (index-of ranks (car vals)))
             (index-of ranks (cadr vals)))
        (count-runs-helper (cdr vals) 1 (* mult streak) (add1 runlen))
        ]
       ; following element breaks the run
       [else
        (if (> runlen 2)
            (cons (cons runlen (* mult streak))
                  (count-runs-helper (cdr vals) 1 1 1))
            (count-runs-helper (cdr vals) 1 1 1)
            )
        ]
       )
     ]
    ; end of list
    [else
     (if (> runlen 2)
         (list (cons runlen (* mult streak)))
         '())
     ]))

(define (count-points hand)
  (+ (* 2 (count-15s hand))
     (* 2 (count-pairs hand))
     (foldl
      (lambda (item total) (+ total (* (car item) (cdr item))))
      0
      (count-runs hand))))



; test functions
(define (myprint . args) (display (apply ~a args #:separator "\n")) (display "\n"))

(define (test-hand size)
  (let ([hand (new-hand size)])
    (myprint "hand:" hand
             "sorted:" (sort hand card-compare)
             ""
             "15's:" (count-15s hand)
             "pairs:" (count-pairs hand)
             "runs:" (count-runs hand)
             "total points:" (count-points hand))))

; testing
(test-hand 7)