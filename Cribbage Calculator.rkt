#lang racket

(require math/statistics)
(require data-frame)
(require graphite)

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
  (define (count-15s-helper vals sum)
    (cond
      [(> sum 15) 0]
      [(eq? sum 15) 1]
      [else
       (if (empty? vals) 0
           (+ 
            (count-15s-helper (cdr vals) sum)
            (count-15s-helper (cdr vals) (+ sum (car vals)))))
       ]))
  (count-15s-helper (sort (values hand) >) 0))

; slow
;(define (count-15s hand)
;  (foldl
;   (lambda (sublist tally)
;     (if (eq? (foldl + 0 sublist) 15)
;         (add1 tally)
;         tally))
;   0
;   (combinations (values hand))))

; count the number of pairs in a given hand
(define (count-pairs hand)
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
  (count-pairs-helper
   (map car (sort hand rank-compare))
   1))


; Count the number of runs of each size in a given hand
; and return the results in a list.
; The list indices indicate the size of run and the values
; indicate how many were counted.
(define (count-runs hand)
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
              )]
         )]
      ; end of list
      [else
       (if (> runlen 2)
           (list (cons runlen (* mult streak)))
           '())
       ]))
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

; determines the number of points a hand contains from flushes
; The way this is determined for non-standard hand sizes is to
; count 1 point for every card which has the same suit as at least
; 4 others.
(define (count-flush hand)
  (let ([hand-suits (map cdr hand)])
  ; test for flushes of all four suits and add the results
  (foldl
   (lambda (suit total)
     (let ([n (count (curry eq? suit) hand-suits)])
       (+ total
          (if (> n 4) n 0))))
   0
   suits)))


(define (count-points hand)
  (+ (* 2 (count-15s hand))
     (* 2 (count-pairs hand))
     (count-flush hand)
     (foldl
      (lambda (item total) (+ total (* (car item) (cdr item))))
      0
      (count-runs hand))))



; test functions

; my special print function for convenience
(define (myprint . args) (display (apply ~a args #:separator "\n")) (display "\n"))

; Generate a hand of a given size randomly and print test info on it.
; A random cut card is also added to the hand.
; if do-print? is set to false, the function will instead print nothing
; and just return the total point value
(define (test-random size #:p [do-print? #t])
  (let ([hand (new-hand (add1 size))])
    (if do-print?
        (myprint "hand:" (cdr hand)
                 "cut:" (car hand)
                 "sorted:" (sort hand card-compare)
                 ""
                 "15's:" (count-15s hand)
                 "pairs:" (count-pairs hand)
                 "runs:" (count-runs hand)
                 "flush:" (count-flush hand)
                 "total points:" (count-points hand))
        (count-points hand))))
(define test-r test-random) ; alias

; Generate a random hand of a given size + cribnum extra cards,
; Then pick cribnum cards to throw away based on which will keep the most
; points, and print test info on it.
; A random cut card is also added to the hand.
; if do-print? is set to false, the function will instead print nothing
; and just return the total point value
(define (test-most-points-kept size cribnum #:p [do-print? #t])
  (let* ([hand (new-hand (+ 1 size cribnum))]
         [kept
          (car (foldl
                (lambda (candidate hiscore)
                  ; choose the highest scoring hand
                  (let ([points (count-points candidate)])
                    (if (> points (cdr hiscore))
                        (cons candidate points)
                        hiscore)))
                '(() . 0)
                (combinations (cdr hand) size)))]
         [final-hand (cons (car hand) kept)])
    (if do-print?
        (myprint
         "hand:" (cdr hand)
         "kept:" kept
         "kept points:" (count-points kept)
         "cut:" (car hand)
         "sorted:" (sort final-hand card-compare)
         ""
         "15's:" (count-15s final-hand)
         "pairs:" (count-pairs final-hand)
         "runs:" (count-runs final-hand)
         "flush:" (count-flush final-hand)
         "total points:" (count-points final-hand))
        (count-points final-hand))
    ))
(define test-mpk test-most-points-kept) ; alias
    
; Run a specific test n times and return the average number of points
(define (bulk-test n test . args)
  (let* ([data
          (build-list
           n
           (lambda (junk) (apply test args #:p #f)))]
         [data-frame (make-data-frame)]
         [results (samples->hash data)])
    
     
    (myprint
     "results: " results
     "mean:" (~r (mean data))
     "variance:" (~r (variance data))
     "stddev:" (~r (stddev data)))
    
    (let-values ([(scores frequencies) (count-samples data)])
      (df-add-series! data-frame (make-series "scores" #:data (list->vector scores)))
      (df-add-series! data-frame (make-series "frequencies" #:data (list->vector frequencies)))
      (graph #:data data-frame
             #:mapping (aes #:x "scores" #:y "frequencies")
             #:x-min 0 #:width 800
             (col)
             ))))

; testing