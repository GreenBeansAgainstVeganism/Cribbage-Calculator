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

; checks whether the hand contains the jack of the same suit
; as the cut card. If cut is not specified, then it uses the
; first element of hand as the cut card.
(define right-jack?
  (case-lambda
    [(hand)
     (list? (member
             (cons 'jack (cdar hand))
             (cdr hand)))]
    [(hand cut)
     (list? (member
             (cons 'jack (cdr cut))
             hand))]))

; calculates the total of all types of points for a hand.
; set includes-cut? to true to factor in cut-specific points
; like the right jack
(define (count-points hand #:cut? [includes-cut? #f])
  (+ (* 2 (count-15s hand))
     (* 2 (count-pairs hand))
     (count-flush hand)
     (if (and includes-cut? (right-jack? hand)) 1 0)
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
(define (test-random size [cribnum 0] #:p [do-print? #t] #:data [deck (shuffle (new-deck))])
  (let ([hand (take deck (add1 size))])
    (if do-print?
        (myprint "hand:" (cdr hand)
                 "cut:" (car hand)
                 "sorted:" (sort hand card-compare)
                 ""
                 "15's:" (count-15s hand)
                 "pairs:" (count-pairs hand)
                 "runs:" (count-runs hand)
                 "flush:" (count-flush hand)
                 "right jack:" (right-jack? hand)
                 "total points:" (count-points hand #:cut? #t))
        (count-points hand #:cut? #t))))
(define test-r test-random) ; alias

; Generate a random hand of a given size + cribnum extra cards,
; Then pick cribnum cards to throw away based on which will keep the most
; points, and print test info on it.
; A random cut card is also added to the hand.
; if do-print? is set to false, the function will instead print nothing
; and just return the total point value
(define (test-most-points-kept size cribnum #:p [do-print? #t] #:data [deck (shuffle (new-deck))])
  (let* ([hand (take deck (+ 1 size cribnum))]
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
         "right jack:" (right-jack? kept (car hand))
         "total points:" (count-points final-hand #:cut? #t))
        (count-points final-hand #:cut? #t))
    ))
(define test-mpk test-most-points-kept) ; alias


; Generate a random hand of a given size + cribnum extra cards,
; Then pick cribnum cards to throw away based on what would
; yield the highest expected score when considering possible cut cards.
; A random cut card is also added to the hand.
; if do-print? is set to false, the function will instead print nothing
; and just return the total point value
(define (test-best-expected-cut size cribnum #:p [do-print? #t] #:data [deck (shuffle (new-deck))])
  ; Helper method to determine the expected score of a hand given a
  ; random cut card from draw-pile
  (define (expected-score hand draw-pile)
    (/
     (foldl (lambda (cut total)
              (+ total
                 (count-points (cons cut hand) #:cut? #t)))
            0 draw-pile)
     (length draw-pile)))
  (let*-values ([(hand draw-pile) (split-at deck (+ size cribnum))]
                [(kept)
                 (car (foldl
                       (lambda (candidate hiscore)
                         ; choose the highest scoring hand
                         (let ([expected
                                ; Calculate expected value of the hand score
                                ; with respect to the cut card
                                (expected-score candidate draw-pile)])
                           (if (> expected (cdr hiscore))
                               (cons candidate expected)
                               hiscore)))
                       '(() . 0)
                       (combinations hand size)))]
                [(final-hand) (cons (car draw-pile) kept)])
    (if do-print?
        (myprint
         "hand:" hand
         "kept:" kept
         "kept points:" (count-points kept)
         "expected score:" (~r (expected-score kept draw-pile))
         "cut:" (car draw-pile)
         "sorted:" (sort final-hand card-compare)
         ""
         "15's:" (count-15s final-hand)
         "pairs:" (count-pairs final-hand)
         "runs:" (count-runs final-hand)
         "flush:" (count-flush final-hand)
         "right jack:" (right-jack? kept (car draw-pile))
         "total points:" (count-points final-hand #:cut? #t))
        (count-points final-hand #:cut? #t))
    ))
(define test-bec test-best-expected-cut) ; alias
    
; Run a specific test n times and print results
(define (bulk-test n test . args)
  (let* ([data
          (build-list
           n
           (lambda (junk) (apply test args #:p #f)))]
         [data-frame (make-data-frame)]
         [results (samples->hash data)]
         [test-title (~a test)])
    
     
    (myprint
     test-title
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
             #:title test-title
             (col)
             ))))

; transpose a 2d list, e.g.
; ((1 2 3) (4 5 6) (7 8 9)) -> ((1 4 7) (2 5 8) (3 6 9))
; all sublists must be same length
(define (transpose lst)
  (foldl
   (lambda (subl new-lst)
     (map cons subl new-lst))
   (make-list (length (car lst)) '())
   (reverse lst)))

; Run multiple tests n times on the same set of test cases and print results
(define (multi-test n tests . args)
  (let ([data-set
         (transpose (build-list
                     n
                     (lambda (junk)
                       (let ([test-case (shuffle (new-deck))])
                         (map
                          (lambda (test) (apply test args
                                                #:p false #:data test-case))
                          tests)))))])
    (map (lambda (data test)
           (let ([data-frame (make-data-frame)]
                 [results (samples->hash data)]
                 [test-title (~a test)])
            
             (myprint
              test-title
              "results: " results
              "mean:" (~r (mean data))
              "variance:" (~r (variance data))
              "stddev:" (~r (stddev data))
              "")
             
    
             (let-values ([(scores frequencies) (count-samples data)])
               (df-add-series! data-frame (make-series "scores" #:data (list->vector scores)))
               (df-add-series! data-frame (make-series "frequencies" #:data (list->vector frequencies)))
               (graph #:data data-frame
                      #:mapping (aes #:x "scores" #:y "frequencies")
                      #:x-min 0 #:width 800
                      #:title test-title
                      (col)
                      ))
             ))
         data-set tests)))

; testing