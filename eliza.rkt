;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname eliza) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; checks if list of two symbols are equal

(define (same? l1 l2)
  (local [(define (symbols-equal? l1 l2)
            (cond [(and (empty? l1) (empty? l2)) true]
                  [(symbol=? (first l1) (first l2))
                   (symbols-equal? (rest l1) (rest l2))]
                  [else false]))]
    (cond [(or (empty? l1) (empty? l2)) false]
          [(= (length l1) (length l2))
           (symbols-equal? l1 l2)]
          [else false])))

(check-expect (same? '(a bf ace) '(ae cq ve)) false)
(check-expect (same? '(a bf ace) '(a bf ace)) true)
(check-expect (same? '(a bf ace) '(ae cq sdv ve)) false)

(define (match-quest p s)
  (cond [(symbol=? (first p) (first s))
         (match-quest (rest p) (rest s))]
        [(symbol=? (first p) '?)
         (cond [(empty? (or (second p) (second s))) true]
               [(= (second p) (second s)) (match-quest (rest (rest p)) (rest (rest s)))]
               [else false])]
        [else false]))
        
(define (extract-quest p t))
        
