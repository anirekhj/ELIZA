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

(define (match-quest p t)
  (local [(define (might-containq? p)
            (cond [(empty? p) true]
                  [(symbol=? '* (first p)) false]
                  [else (might-containq? (rest p))]))
          (define (initial p t)
            (cond [(and (empty? p) (empty? t)) true]
                  [(= (length p) (length t)) (compare p t)]
                  [else false]))
          (define (compare p t)
            (cond [(equal? (first t) (first p))
                   (initial (rest p) (rest t))]
                  [(symbol=? (first p) '?) (loop p t)]))
          (define (loop p t)
            (cond [(empty? (rest t)) true]
                  [(equal? (second t) (second p))
                   (initial (rest (rest p)) (rest (rest t)))]
                  [(symbol=? (second p) '?) (loop (rest p) (rest t))]
                  [else false]))]
    (cond [(might-containq? p) (initial p t)]
          [else false])))

;(check-epect (match-quest '(CS ? is not fun at ?) '(CS 135 is not fun at all)) true)

(define (extract-quest pwa t)
  (cond [(empty? pwa) empty]
        [(symbol=? '? (first pwa))
         (cons (list (first t)) (extract-quest (rest pwa) (rest t)))]
        [else (extract-quest (rest pwa) (rest t))]))

;(check-expect (extract-quest '(CS ? is ? fun) '(CS 135 is really fun))
;             '((135) (really)))
        
