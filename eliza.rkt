;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname eliza) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t quasiquote repeating-decimal #f #t none #f () #t)))
(define changelist '((i you)
                      (you me)
                      (me you)
                      (was were)
                      (am are)
                      (are am)
                      (yours mine)
                      (i'd you'd)
                      (i've you've)
                      (i'll you'll)
                      (your my)
                      (my your)))

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

;(check-expect (same? '(a bf ace) '(ae cq ve)) false)
;(check-expect (same? '(a bf ace) '(a bf ace)) true)
;(check-expect (same? '(a bf ace) '(ae cq sdv ve)) false)


(define (match-quest p t)
  (local [(define (might-containq? p)
            (cond [(empty? p) true]
                  [(equal? '* (first p)) false]
                  [else (might-containq? (rest p))]))
          (define (initial p t)
            (cond [(and (empty? p) (empty? t)) true]
                  [(= (length p) (length t)) (compare p t)]
                  [else false]))
          (define (compare p t)
            (cond [(equal? (first t) (first p))
                   (initial (rest p) (rest t))]
                  [(symbol=? (first p) '?) (loop p t)]
                  [else false]))
          (define (loop p t)
            (cond [(empty? (rest t)) true]
                  [(equal? (second t) (second p))
                   (initial (rest (rest p)) (rest (rest t)))]
                  [(symbol=? (second p) '?) (loop (rest p) (rest t))]
                  [else false]))]
    (cond [(might-containq? p) (initial p t)]
          [else false])))

;(match-quest '(CS ? ? ? not fun at ?) '(CS 135 is really not fun at))
;(match-quest empty '(CS 135 is no no fun at all))

(define (extract-quest pwa t)
  (cond [(empty? pwa) empty]
        [(equal? '? (first pwa))
         (cons (list (first t)) (extract-quest (rest pwa) (rest t)))]
        [else (extract-quest (rest pwa) (rest t))]))

;(check-expect (extract-quest '(CS ? is ? fun) '(CS 135 is really fun))
;             '((135) (really)))

(define (match-star p t)
  (local [(define (containa? p)
            (cond [(empty? p) true]
                  [(equal? '? (first p)) false]
                  [(equal? '? (first p)) false]
                  [else (containa? (rest p))]))
          (define (initial p t)
            (cond [(and (empty? p) (empty? t)) true]
                  [else (compare p t)]))
          (define (compare p t)
            (cond [(equal? (first t) (first p))
                   (initial (rest p) (rest t))]
                  [(symbol=? (first p) '*) (loop (rest p) t)]
                  [else false]))
          (define (loop p t)
            (cond [(or (empty? p) (empty? (rest t))) true]
                  [(equal? (first t) (first p))
                   (initial (rest p) (rest t))]
                  [(not (equal? (first t) (first p)))
                   (loop p (rest t))]
                  [else false]))]
    (cond [(containa? p) (initial p t)]
          [(not (= (length p) (length t))) false]
          [else false])))

(match-star '(CS 135) '(CS 135 is no no fun at all))
