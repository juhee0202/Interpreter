
let def1 = Cons (kw_define, (Cons (x, Cons (hundred, Nil))))
let def2 = Cons (kw_define, (Cons (x, Cons (course, Nil))))
let one_plus_two = Cons (plus, Cons (one, Cons (two, Nil)))
let one_minus_two = Cons (plus, Cons (one, Cons (neg_two, Nil)))
let one_plus_two_three = Cons (plus, Cons (one, Cons (two, Cons (three, Nil))))
let x_plus_x = Cons (plus, Cons (x, Cons (x, Nil)))
let x_plus_y = Cons (plus, Cons (x, Cons (y, Nil)))
let quote1 = Cons (quote, Cons (x, Cons (x, Nil)))
let quote2 = Cons (quote, Cons (one, Nil))
let if1 = Cons (kw_if, Cons (true1, Cons (one, Cons (two, Nil))))
let if2 = Cons (kw_if, Cons (false1, Cons (one, Cons (two, Nil))))
let if3 = Cons (kw_if, Cons (one, Cons (one, Cons (two, Nil))))

let let1 = Cons (kw_let, Cons (Cons(Cons (x, Cons (0, Nil)), Cons (Cons (y, Cons (1, Nil)), Nil)), Cons (x_plus_y, Nil)))
let letstar1 = Cons (kw_letstar, Cons (Cons (Cons (x, Cons (0, Nil)), Cons (Cons (y, Cons (Cons (plus, Cons (1, Cons (x, Nil))), Nil)), Nil)), Cons (x_plus_y, Nil)))
let letrec1 = Cons (kw_letrec, Cons (Cons (Cons (factorial, Cons (Cons 
              (kw_lambda, Cons (Cons (x, Nil), Cons (Cons (kw_if, Cons (Cons 
              (equal, Cons (x, Cons (0, Nil))), Cons (one, Cons (Cons (mult_sign, 
              Cons (x, Cons (Cons (factorial, Cons (Cons (plus_sign, Cons (x, 
              Cons (neg_one, Nil))), Nil)), Nil))), Nil)))), Nil))), Nil)), 
              Nil), Cons (Cons (factorial, Cons (5, Nil)), Nil)))

zardoz> ;; Below we use mutual recursion, where even? and odd? both call one   
        ;; another.  We've mentioned that you can do this in OCaml, but you've
        ;; never had to before.
        (letrec ((even? (lambda (x)
                          (if (equal? x 0)
                              #t
                            (odd? (+ x -1)))))
                 (odd? (lambda (x)
                         (if (equal? x 0)
                             #f
                           (even? (+ x -1))))))
          (even? 5))

#f
zardoz> (define non-nil #t)

()
zardoz> ;; The quote construct might seem a little bit mysterious, so here's an
        ;; example to motivate it: here's a function that binds an already
        ;; defined variable that the user passes in to the empty list:
        (define nihilize
          (lambda (symbol)
            (eval (cons 'set! (cons symbol (cons ''() '()))))))
        ;; Note that we need to double quote the empty list so it's not
        ;; evaluated during the set!.

()
zardoz> non-nil

#t
zardoz> (nihilize 'non-nil)

()
zardoz> non-nil

()
