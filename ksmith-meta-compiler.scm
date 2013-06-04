;;Kristen Smith
;;ksmith22@brandeis.edu
;;April 2013

;; Scheme-to-Scheme compiler
;; with lexical addressing, multiple arguments, letrec, call/cc, enter, exit

; pretty print things
(#%require (only racket/pretty pretty-print))
(define pp pretty-print)

(define (compile exp env-names)
  (cond ((constant? exp)
         (compile-constant exp))
        ((variable? exp)
         (compile-variable exp env-names))
        ((letrec? exp)
         (compile-letrec (letrec-vars exp)
                         (letrec-vals exp)
                         (letrec-body exp)
                         env-names))
        ((lambda? exp)
         (compile-lambda (binders exp) (body exp) env-names))
        ((if? exp)
         (compile-if (predicate exp)
                     (then-part exp)
                     (else-part exp)
                     env-names))
        ((sequence? exp)
         (compile-sequence (cdr exp) env-names))
        ((enter? exp)
         (compile-enter (enter-body exp) env-names))
        ((exit? exp)
         (compile-exit (exit-body exp) env-names))
        ((call/cc? exp)
         (compile-call/cc (cadr exp) env-names))
        (else ; it's an application!
         (compile-application (function-of exp)
                              (arguments-of exp)
                              env-names))))

(define (compile-constant c)
  (lambda (env-values cont)
    (cont c)))

(define (compile-variable v env-names)
  (let ((a (lookup-variable v env-names)))
    (lambda (env-values cont)
      (cont (fetch env-values a)))))

(define (compile-sequence sequence env-names)
  (let ((compiled-first
        (compile (car sequence) env-names)))
    (if (null? (cdr sequence))
        compiled-first
        (let ((compiled-rest 
              (compile-sequence (cdr sequence) env-names)))
          (lambda (env-values cont)
            (compiled-first
               env-values
               (lambda (a) (compiled-rest env-values cont))))))))

(define (compile-if test then else env-names)
  (let ((test-code (compile test env-names))
        (then-code (compile then env-names))
        (else-code (compile else env-names)))
    (lambda (env-values cont)
      (test-code env-values
                 (lambda (p)
                   ((if p then-code else-code) env-values cont))))))

(define (compile-arguments args env-names)
  (if (null? args)
      (lambda (env-values cont)
         (cont '()))
      (let ((first-code (compile (car args) env-names))
            (rest-code (compile-arguments (cdr args) env-names)))
        (lambda (env-values cont)
          (first-code env-values
                      (lambda (first-value)
                        (rest-code env-values
                                   (lambda (rest-values)
                                     (cont (cons first-value 
                                                rest-values))))))))))

;has to work with compile-lambda
(define (compile-application fun args env-names) 
  ;(compile-lambda args fun env-names) instead??
  (let ((fun-code (compile fun env-names))
        (arg-code (compile-arguments args env-names))) 
    (lambda (env-values cont)
      (fun-code env-values 
                (lambda (f)
                  (arg-code env-values (lambda (a)
                                         (f a cont)))))))) 

(define (compile-lambda binders exp env-names) 
  (let ((body-code (compile exp (cons binders env-names))))
    (lambda (env-values cont) 
      (cont (lambda (x k)
              (body-code (cons x env-values) k))))))
      
                            
        

(define (compile-letrec vars vals body env-names)
  (let ((new-env-names (add-frame vars env-names)))
    (let ((val-codes (map (lambda (val) (compile val new-env-names))
                          vals))
          (body-code (compile body new-env-names)))
      (lambda (env-values cont)
        (let ((new-env-values 
              (add-frame (map (lambda (v) '*UNDEFINED*) vals) env-values)))
          (bind-values val-codes (car new-env-values) new-env-values)
          (body-code new-env-values cont))))))

(define (bind-values compiled-bindings frame-values new-env-values)
  (if (null? frame-values)
      'done
      ((car compiled-bindings) 
       new-env-values
       (lambda (b)
         (set-car! frame-values b)
         (bind-values (cdr compiled-bindings)
                      (cdr frame-values)
                      new-env-values)))))

(define (compile-enter exp env-names)
  (let ((body-code (compile exp (add-frame (list '*EXIT*) env-names))))
    (lambda (env-values cont)
      (body-code (add-frame (list cont) env-values) cont))))

(define (compile-exit exp env-names)
  (let ((body-code (compile exp env-names))
        (i (lookup-variable '*EXIT* env-names)))
    (lambda (env-values cont)
      (body-code env-values (fetch env-values i)))))

;Test code for (try test4), which I typed into interpreter
;to test compile-call/cc:
(define test4 
  '(call/cc (lambda (k) (+ 5 (k 3)))))

(define (compile-call/cc exp env-names) 
  (let ((body (compile exp env-names)))
    (lambda (env-values cont)
        (body env-values 
         (lambda (m)
           ;why is list needed
           ;what is m going to be
           ;why apply "cont" and not k within the list
           (m (list (lambda (x k) (apply cont x))) cont)))))) 
            

; Syntax stuff

(define (begins-with atom)
  (lambda (exp)
    (if (pair? exp) (equal? (car exp) atom) #f)))

(define call/cc? 
  (begins-with 'call/cc))

(define constant? integer?)
(define (variable? v) (not (pair? v)))

(define letrec? (begins-with 'letrec))
(define (letrec-vars exp) (map car (cadr exp)))
(define (letrec-vals exp) (map cadr (cadr exp)))
(define letrec-body caddr)

(define lambda? (begins-with 'lambda))
(define binders cadr)
(define body caddr)

(define if? (begins-with 'if))
(define predicate cadr)
(define else-part cadddr)
(define then-part caddr)

(define sequence? (begins-with 'begin))

(define enter? (begins-with 'enter))
(define exit? (begins-with 'exit))
(define enter-body cadr)
(define exit-body cadr)

(define function-of car)
(define arguments-of cdr)

; Initial environment and continuation stuff

;-------------Problem #1------------------------------------------------------------------

;use 1+ to generate ref numbers for the variable
;so lambda can take two inputs

(define (1+ x)
  (+ x 1))



;outputs the number of variable v in a specific list/row of the list of frames
;modified from intro of problem-set explanation
(define (simple-lookup v env-names)
  (cond ((null? env-names) '())
        ((eq? v (car env-names)) 0)
        (else (1+ (simple-lookup v (cdr env-names))))))

;(cons (cdr (car env-frames))(cdr env))

;(1)lookup which frame the variable is going to be in (let ((list-num (simple-lookup <v?> 
;(2)Look through the particular row. 
(define (lookup-variable v env-names) 
  (define (search a b env)
      (cond ((null? (car env)) (search (1+ a) 0 (cdr env)))
            ((eq? (caar env) v)(list a b))
            (else (search a (1+ b) (cons (cdr (car env))(cdr env))))))
  (search 0 0 env-names))


(define (nth n lst)
      (if (eq? n 0)
          (car lst)
          (nth (- n 1)(cdr lst))))
(define (fetch env-values lst)
  (let ((a (car lst))
        (b (cadr lst)))
    (nth b (nth a env-values))))
  
               

;-----------------------------------------------------------------------------------------

(define (add-frame frame env)
  (cons frame env))

(define (initial-continuation v) v)

(define (extend var val bindings)
  (cons (cons var val) bindings))

(define (prim-op op)
  (lambda (x k) (k (apply op x))))

(define initial-global-environment
  (extend 'cons (prim-op cons)
  (extend 'car (prim-op car)
  (extend 'cdr (prim-op cdr)
  (extend 'null? (prim-op null?)
  (extend 'pair? (prim-op pair?)
  (extend 'zero? (prim-op zero?)
  (extend 'true #t
  (extend 'false #f
  (extend '- (prim-op -)
  (extend  '* (prim-op *)
  (extend '+ (prim-op +)
  (extend '= (prim-op =)
  (extend '< (prim-op <)
  (extend '1+ (prim-op 1+) '() )))))))))))))))

(define initial-names (cons (map car initial-global-environment) '()))
(define initial-values (cons (map cdr initial-global-environment) '()))

(define (try exp) 
  ((compile exp initial-names) initial-values initial-continuation))

(define test1
'(letrec
   ((neg (lambda (x) (- 1 x)))
    (square (lambda (x) (* x x)))
    (fun (lambda (a b) (neg (+ (square a) (square b))))))
   (fun (fun 10 20) 30)))

(define test2
'(letrec
   ((neg (lambda (x) (- 1 x)))
    (odd (lambda (n) (if (= n 0) 0 (even (- n 1)))))
    (even (lambda (n) (if (= n 0) 1 (odd (- n 1))))))
   (even 11)))
;;-----------------#4--------------------------------------------------
(define ff
  (try '(letrec ((fact
                  (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))))
          fact)))

(define (link proc)
  (lambda w 
    (proc w initial-continuation)))
 ;;---------------------------------------------------------------------  