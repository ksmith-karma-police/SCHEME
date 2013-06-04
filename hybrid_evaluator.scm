;;Kristen Smith
;;ksmith22@brandeis.edu
;;Friday 4/19/2013

;;;; REQUIRED CODE FOR PROBLEM SET 5 OF STRUCTURE AND INTERPRETATION 
;;;; COMPUTER PROGRAMS

;;;; It contains the metacircular evaluator from Chapter 4
;;;; (sections 4.1.1-4.1.4) and the lazy evaluator from section 4.2
;;;; of Structure and Interpretation of Computer Programs

;;;; Matches code in ch4.scm except that "eval" is "mc-eval"
;;;; and "apply" is "mc-apply".
;;;; Also includes enlarged primitive-procedures list

;;;; This file can be loaded into Scheme as a whole.
;;;; Then you can initialize and start the evaluator by evaluating
;;;; (driver-loop).

;;;;  To run without memoization, reload the first version of force-it below

;;; From section 4.1.4 -- must precede def of metacircular apply
(define apply-in-underlying-scheme apply)

;;;SECTION 4.1.1

; pretty print things
(#%require (only racket/pretty pretty-print))
(define pp pretty-print)

; error messages
(#%require (only racket/base error))

; Our scheme implements 1+, -1+ as primitive procedures
(define (1+ x) (+ x 1))
(define (-1+ x) (- x 1))

; Our scheme uses true and false, not #t and #f
(define true #t)
(define false #f)


(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((application? exp)
         (mc-apply (mc-eval (operator exp) env)
                   (list-of-values (operands exp) env)))
        ((let*->nested-lets? exp) (mc-eval (let*->nested-lets exp) env))
        ((unsugar-let? exp) (mc-eval (unsugar-let exp) env))
        ((make-unbound? exp) (mc-eval (make-unbound! (car (cdr exp)) (first-frame env))))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (mc-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (mc-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (mc-eval (if-predicate exp) env))
      (mc-eval (if-consequent exp) env)
      (mc-eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (mc-eval (first-exp exps) env))
        (else (mc-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (mc-eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (mc-eval (definition-value exp) env)
                    env)
  'ok)

;;;SECTION 4.1.2

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;;SECTION 4.1.3

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;;SECTION 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'zero? zero?)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list '1+ 1+)
        (list '-1+ -1+)
        (list 'quotient quotient)
        (list 'remainder remainder)
        (list '/ /)
        (list '* *)
        (list '+ +)
        (list '- -)
        (list '= =)
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))


(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (mc-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))


(define the-global-environment (setup-environment))

(pp "To start the metacircular evaluator, evaluate (driver-loop)")

'METACIRCULAR-EVALUATOR-LOADED

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; Your answers to Exercise 4.6, 4.7, and 4.13 goes here.

;--4.6----------------------
(define (unsugar-let? exp)
  (mc-eval (unsugar-let exp) env))

(define (unsugar-let exp)
  (let ((vars (map car (cadr exp)))
        (vals (map cadr (cadr exp)))
        (body (car (cddr exp))))
    (cons (list 'lambda vars body) vals)))

;---4.7----------------------

(define (let*-assignment exp) (car (cdr exp)))
(define (let*-body exp) (cddr (exp)))

(define (let*->nested-lets exp)
  (define (change-let* assignment body)
    (if (null? (cdr assignment))
        (cons 'let (cons assignment body))
        (list 'let (list (car assignment))
                   (change-let* (cdr assignment) body))))
  (change-let* (let*-assignment exp) (let*-body exp)))

(define (let*->nested-lets? exp)
  (mc-eval (let*->nested-lets exp) env))
;--4.13------------------

(define (make-unbound! variable frame)
  (let ((vars (frame-variables frame)) ; car frame
        (vals (frame-values frame))) ; cdr frame
    (if (eq? (car vars) variable)
        (begin (set-car! frame (cdr vars))
               (set-cdr! frame (cdr vals))
               frame)
        ;go through the frame's variables/values table entry by entry
        (unsugar-let ((tempFrame (make-unbound! variable (make-frame (cdr (frame-variables frame))
                                                             (cdr (frame-values frame)))))
              ;return a new frame that is the beginning of the original frame + the modified part
              (make-frame (cons (caar frame) (frame-variables frame))
                          (cons (cadr frame) (frame-values frame))))))))

(define (add-binding var val frame)
  (make-frame (cons var (frame-variables frame))
                        (cons val (frame-values frame))))
        
;?????
(define (make-unbound? exp)
  (mc-eval (make-unbound! (cadr exp) (first-frame env))))
      
 
;; Continue with the answers to Problem 1, 2, and 3 after the code below.
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;;;SECTION 4.2.2

;;; Modifying the evaluator

(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((application? exp)             ; clause from book
         (mc-apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))
  

;force the thunk/unevaluated expression & evaluate it
(define (actual-value exp env)
  (force-it (mc-eval exp env)))

(define (mc-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ; changed -->list of "thunked" parts of the argument
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

;forces eval of everything
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env) ;-->force eval of first operand of the expression(s)only (cons ("force-it" (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

;(cons (mc-eval (first-operand exps) env)
;       (list-of-arg-values 

;delays everything
(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (mc-eval (if-consequent exp) env)
      (mc-eval (if-alternative exp) env)))

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))


;;; Representing thunks

;; non-memoizing version of force-it

(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

;; thunks

;definition of a thunk--tag a list with the word "thunk"
(define (delay-it exp env)
  (list 'thunk exp env))

;it's a thunk if it is a list with the word "thunk" at the beginning
(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk)) ;see delay-it
(define (thunk-env thunk) (caddr thunk)) ;see delay-it

;; "thunk" that has been forced and is storing its (memoized) value
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))


;; memoizing version of force-it

(define (force-it obj)
  (cond ((thunk? obj) ;-->obj hasn't been evaluated
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk) ; tag it with "evaluated-thunk" @beginning
           (set-car! (cdr obj) result)  ; replace exp with its value (w/the forced eval)
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj) ;-->has been evaluated (no longer a thunk really)
         (thunk-value obj))
        (else obj)))
;----------------------------------------------------------


;----------------------------------------------------------

;; A longer list of primitives -- suitable for running everything in 4.2
;; Overrides the list in ch4-mceval.scm

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'newline newline)
        (list 'display display)
;;      more primitives
        ))

'LAZY-EVALUATOR-LOADED

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; Your answers to Problem 1, 2, and 3 after the code below.

;-----#1-------------------------------------------------------------------------------------
(define (mc-apply procedure arguments env)  
  ;if a primitive procedure, evaluate it (applicative--no delaying)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          ;attach a new frame onto this environment
          (extend-environment
           (extract-hybrid-params (procedure-parameters procedure))
           (list-of-hybrid-args (procedure-parameters procedure) arguments env) ; what should the parameters be??
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- HYBRIDIZED-APPLY" procedure))))

(define (extract-hybrid-params parameters)
  (cond ((null? parameters) '())
        ((delayed? (car parameters))
         (cons (cadr (car parameters)) (extract-hybrid-params (cdr parameters))))
        (else (cons (car parameters) (extract-hybrid-params (cdr parameters))))))

(define (list-of-hybrid-args proc-params args env)
  (cond ((null? args) '())
        ((delayed? (car proc-params))
         (cons (delay-it (car args) env)
               (list-of-hybrid-args (cdr proc-params) (cdr args) env)))
        (else (cons (actual-value (car args) env) 
                    (list-of-hybrid-args (cdr proc-params) (cdr args) env)))))

(define (delayed? obj)
  (tagged-list? obj 'delayed))   
   
;------#2--------------------------------------------------------------------
;(define (if predicate (delayed action) (delayed alternative))
;  (cond ((predicate action))
;        (else (alternative)))

;------#3--------------------------------------------------------------------

;uncomment procedures below and plug into interpreter
;(define (cons a (delayed b))
;  (lambda (m) (m a b)))
;(define (car z)
;  (z (lambda (p q) p)))
;(define (cdr z)
;  (z (lambda (p q) q)))

  

(define primitive-procedures
  (list (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'newline newline)
        (list 'display display)
;;      more primitives
        ))