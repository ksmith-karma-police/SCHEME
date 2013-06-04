;;@author Kristen Smith (ksmith22@brandeis.edu)
;;Assignment: PS03 -- Marry
;;@version 1.0
;;Date: Thursday 3/07/13

;; This is the code for -- Stable Marriage

;Edited to match courtship definition
(define (match-make proposers proposees)
  (send proposers 'reset)
  (send proposees 'reset)
  (courtship proposers proposers) ;(initially. all proposers are unengaged...)
  (zip-together (send proposers 'name)
                (send (send proposers 'intended) 'name)))

;(define (courtship unengaged-proposers proposers proposees) ... )

;(define (currently-unengaged list-of-people) ... )

;(define (send list-of-people message) ... )

;(define (couple? person1 person2) ...)

(define (zip-together list1 list2)
  (if (null? list1)
      '()
      (cons (list (car list1) (car list2))
            (zip-together (cdr list1) (cdr list2)))))

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (make-person my-name)
  (let ((preference-list '())
        (possible-mates '())
        (current-intended '()))
;        (divorcees '())) 
    (define (me message)
      (cond ((eq? message 'name) my-name)
            ((eq? message 'intended) current-intended)
            ((eq? message 'loves) preference-list)
            ((eq? message 'possible) possible-mates)
;            ((eq? message 'divorcee) divorcees)
;            ((eq? message 'dumped) (append (list me) divorcees)) ;adds "me" to the list of divorcees
            ((eq? message 'reset)
               (set! current-intended '())
               (set! possible-mates preference-list)
               'reset-done)
            ((eq? message 'load-preferences)
               (lambda (plist)
                  (set! preference-list plist)
                  (set! possible-mates plist)
                  (set! current-intended '())
                  'preferences-loaded))
            ((eq? message 'propose)
             (let ((beloved (car possible-mates)))  
               (begin (display (me 'name))
                      (display " is proposing to ")
                      (display (beloved  'name))
                      (newline)
                      (set! possible-mates (cdr possible-mates)))
                 (if (eq? ((beloved 'i-love-you) me)
                          'i-love-you-too)
                     (begin (set! current-intended beloved)
                            'we-are-engaged)
                     'no-one-loves-me)))                                  
            ((eq? message 'i-love-you)
             (lambda (proposer)
                ;if already engaged and likes proposer more than current intended...
               (cond ((and (not (null? (me 'intended))) 
                           (i-like-more? proposer (me 'intended)))
                      ;...dump the current intended and say YES to proposer
                      (begin (display (me 'name))
                             (display " has decided to change their mind.")
                             (newline)
                             (display (me 'name))
                             (display " is now going to marry ")
                             (display (proposer 'name))
                             (newline)                             
                             (((me 'intended) 'i-changed-my-mind) me)
;                             (display "The current husband/wife of ")
;                             (display (me 'name))
;                             (display " is ")
;                             (display (me 'intended))
;                             (newline)
                             (set! current-intended proposer)
                             (display "\"I love you too!\" ")
                             (display (me 'name))
                             (write-line " says.")
                             (newline)
                             'i-love-you-too))    
                      ;if already engaged and likes current intended more than proposer...
                     ((and (not (null? (me 'intended))) 
                           (i-like-more? (me 'intended) proposer))
                      ;..tell the proposer to buzz off! Creeper. Say NO.
                      (begin (display "\"Buzz off, creep!\"")
                             (display (me 'name))
                             (writeline " says. ")
                             'buzz-off-creep!)) 
                     ; Otherwise if not currently engaged, say YES (null? (me 'intended))
                     (else (begin (set! current-intended proposer)
                                  (display (me 'name))
                                  (display " is not engaged.")
                                  (newline)
                                  (display "\"I love you too!\" (s)he says.")
                                  (newline)
                                  (newline)
                                  'i-love-you-too)))))                                             
            ((eq? message 'i-changed-my-mind)
               (lambda (lost-love)
                  (cond ((eq? current-intended lost-love)
                            (set! current-intended '())
                            (begin (display "\"i-changed-my-mind, ")
                                   (display (me 'name))
                                   (display "\" (s)he says")
                                   (newline)
                                   (display (me 'name))
                                   (display " has been dumped.")
                                   (newline)
                                   (newline)
                                   ;(append (list me) divorcees)
                                   ;(me 'dumped)
                                   'dumped))
                        (else (begin (display "\"There must be some misunderstanding,\"")
                                     (display (me 'name))
                                     (display " says.")
                                     (newline)
                                     (display (me 'name))
                                     (display " is not married to ")
                                     (display (lost-love 'name))
                                     (newline)
                                     'there-must-be-some-misunderstanding)))))
            (else 
             (display "bad message to a person")
             (newline)
             (display (list me my-name message))
             (newline)
              (error ("Bad message to a person ") (list me my-name message)))))
             ;(write-line 'error)))
      me))

(define (error) (write-line))

;; This is a test file for -- Stable Marriage

(define alan (make-person 'Alan))
(define bob (make-person 'Bob))
(define charles (make-person 'Chuck))
(define david (make-person 'Dave))
(define ernest (make-person 'Ernie))
(define franklin (make-person 'Frank))
(define agnes (make-person 'Agnes))
(define bertha (make-person 'Bertha))
(define carol (make-person 'Carol))
(define deborah (make-person 'Debbie))
(define ellen (make-person 'Ellen))
(define francine (make-person 'Fran))

((alan 'load-preferences) 
   (list agnes carol francine bertha deborah ellen))
((bob 'load-preferences) 
   (list carol francine bertha deborah agnes ellen))
((charles 'load-preferences) 
   (list agnes francine carol deborah bertha ellen))
((david 'load-preferences) 
   (list francine ellen deborah agnes carol bertha))
((ernest 'load-preferences) 
   (list ellen carol francine agnes deborah bertha))
((franklin 'load-preferences) 
   (list ellen carol francine bertha agnes deborah))
((agnes 'load-preferences) 
   (list charles alan bob david ernest franklin))
((bertha 'load-preferences) 
   (list charles alan bob david ernest franklin))
((carol 'load-preferences) 
   (list franklin charles bob alan ernest david))
((deborah 'load-preferences) 
   (list bob alan charles franklin david ernest))
((ellen 'load-preferences) 
   (list franklin charles bob alan ernest david))
((francine 'load-preferences) 
   (list alan bob charles david franklin ernest))

(define men (list alan bob charles david ernest franklin))
(define women (list agnes bertha carol deborah ellen francine))

;;*************************************************************************************************
;Problem #0--In the description of the stable marriage algorithm, 
;we consider separately the cases of a person receiving one, or more than one, 
;proposal of marriage. No such “separate case” treatment is apparent in 
;the Scheme implementation. Explain why. --> see .pdf file


;Problem #1--
(define (send list-of-people message) 
  (cond ((null? list-of-people) '())
        (else ((car list-of-people) message)
              (send (cdr list-of-people) message))))

;Short(er) version of courtship. First clause: to make sure those who are dumped get back onto 
;the list of proposers so they may propose to anyone who is unengaged. Second clause: 
(define (courtship unengaged-proposers proposers)
  (if (null? unengaged-proposers)
      '()
      (begin((car unengaged-proposers) 'propose)
            (courtship (currently-unengaged? proposers) proposers))))

(define (couple? person1 person2)
  (if (and (eq? ((person1 'intended) 'name) person2)
           (eq? ((person2 'intended) 'name) person1))
      #t
      (#f)))

;Part of solution for currently-unengaged procedure
(define (predicate person)
    (if (null? (person 'intended)) 
        #t
        #f))         

;Note -- procedure must be used in the <me> method/procedure in order
;to make sure anyone who is dumped is removed from the unengaged-proposers list
(define (currently-unengaged? list-of-people)   
  (filter predicate list-of-people))
      

;free-standing i-like-more? procedure
(define (i-like-more? person1 person2)
  (define (preference-rank people-list)
    (cond ((null? people-list) '())
            ((eq? (car people-list) person1) #t)
            ((eq? (car people-list) person2) #f)
            (else (set! people-list (cdr people-list))
                  (preference-rank people-list))))
  (lambda (people-list)
    (preference-rank people-list)))
      
           
(define (write-line x)
  (display x)
  (newline))
              
  