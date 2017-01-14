#lang racket
; import modifiable pair
(require scheme/mpair)
(define (list->mlist lst)
  (if (null? lst)
      '()
      (mcons (car lst) (list->mlist (cdr lst)))))
;(define cons mcons)
;(define car mcar)
;(define cdr mcdr)
;(define list mlist)
;(define set-car! set-mcar!)
;(define set-cdr! set-mcdr!)

;eval and apply

(define (ac-output . param)
  (map (lambda (p) (display p)) param)
  'ok)

(define (ac-apply procedure arguments)
  (ac-output "INFO ac-apply: " procedure " " arguments "\n\n")
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
         (error "Unknown procedure type: APPLY" procedure))))

(define (eval exp env)
  (ac-output "INFO eval exp: " exp "; type: ")
  (define res
    (cond ((self-evaluating? exp) (ac-output "self-evaluating\n") exp)
          ((variable? exp) (ac-output "variable\n") (lookup-variable-value exp env))
          ((quoted? exp) (ac-output "quoted\n") (text-of-quotation exp))
          ((assignment? exp) (ac-output "assignment!\n") (eval-assignment exp env))
          ((make-unbound!? exp) (eval-unbound exp env))
          ((definition? exp) (ac-output "definition\n") (eval-definition exp env))
          ((if? exp) (ac-output "if\n") (eval-if exp env))
          ((and? exp) (eval-and exp env))
          ((or? exp) (eval-or exp env))
          ((lambda? exp) (ac-output "lambda\n") (make-procedure (lambda-parameters exp)
                                                                (lambda-body exp)
                                                                env))
          ((let? exp) (ac-output "let\n") (eval (let->combination exp) env))
          ((let*? exp) (ac-output "let*\n") (eval (let*->nested-lets exp) env))
          ((letrec? exp) (ac-output "letrec\n") (eval (letrec->nested-lets exp) env))
          ((for? exp) (ac-output "for\n") (eval (for->exp exp) env))
          ((begin? exp) (ac-output "begin\n")
                        (eval-sequence (begin-actions exp) env))
          ((cond? exp) (ac-output "cond\n") (eval (cond->if exp) env))
          ((application? exp) (ac-output "application\n")
                              (ac-apply (eval (operator exp) env)
                                        (list-of-values (operands exp) env)))
          (else
           (error "Unknown expression type: EVAL" exp))))
  (ac-output "INFO eval exp: " exp " res: " res "\n")
  res)


  
  
;eval sequence
(define (eval-sequence exps env)
  (define (eval-sequence-recusion remain-exps val)
    (if (null? remain-exps)
        val
        (eval-sequence-recusion
         (rest-exps remain-exps)
         (eval (first-exp remain-exps) env))))
  (eval-sequence-recusion exps '()))


;some util
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #false))

;define begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (make-begin seq)
  (cons 'begin seq))

;define expression
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

;define application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
;; exercise 4.2
(define (application?-4.2 exp) (tagged-list? exp 'call))
(define (operator-4.2 exp) (cadr exp))
(define (operands-4.2 exp) (cddr exp))
;; for operands
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
;; eval operands
(define (list-of-values operands env)
  (display "INFO list-of-values: ")
  (display operands)
  (newline)
  (newline)
  (map (lambda (operand)
         (eval operand env))
       operands))
;; exercise 4.1
(define (list-of-value-lr exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env))
            (right (list-of-values (rest-operands exps) env)))
        (cons left right))))
(define (list-of-value-rl exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values (rest-operands exps) env))
            (left (eval (first-operand exps) env)))
        (cons left right))))

;define quoted?
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

;; exercise 4.4 define "and" and "or"
(define (and? exp) (tagged-list? exp 'and))
(define (eval-and exps env)
  (define (eval-and-recusion remain-exps result)
    (if (or (null? remain-exps) (not result))
        result
        (eval-and-recusion
         (rest-exps remain-exps)
         (eval (first-exp remain-exps) env))))
  (eval-and-recusion (cdr exps) #t))

(define (or? exp) (tagged-list? exp 'or))
(define (eval-or exp env)
  (define (eval-or-recusion remain-exps result)
    (if (or (null? remain-exps) result)
        result
        (eval-or-recusion (rest-exps remain-exps) (eval (first-exp remain-exps) env))))
  (eval-or-recusion (cdr exp) #f))

;define if
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (let ((alternative-clause (cdddr exp)))
    (if (null? alternative-clause)
        'flase
        (car alternative-clause))))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;define cond by expanding it to if expression
;;for cond
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
;;for cond clause
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
;;e4.5 usage:
;> (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;        (else false))
;> 2
(define (cond-addition-clause? clause)
  (eq? (cadr clause) '=>))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-actions-additional clause) (cddr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first-clause (car clauses))
            (rest-clauses (cdr clauses)))
        (cond ((cond-else-clause? first-clause)
               (if (null? rest-clauses)
                   (sequence->exp (cond-actions first-clause))
                   (error "ELSE clause isn't last: COND->IF" clauses)))
              ;; exercise 4.5
              ((cond-addition-clause? first-clause)
               (let ((predicate (cond-predicate first-clause)))
                 (make-if predicate
                          (list (sequence->exp (cond-actions-additional first-clause)) predicate)
                          (expand-clauses rest-clauses))))
              (else
               (make-if (cond-predicate first-clause)
                        (sequence->exp (cond-actions first-clause))
                        (expand-clauses rest-clauses)))))))

;define self evaluating exp
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;define variable
(define variable? symbol?)

;define lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; exercise 4.3
;(require "data-direct.scm")
;(put 'eval 'quoted (lambda (exp env)
;                     (text-of-quotation exp)))
;(put 'eval 'set! eval-assignment)
;(put 'eval 'define eval-definition)
;(put 'eval 'if eval-if)
;(put 'eval 'lambda (lambda (exp env)
;                     (make-procedure (lambda-parameters exp)
;                                     (lambda-body exp)
;                                     env)))
;(put 'eval 'begin (lambda (exp env)
;                    (eval-sequence (begin-actions exp) env)))
;(put 'eval 'cond (lambda (exp env)
;                   (eval-4.3 (cond->if exp) env)))
;(define (eval-4.3 exp env)
;  (cond ((self-evaluating? exp) exp)
;        ((variable? exp) (lookup-variable-value exp env))
;        ((get 'eval (car exp)) ((get 'eval (car exp)) exp env))
;        ((application? exp)
;         (ac-apply (eval (operator exp) env)
;                (list-of-values (operands exp) env)))
;        (else
;         (error "Unknown expression type: EVAL" exp))))


;define let, let* and named-let
;;exercise 4.6 let
(define (let? exp)
  (tagged-list? exp 'let))
(define let-variables cadr)
(define let-body cddr)
(define (let-variables->parameters variables)
  (map car variables))
(define (let-variables->arguments variables)
  (map cadr variables))
;(define (let->combination exp)
;  (list (make-lambda (let-variables->parameters (let-variables exp))
;                     (let-body exp))
;    (let-variables->argument (let-variables exp))))
(define (make-let variables body)
  (cons 'let (cons variables body)))
;; exercise 4.7 let*
(define (let*? exp)
  (tagged-list? exp 'let*))
(define (expand-let* variables body)
  (if (null? variables)
      body
      (list (make-let (list (car variables))
                (expand-let* (cdr variables) body)))))
(define (let*->nested-lets exp)
  (car (expand-let* (let-variables exp) (let-body exp))))


;;define letrec
(define (letrec? exp)
  (tagged-list? exp 'letrec))
(define (letrec->nested-lets exp)
  (define (expand-letrec variables body)
    (make-let
     (map (lambda (variable) (list (car variable) ''*unassigned*)) variables)
     (append
      (map (lambda (variable) (cons 'set! variable)) variables)
      body)))
  (expand-letrec (let-variables exp) (let-body exp)))


;; exercise 4.8
(define (make-function function-name function-parameters body)
  (list 'define (cons function-name function-parameters) body))
(define (named-let? exp)
  (variable? (cadr exp)))
(define named-let-name cadr)
(define named-let-variables caddr)
(define named-let-body cdddr)
(define (let->combination exp)
  (if (named-let? exp)
      (make-begin
       (list (make-function
              (named-let-name exp)
              (let-variables->parameters (named-let-variables exp))
              (sequence->exp (named-let-body exp)))
             (cons
              (named-let-name exp)
              (let-variables->arguments (named-let-variables exp)))))
      (cons (make-lambda (let-variables->parameters (let-variables exp))
                         (let-body exp))
            (let-variables->arguments (let-variables exp)))))

;define for, exercise 4.9
;usage (for ((a list-of-a) (b list-of-b)) <body>)
(define (for? exp) (tagged-list? exp 'for))
(define for-variables let-variables)
(define for-body let-body)
(define for-variables->parameters let-variables->parameters)
(define for-variables->arguments let-variables->arguments)
(define (for->exp exp)
  (let* ((parameters (for-variables->parameters (for-variables exp)))
        (arguments (for-variables->arguments (for-variables exp)))
        (intern-parameters (map (lambda (x) (gensym)) parameters))
        (body (for-body exp)))
    `(begin
       (define ,(cons 'for-loop intern-parameters)
           (if ,(cons 'or (map (lambda (x) (list 'null? x)) intern-parameters))
               'ok
               (begin
                 ,(cons
                   (cons 'lambda (cons parameters body))
                   (map (lambda (x) (list 'car x)) intern-parameters))
                 ,(cons 'for-loop (map (lambda (x) (list 'cdr x)) intern-parameters)))))
       ,(cons 'for-loop arguments))))

;define true and false
(define (true? x)
  (not (eq? x false)))
(define (false? x) (eq? x false))

;define procedure
;;e4.16
(define (split-body exps)
  (if (null? exps)
      (cons '() '())
      (let* ((rest (split-body (cdr exps)))
             (defines (car rest))
             (body (cdr rest))
             (exp (car exps)))
        (if (definition? exp)
            (cons (cons exp defines) body)
            (cons defines (cons exp body))))))

(define (scan-out-defines body)
  (let* ((splited-body (split-body body))
         (defines (car splited-body))
         (new-body (cdr splited-body)))
    (append defines new-body)))
  

(define (scan-out-defines-4.16 body)
  (let* ((splited-body (split-body body))
         (defines (car splited-body))
         (new-body (cdr splited-body)))
    (if (null? defines)
        body
        (list (make-let
         (map
          (lambda (definition)
            (list
             (definition-variable definition)
             ''*unassigned*))
          defines)
         (append
          (map
           (lambda (definition)
             (list
              'set!
              (definition-variable definition)
              (definition-value definition)))
           defines)
          new-body))))))
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;define env
(define (enclosing-environment env) (mcdr env))
(define (first-frame env) (mcar env))
(define (rest-frames env) (mcdr env))
(define the-empty-environment (mlist))
;;define frame e4.11
(define (make-frame variables values)
  (mcons 'frame (mmap (lambda (variable value) (mcons variable value)) variables values)))
(define (add-binding-to-frame! var val frame)
  (set-mcdr! frame (mcons (mcons var val) (frame-bindings frame))))
(define (frame-bindings frame)
  (mcdr frame))
;;define pair
(define (first-pair pairs)
  (mcar pairs))
(define (rest-pairs pairs)
  (mcdr pairs))
(define (pair-variable pair)
  (mcar pair))
(define (value pair)
  (mcdr pair))
(define (set-value! pair val)
  (set-mcdr! pair val))
(define (set-first-pair! pairs pair)
  (set-mcar! pairs pair))
(define (delete-reset-pairs! pairs)
  (set-mcdr! pairs '()))

(define (display-mlist pairs)
  (if (null? pairs)
      (newline)
      (begin
        (print (first-pair pairs))
        (newline)
        (display-mlist (rest-pairs pairs)))))

(define (extend-environment vars vals base-env)
  (let ((vars-mlist (list->mlist vars))
        (vals-mlist (list->mlist vals))
        (vars-length (length vars))
        (vals-length (length vals)))
    (if (= vars-length vals-length)
        (mcons (make-frame vars-mlist vals-mlist) base-env)
        (if (< vars-length vals-length)
            (error "Too many arguments supplied" vars vals)
            (error "Too few arguments supplied" vars vals)))))

;;e4.12
(define (traversing-variables env variable if-find if-not-find)
  (define (env-loop current-env)
    (define (scan pairs)
      (cond ((null? pairs)
             (env-loop (enclosing-environment current-env)))
            ((eq? variable (pair-variable (first-pair pairs))) (if-find pairs))
            (else (scan (rest-pairs pairs)))))
    (if (eq? current-env the-empty-environment)
        (if-not-find env)
        (scan (frame-bindings (first-frame current-env)))))
  (env-loop env))


(define (set-variable-value! var val env)
  (define (set-first-pairs-val! pairs)
    (set-value! (first-pair pairs) val))
  (traversing-variables
   env var set-first-pairs-val!
   (lambda (env) (error "Unbound variable: SET!" var))))

(define (define-variable! var val env)
    (define (set-first-pairs-val! pairs)
      (set-value! (first-pair pairs) val))
  (traversing-variables
   (mlist (first-frame env)) var set-first-pairs-val!
   (lambda (env)
     (add-binding-to-frame!
      var val (first-frame env)))))

;;modified by e4.16
(define (lookup-variable-value var env)
  (let ((val
         (traversing-variables
          env var
          (lambda (pairs) (value (first-pair pairs)))
          (lambda (env) (error "Unbound variable" var)))))
    (if (eq? val '*unassigned*)
        (error "unassigned" var)
        val)))


;define assignment
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

;define definition
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))
(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'add-definition-ok)

;define unbound e4.13 only find in local env
(define (make-unbound!? exp)
  (tagged-list? exp 'make-unbound!))

(define (make-unbound!-variable exp)
  (cadr exp))

(define (unbound-variable! var env)
  (traversing-variables
   (mlist (first-frame env)) var
   (lambda (pairs)
     (define (helper dst-pairs src-pairs)
       (set-first-pair! dst-pairs (first-pair src-pairs))
       (if (null? (rest-pairs src-pairs))
           (delete-reset-pairs! dst-pairs)
           (helper (rest-pairs dst-pairs) (rest-pairs src-pairs))))
     (helper pairs (rest-pairs pairs)))
   (lambda (env)
     (error "Unbound variable" var))))

(define (eval-unbound exp env)
  (unbound-variable! (make-unbound!-variable exp) env))


;define primitive-procedure
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'list list)
        (list 'null? null?)
        (list 'display display)
        (list '+ +)
        (list '* *)
        (list '- -)
        (list '/ /)
        (list '= =)))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define (apply-primitive-procedure proc args)
  (apply
   (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
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

(define (run exp)
  (eval exp the-global-environment))

;test
;;test define fun, call fun, if, quate list 
(eval '(define (append x y)
         (if (null? x)
             y
             (cons (car x) (append (cdr x) y)))) the-global-environment)

(define res (run '(append '(a b c) '(d e f))))
(if (equal? res '(a b c d e f))
    (ac-output "test define ok\n")
    (error "test define error\n"))

;;test quote
(if (eq? (run ''a) 'a)
    (ac-output "test quote ok\n")
    (error "test quote error\n"))

;;test define variable and begin
(if (equal? (run '(begin (define x 1) x)) 1)
    (ac-output "test define and begin ok\n")
    (error "test define and begin error\n"))

;;test cond and let
(if (and (equal? (run '(let ((a 1))
                         (cond ((= a 0) 'a)
                               ((= 1 a) (+ a 1))
                               (else 'else)))) 2)
         (equal? (run '(let ((a 10))
                         (cond ((= a 0) 'a)
                               ((= 1 a) (+ a 1))
                               (else a)))) 10)
         (equal? (run '(let ((a 10))
                         (cond ((= a 0) 'a)
                               ((+ 10 1) => (lambda (a) (* a 2)))
                               (else a)))) 22))
    (ac-output "test let and cond ok\n")
    (error "test let and cond error\n"))

;;test and or
(if (and (equal? 1 (run '(and 3 2 1)))
         (equal? (run '(and 3 false 1)) #f)
         (equal? (run '(and true)) #t)
         (equal? (run '(and false)) #f)
         (equal? (run '(or 1 false false)) 1)
         (equal? (run '(or false)) #f)
         (equal? (run '(or 1)) 1)
         (equal? (run '(or false false false)) #f)
         (equal? (run '(or false false 10)) 10))
    (ac-output "test and or ok")
    (error "test and or error"))

;;test let*
(if (equal? 3 (run '(let* ((a 1) (b (+ a 2))) b)))
    (ac-output "test let* ok")
    (error "test let* error"))

;;test named let*
(if (equal? 55 (run '(let add ((a 10))
                       (if (= a 0)
                           0
                           (+ a (add (- a 1)))))))
    (ac-output "test named let ok")
    (error "test named let error"))

;;test set!
(if (equal? 4 (run '(begin (define a 10) (set! a 4) a)))
    (ac-output "test set! ok")
    (error "test set!" error))

;;test for
(if (equal? 'ok (run '(for ((a (list 1 2 3 10)) (b (list 1 2 3 10)))
                       (display (+ a b))
                       (+ a b))))
    (ac-output "test for ok")
    (error "test for error"))

;;test map e4.14
(run '(define (map fun list)
        (if (null? list)
            '()
            (cons (fun (car list)) (map fun (cdr list))))))
(run '(map (lambda (x) (+ x x)) '(1 2 3)))

(run '(define (factorial n)
        (if (= n 1) 1  (* n (factorial (- n 1))))))
(run '(factorial 6))


;;test unbound
;(run '(begin (define a 10) (make-unbound! a) a))

(run '(define (test input)
        (if (even? input)
            (+ a b)
            (* a b))
        (define (even? n) (if (= n 0) true (odd? (- n 1))))
        (define (odd? n) (if (= n 0) false (even? (- n 1))))
        (define a 10)
        (define b 20)))

(if (and (equal? 200 (run '(test 21))) (equal? 30 (run '(test 20))))
    (ac-output "test internal define ok")
    (error "test internal define error"))
    
;;test letrec
(if (equal? 120 (run '(letrec
                          ((fact (lambda (n)
                                   (if (= n 1) 1 (* n (fact (- n 1)))))))
                        (fact 5))))
    (ac-output "test letrec ok")
    (error "test letrec error"))


;run
;(driver-loop)