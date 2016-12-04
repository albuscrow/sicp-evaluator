#lang racket
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))


(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure argements))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-evironment
           (procedure-paremeters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type: APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-squence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assigment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;; exercise 4.1
(define (list-of-value-lr exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env))
            (right (list-of-values (reset-operands exps) env)))
        (cons left right))))

(define (list-of-value-rl exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values (reset-operands exps) env))
            (left (eval (first-operand exps) env)))
        (cons left right))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define variable? symbol?)

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))


(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))

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
  (list 'if predicate consquent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq)
  (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))

(define (cond-predicate clause) (car clause))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-addition-clause? clause)
  (eq? (cadr clause) '=>))

(define (cond-actions clause) (cdr clause))
(define (cond-actions-additional clause) (cddr clause))
(define (cond->if exp) (expand-clause (cond-clause exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last: COND->IF"
                          clauses)))
              ;; exercise 4.5
              ((cond-addition-clause? first)
               (let ((predicate (cond-predicate first)))
                 (make-if (predicate)
                          (list (sequence->exp (cond-actions-additional first)) predicate)
                          (expand-clause rest))))
              (else
               (make-if (cond-predicate first)
                        (sequence->exp (cond-actions first))
                        (expand-clause rest)))))))

;; exercise 4.2
(define (application?-4.2 exp) (tagged-list? exp 'call))
(define (operator-4.2 exp) (cadr exp))
(define (operands-4.2 exp) (cddr exp))

;; exercise 4.3
(require "data-direct.scm")
(put 'eval 'quoted (lambda (exp env)
                     (text-of-quotation exp)))
(put 'eval 'set! eval-asignment)
(put 'eval 'define eval-definition)
(put 'eval 'if eval-if)
(put 'eval 'lambda (lambda (exp env)
                     (make-procedure (lambda-parameters exp)
                                     (lambda-body exp)
                                     env)))
(put 'eval 'begin (lambda (exp env)
                    (eval-sequence (begin-actions exp) env)))
(put 'eval 'cond (lambda (exp env)
                   (eval-4.3 (cond-if exp) env)))

(define (eval-4.3 exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get 'eval (car exp)) ((get 'eval (car exp)) exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

;; exercise 4.4
(define (and? exp) (tagged-list? exp 'and))

(define (eval-and exp env)
  (cond ((null? exp) (error "And's arguments number can not be zero: EVAL-AND " exp))
        ((last-exp? exp) (eval (first-exp exp) env))
        (else
         (let ((b (eval (first-exp exp) env)))
           (if b
               (eval-and (rest-exps exp) env)
               'false)))))

(define (or? exp) (tagged-list? exp 'or))

(define (eval-or exp env)
  (cond ((null? exp) (error "Or's arguments number can not be zero: EVAL-AND " exp))
        ((last-exp? exp) (eval (first-exp exp) env))
        (else
         (let ((b (eval (first-exp exp) env)))
           (if b
               'true
               (eval-or (rest-exps exp) env))))))

;; exercise 4.6
(define (let? exp)
  (tagged-list? exp 'let))
(define let-variables cadr)
(define let-body cddr)
(define (let-variables-> fun)
  (define (helper variables)
    (if (null? variables)
        '()
        (cons (fun (car variables)) (helper (cdr variables)))))
  helper)
(define let-variables->parameters (let-variables-> car))
(define let-variables->argument (let-variables-> cadr))
(define (let->combination exp)
  `(apply ,(make-lambda (let-variables->parameters (let-variables exp))
                 ,(let-body exp))
    (let-variables->argument (let-variables exp))))
(define (make-let variables body)
  (list 'let variables body))

;; exercise 4.7
(define (let*? exp)
  (tagged-list? exp 'let*))

(define (expand-let* variables body)
  (if (null? variables)
      body
      (make-let (list (car variables))
                (expand-let* (cdr variables) body))))

(define (let*->nested-lets exp)
  (expand-let* (let-variables exp) (let-body exp)))

;; exercise 4.8

(define (make-function function-name function-parameters body)
  (list 'define (cons function-name function-parameters) body))



(define (named-let? exp)
  (variable? (cadr exp)))

(define named-let-name cadr)
(define named-let-variables caddr)
(define named-let-body cdddr)

(define (let->combination-4.8 exp)
  (if (named-let? exp)
      (make-begin (list (make-function
                         (named-let-name exp)
                         (let-variables->parameters (named-let-variables exp))
                         (named-let-body exp))
                        (list 'apply
                              (named-let-name exp)
                              (let-variables->arguments (named-let-variables exp)))))
      `(apply ,(make-lambda (let-variables->parameters (let-variables exp))
                            ,(let-body exp))
              (let-variables->arguments (let-variables exp)))))


;; exercise 4.9
;; usage (for ((a list-of-a) (b list-of-b) <body>)
(define for-variables let-variables)
(define for-body let-body)
(define for-variables->parameters let-variables->parameters)
(define for-variables->arguments let-variables->arguments)
(define (for->exp exp)
  (let helper ((arguments (for-variables->arguments (for-variables exp)))
               (parameters (for-variables->parameters (for-variables exp))))
    (list 'apply
          (make-lambda arguments
                       `(if (apply and ,(map (lambda (x) '(null? x)) arguments))
                            (cons (apply
                                   (lambda ,parameters ,(for-body exp))
                                   (map (lambda (x) '(car x)) arguments))
                                  (apply helper (map (lambda (x) '(cdr x)) arguments))) 
                            '()))
          arguments)))



(define (true? x)
(not (eq? x false)))
(define (false? x) (eq? x false))


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
  (set-cdr! frame (cons val (cdr frame))))\

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-look env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-lop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-enviroment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;; p515