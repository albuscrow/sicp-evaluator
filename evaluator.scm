#lang racket
; import modifiable pair
(require scheme/mpair)
(define (list->mlist lst)
  (if (null? lst)
      '()
      (mcons (car lst) (list->mlist (cdr lst)))))

;eval and apply
;;normal
(define (ac-apply-before4.1.7 procedure arguments)
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

(define (eval-before4.1.7 exp env)
  (define res
    (cond ((self-evaluating? exp) exp)
          ((variable? exp) (lookup-variable-value exp env))
          ((quoted? exp) (text-of-quotation exp env))
          ((assignment? exp) (eval-assignment exp env))
          ((make-unbound!? exp) (eval-unbound exp env))
          ((definition? exp) (eval-definition exp env))
          ((if? exp) (eval-if exp env))
          ((unless? exp) ((analyze (unless->if exp)) env))
          ((and? exp) (eval-and exp env))
          ((or? exp) (eval-or exp env))
          ((lambda? exp) (make-procedure (lambda-parameters exp)
                                                                (lambda-body exp)
                                                                env))
          ((let? exp) (eval-before4.1.7 (let->combination exp) env))
          ((let*? exp) (eval-before4.1.7 (let*->nested-lets exp) env))
          ((letrec? exp) (eval-before4.1.7 (letrec->nested-lets exp) env))
          ((for? exp) (eval-before4.1.7 (for->exp exp) env))
          ((begin? exp) (eval-sequence (begin-actions exp) env))
          ((cond? exp) (eval-before4.1.7 (cond->if exp) env))
          ((application? exp) (ac-apply-before4.1.7 (eval-before4.1.7 (operator exp) env)
                                        (list-of-values (operands exp) env)))
          (else
           (error "Unknown expression type: EVAL" exp))))
  (ac-output "eval-before4.1.7:\n" exp "\n" res "\n")
  res)

;;analyze
(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((unless? exp) (analyze (unless->if exp)))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((let*? exp) (analyze (let*->nested-lets exp)))
        ((letrec? exp) (analyze (letrec->nested-lets exp)))
        ((for? exp) (analyze (for->exp exp)))
        ((and? exp) (analyze-and exp))
        ((or? exp) (analyze-or exp))
        ((application? exp) (analyze-application exp))
        (else (error "Unknow expression type: ANALYZE" exp))))

(define (eval-analyze exp env)
  ((analyze exp) env))
  
;;lazy
(define (ac-apply-lazy procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args
            (procedure-parameters-usage procedure) arguments env)
           (procedure-environment procedure))))
        (else (error "Unknow procedure type: APPLY" procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps)
                          env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args usages exps env)
  (if (no-operands? exps)
      '()
      (let ((usage (car usages))
            (arg (first-operand exps)))
        (cons (if (eq? usage 'normal)
                  (actual-value arg env)
                  (delay-it (first-operand exps) env usage))
              (list-of-delayed-args (cdr usages) (rest-operands exps)
                                    env)))))

(define (delay-it exp env usage)
  (mlist 'thunk exp env usage))

;;define thunk
(define (thunk? obj)
  (if (mpair? obj)
      (eq? (mcar obj) 'thunk)
      #f))
(define (thunk-exp exp)
  (mcar (mcdr exp)))
(define (thunk-env exp)
  (mcar (mcdr (mcdr exp))))
(define (evaluate-thunk thunk)
  (let ((result (actual-value (thunk-exp thunk)
                              (thunk-env thunk))))
    (if (eq? (mcar (mcdr (mcdr (mcdr thunk)))) 'lazy-memo)
        (begin 
          (set-mcar! thunk 'evaluated-thunk)
          (set-mcar! (mcdr thunk) result)
          (set-mcar! (mcdr (mcdr thunk)) '())
          )
        'not-memo)
    result))
(define (evaluated-thunk? obj)
  (if (mpair? obj)
      (eq? 'evaluated-thunk (mcar obj))
      #f))
(define (thunk-value evaluated-thunk)
  (mcar (mcdr evaluated-thunk)))
(define (force-it obj)
  (cond ((thunk? obj)
         (evaluate-thunk obj))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (eval-lazy exp env)
  (define res
    (cond ((self-evaluating? exp) exp)
          ((variable? exp) (lookup-variable-value exp env))
          ((quoted? exp) (text-of-quotation exp env))
          ((assignment? exp) (eval-assignment exp env))
          ((make-unbound!? exp) (eval-unbound exp env))
          ((definition? exp) (eval-definition exp env))
          ((if? exp) (eval-if-lazy exp env))
          ((unless? exp) ((analyze (unless->if exp)) env))
          ((and? exp) (eval-and exp env))
          ((or? exp) (eval-or exp env))
          ((lambda? exp) (make-procedure (lambda-parameters exp)
                                                                (lambda-body exp)
                                                                env))
          ((let? exp) (eval-lazy (let->combination exp) env))
          ((let*? exp) (eval-lazy (let*->nested-lets exp) env))
          ((letrec? exp) (eval-lazy (letrec->nested-lets exp) env))
          ((for? exp)  (eval-lazy (for->exp exp) env))
          ((begin? exp) (eval-sequence (begin-actions exp) env))
          ((cond? exp) (eval-lazy (cond->if exp) env))
          ((application? exp) (ac-apply-lazy (actual-value (operator exp) env)
                                        (operands exp)
                                        env))
          (else
           (error "Unknown expression type: EVAL" exp))))
  ;(ac-output "eval-lazy:\n" exp "\n" res "\n") 
  res)

(define (actual-value exp env)
  (force-it (eval-lazy exp env)))





;;;
(define ac-eval eval-lazy)

;some util
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #false))
(define (ac-output . param)
  (map (lambda (p)
         (if (tagged-list? p 'procedure)
             (ac-output "(procedure " (cadr p) (caddr p) " )\n")
             (display p))) param)
  'ok)
  
;eval sequence
(define (eval-sequence exps env)
  (define (eval-sequence-recusion remain-exps val)
    (if (null? remain-exps)
        val
        (eval-sequence-recusion
         (rest-exps remain-exps)
         (ac-eval (first-exp remain-exps) env))))
  (eval-sequence-recusion exps '()))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence: ANALYZE")
        (loop (car procs) (cdr procs)))))

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
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
       (fproc env)
       (map (lambda (aproc) (aproc env))
            aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))))
        (else
         (error "Unknown procedure type: EXECUTE-APPLICATION" proc))))
                                  
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
  (map (lambda (operand)
         (ac-eval operand env))
       operands))
;; exercise 4.1
(define (list-of-value-lr exps env)
  (if (no-operands? exps)
      '()
      (let ((left (ac-eval (first-operand exps) env))
            (right (list-of-values (rest-operands exps) env)))
        (cons left right))))
(define (list-of-value-rl exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values (rest-operands exps) env))
            (left (ac-eval (first-operand exps) env)))
        (cons left right))))

;define quoted?
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp env)
  (let ((text (cadr exp)))
    (define (helper text)
            (if (null? text)
                '(list)
                (list 'cons  (list 'quote (car text)) (helper (cdr text)))))
    (if (or (self-evaluating? text)(symbol? text))
        text
        (ac-eval (helper text) env))))
(define (text-of-quotation-normal exp)
  (cadr exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation-normal exp)))
    (lambda (env) qval)))

;; exercise 4.4 define "and" and "or"
(define (and? exp) (tagged-list? exp 'and))
(define (eval-and exps env)
  (define (eval-and-recusion remain-exps result)
    (if (or (null? remain-exps) (not result))
        result
        (eval-and-recusion
         (rest-exps remain-exps)
         (ac-eval (first-exp remain-exps) env))))
  (eval-and-recusion (cdr exps) true))

(define (analyze-and exp)
  (if (null? (cdr exp))
      (lambda (env) #t)
      (let ((procs (map analyze (cdr exp))))
        (define (sequentially proc1 proc2)
          (lambda (env)
            (let ((res (proc1 env)))
              (if res
                  (proc2 env)
                  res))))
        (define (loop procs)
          (let ((first-proc (car procs))
                (rest-procs (cdr procs)))
            (if (null? rest-procs)
                (lambda (env) (first-proc env))
                (sequentially first-proc (loop rest-procs)))))
        (loop procs))))
        
        

(define (or? exp) (tagged-list? exp 'or))
(define (eval-or exp env)
  (define (eval-or-recusion remain-exps result)
    (if (or (null? remain-exps) result)
        result
        (eval-or-recusion (rest-exps remain-exps) (ac-eval (first-exp remain-exps) env))))
  (eval-or-recusion (cdr exp) false))

(define (analyze-or exp)
  (if (null? (cdr exp))
      (lambda (env) #f)
      (let ((procs (map analyze (cdr exp))))
        (define (sequentially proc1 proc2)
          (lambda (env)
            (let ((res (proc1 env)))
              (if res
                  res
                  (proc2 env)))))
        (define (loop procs)
          (let ((first-proc (car procs))
                (rest-procs (cdr procs)))
            (if (null? rest-procs)
                (lambda (env) (first-proc env))
                (sequentially first-proc (loop rest-procs)))))
        (loop procs))))

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
  (ac-output "eval-if\n" exp "\n") 
  (if (true? (ac-eval (if-predicate exp) env))
      (begin
        (ac-output (if-consequent exp) "\n")
        (ac-eval (if-consequent exp) env))
      (ac-eval (if-alternative exp) env)))

(define (eval-if-lazy exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval-lazy (if-consequent exp) env)
      (eval-lazy (if-alternative exp) env)))

(define (unless? exp)
  (tagged-list? exp 'unless))
(define (unless->if exp)
  (list 'if (cadr exp) (cadddr exp) (caddr exp)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env) (if (true? (pproc env))
                      (cproc env)
                      (aproc env)))))



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
(define (analyze-self-evaluating exp)
  (lambda (env) exp))

;define variable
(define variable? symbol?)
(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

;define lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp)
  (map (lambda (var) (if (pair? var) var (cons var '(lazy-memo)))) (cadr exp)))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters (scan-out-defines body))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (scan-out-defines (lambda-body exp)))))
    (lambda (env) (make-procedure vars bproc env))))


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
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
  
(define (procedure-parameters p)
  (map (lambda (var) (car var)) (cadr p)))

(define (procedure-parameters-with-usage p)
  (cadr p))

(define (procedure-parameters-usage p)
  (map (lambda (var) (cadr var)) (cadr p)))

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
                       (ac-eval (assignment-value exp) env)
                       env)
  'ok)

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'assignment-ok)))

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
    (ac-eval (definition-value exp) env)
    env)
  'add-definition-ok)

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

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
    (let ((output (ac-eval input the-global-environment)))
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

(define input-prompt-lazy
  ";;; L-Eval input:")
(define output-prompt-lazy ";;; L-Eval value:")
(define (driver-loop-lazy)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value
            input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (run exp)
  (if (equal? ac-eval eval-lazy)
      (actual-value exp the-global-environment)
      (ac-eval exp the-global-environment)))

(define (run-sequence exps)
  (if (null? exps)
      'run-sequence-ok
      (begin
        (display (run (car exps)))
        (newline)
        (run-sequence (cdr exps)))))

(define (disp-cons obj depth)
  (letrec ((user-car (lambda (z)
                       (force-it (lookup-variable-value 'x (procedure-environment (cdr z))))))
           (user-cdr (lambda (z)
                       (force-it (lookup-variable-value 'y (procedure-environment (cdr z)))))))
    (cond
      ((>= depth 10)
       (display "...)"))
      ((null? obj)
       (display ""))
      (else
       (let ((cdr-value (user-cdr obj)))
         (display "(")
         (display (user-car obj))
         (if (tagged-list? cdr-value 'cons)
             (begin
               (display " ")
               (disp-cons cdr-value (+ depth 1)))
             (begin
               (display ".")
               (display cdr-value)))
         (display ")"))))))
     
                             

(run '(define (display-cons cons)
        (if (null? cons)
            (display "\n")
            (begin
              (display (car cons))
              (display " ")
              (display-cons (cdr cons))))))

(if (equal? ac-eval eval-lazy)
    (run-sequence '((define ori-cons cons)
                    (define ori-car car)
                    (define ori-cdr cdr)
                    (define (cons x y) (ori-cons 'cons (lambda (m) (m x y))))
                    (define (car z) ((ori-cdr z) (lambda (p q) p)))
                    (define (cdr z) ((ori-cdr z) (lambda (p q) q)))
                    ))
    'not-define-own-cons) 
                
(run '(define (append x y)
                  (if (null? x)
                      y
                      (cons (car x) (append (cdr x) y)))))
;test
;;test define fun, call fun, if, quote list
(if (equal? eval-lazy ac-eval) 
    (if (equal? (run '(car (cdr (cdr (append '(1) '(2 3)))))) 3)
        (ac-output "test define fun, if, quote ok\n")
        (error "test test define fun, if, quote error\n"))
    (if (equal? (run '(car (cdr (cdr (append (list 1) (list 2 3)))))) 3)
        (ac-output "test define fun, if, quote ok\n")
        (error "test test define fun, if, quote error\n")))


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

;test and or
(ac-output "temp temp" (run '(and 3 2 1)))
(if (and (equal? 1 (run '(and 3 2 1)))
         (equal? (run '(and 3 false 1)) #f)
         (equal? (run '(and true)) #t)
         (equal? (run '(and false)) #f)
         (equal? (run '(or 1 false false)) 1)
         (equal? (run '(or false)) #f)
         (equal? (run '(or 1)) 1)
         (equal? (run '(or false false false)) #f)
         (equal? (run '(or false false 10)) 10)
         )
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
(if (equal? 'ok (run '(for ((a '(1 2 3 10)) (b '(1 2 3 10)))
                       (display (+ a b))
                       (+ a b))))
    (ac-output "test for ok")
    (error "test for error"))

;test map e4.14
(run '(define (map fun lst)
        (if (null? lst)
            (list)
            (cons (fun (car lst)) (map fun (cdr lst))))))
(disp-cons (run '(map (lambda (x) (+ x x)) '(1 2 3))) 0)

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

;;test unless
(if (and (equal? 2 (run '(unless true (/ 1 0) 2)))
         (equal? 1 (run '(unless false 1 (/ 1 0)))))
    (ac-output "test unless ok")
    (error "test unless error!"))

;;e4.27 test lazy
(run '(define count 0))
(run '(define (id x) (set! count (+ count 1)) x))
(run '(define w (id (id 10))))
(run 'count)
(run 'w)
(run 'count)

;;e4.29
(run '(define (square (x lazy)) (* x x)))
(run '(square (id 10)))
(run 'count)

;;e4.30

(run '(define (p1 x)
        (set! x (cons x '(2)))
        x))
(run '(define (p2 x)
        (define (p (e lazy))
          e
          x)
        (p (set! x (cons x '(2))))))
(run '(p1 1))
(run '(p2 1))


;begin stream
(if (equal? ac-eval eval-lazy)
    (run-sequence '((define (list-ref items n)
                      (if (= n 0)
                          (car items)
                          (list-ref (cdr items) (- n 1))))
                    (define (map proc items)
                      (if (null? items)
                          '()
                          (cons (proc (car items)) (map proc (cdr items)))))
                    (define (scale-list items factor)
                      (map (lambda (x) (* x factor)) items))
                    (define (add-lists list1 list2)
                      (cond ((null? list1) list2)
                            ((null? list2) list1)
                            (else (cons (+ (car list1) (car list2))
                                        (add-lists (cdr list1) (cdr list2))))))
                    (define ones (cons 1 ones))
                    (define integers (cons 1 (add-lists ones integers)))
                    (list-ref integers 17)
                    (define (integral integrand initial-value dt)
                      (define int
                        (cons initial-value
                              (add-lists (scale-list integrand dt) int)))
                      int)
                    (define (solve f y0 dt)
                      (define
                        y (integral dy y0 dt))
                      (define dy (map f y))
                      y)
                    (list-ref (solve (lambda (x) x) 1 0.001) 1000)
                    (car (cdr '(a b c)))
                    ))
    'not-test-stream)
;run
;(driver-loop)