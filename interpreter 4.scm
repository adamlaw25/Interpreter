; Jiahao Luo Kieran Aulak Ananth Suresh

; If you are using racket instead of scheme, uncomment these two lines, comment the (load "simpleParser.scm") and uncomment the (require "simpleParser.scm")
; #lang racket
; (require "simpleParser.scm")
(load "classParser.scm")
(require racket/format)

; An interpreter for the simple language that uses call/cc for the continuations.  Does not handle side effects.
;(define call/cc call-with-current-continuation)

; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define interpret
  (lambda (file mainclass)
    (scheme->language
     (call/cc
      (lambda (return)
        (interpret-statement-list (get-main-statement-list (parser file) mainclass) (push-frame (initialize (parser file) (newenvironment))) return
                                    (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                                    (lambda (v env) (myerror "Uncaught exception thrown")))
                                  )))))

; get the main method body
(define get-main-statement-list
  (lambda (statement-list mainclass)
    (if (null? statement-list)
        (myerror "No main method")
        (if (is-main-class (topstatement statement-list) mainclass)
            (get-main-statement-list-in-class (get-class-body (topstatement statement-list)))
            (get-main-statement-list (remainingclasses statement-list) mainclass)))))

(define is-main-class
    (lambda (statement mainclass)
        (if (eq? 'class (operator statement))
            (if (string=? mainclass (~a (get-class-name statement)))
                #t
                #f
            )
            #f)))

; get the main method body
(define get-main-statement-list-in-class
  (lambda (statement-list)
    (if (null? statement-list)
        (myerror "No main method")
        (if (is-main (topstatement statement-list))
            (function-body (topstatement statement-list))
            (get-main-statement-list-in-class (remainingstatements statement-list))))))

; return true if the statement is a main method
(define is-main
  (lambda (statement)
    (if (eq? 'static-function (operator statement))
        (if (eq? 'main (get-function-name-in-parser statement))
            #t
            #f)
        #f)))

; initialization to the base layer 
(define initialize
	(lambda (statement-list environment)
		(if (null? statement-list)
			environment
			(initialize (remainingclasses statement-list) (initialize-class (topstatement statement-list) environment)))))

; initialize a class
; class closure: [(classname) (extend-class (variables-closure) (function-closure))]
(define initialize-class
  (lambda (class-statement-list environment)
    (insert (get-class-name class-statement-list) (list (get-extend-class class-statement-list) (topstatement (initialize-variable-closure (get-class-body class-statement-list) (get-extend-class class-statement-list) environment (newenvironment))) (topstatement (initialize-function-closure (get-class-body class-statement-list) (newenvironment)))) environment)))

; helper methods
(define get-extend-class
  (lambda (class-statement-list)
    (if (null? (extend-class-statement class-statement-list))
        '()
        (extend-class (extend-class-statement class-statement-list)))))

; create the variable closure for a class
(define initialize-variable-closure
  (lambda (class-body extend-class current-environment variable-closure)
    (if (null? class-body)
        (if (null? extend-class)
            variable-closure
            (insert-super variable-closure extend-class current-environment))
        (initialize-variable-closure (remainingstatements class-body) extend-class current-environment (initialize-variable (topstatement class-body) variable-closure)))))

(define insert-super
  (lambda (variable-closure extend-class current-environment)
    (let ((closure-result (topframe (insert 'super (list 'new extend-class) variable-closure))))
      (list (list (reverse (topframe closure-result)) (reverse (operand1 closure-result)))))))

      
; initialize variable based on 1 statement
(define initialize-variable
  (lambda (statement variable-closure)
    (cond
      ((eq? 'var (operator statement)) (initialize-declare statement variable-closure))
      ((eq? '= (operator statement)) (initialize-assign statement variable-closure))
      ((eq? 'function (operator statement)) variable-closure)
      ((eq? 'static-function (operator statement)) variable-closure)
      (else (myerror "variables initialization error:" (operator statement))))))

; create the function closure for a class
(define initialize-function-closure
  (lambda (class-body function-closure)
    (if (null? class-body)
        function-closure
        (initialize-function-closure (remainingstatements class-body) (initialize-function-statement (topstatement class-body) function-closure)))))

; initialize function based on 1 statement
(define initialize-function-statement
  (lambda (statement function-closure)
    (cond
      ((eq? 'var (operator statement)) function-closure)
      ((eq? '= (operator statement)) function-closure)
      ((eq? 'function (operator statement)) (initialize-function statement function-closure))
      ((eq? 'static-function (operator statement)) function-closure)
      (else (myerror "functions initialization error:" (operator statement))))))

; initialize a statement 
#|(define initialize-statement
	(lambda (statement environment)
		(cond
			((eq? 'var (operator statement)) (initialize-declare statement environment))
			((eq? '= (operator statement)) (initialize-assign statement environment))
			((eq? 'function (operator statement)) 
				(if (not (eq? 'main (get-function-name statement)))
					(initialize-function statement environment)
					environment))
			(else (myerror "initialization error:" (operator statement))))))|#

; initialize a declaration 
(define initialize-declare
	(lambda (statement environment)
             (if (exists-declare-value? statement)
                 (insert (get-declare-var statement) (interpret-eval-expression (get-declare-value statement) environment default-continuation default-continuation default-continuation default-continuation) environment)
                 (insert (get-declare-var statement) 'novalue environment))))

; initialize an insertion
(define initialize-insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (topframe environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (topframe environment)) (remainingframes environment)))))

; initialize a function
(define initialize-function
	(lambda (statement environment)
		(insert (get-function-name-in-parser statement) (list (cons 'this (get-function-param statement)) (get-function-body statement)) environment)))

; initialize an assignment
(define initialize-assign
	(lambda (statement environment)
	(update (get-assign-lhs statement) (interpret-eval-expression (get-assign-rhs statement) environment default-continuation default-continuation default-continuation default-continuation) environment)))

; interprets a list of statements.  The environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list (remainingstatements statement-list) (interpret-statement (topstatement statement-list) environment return break continue throw) return break continue throw))))

; a list of statements that will always return the environment
(define interpret-statement-list-return-environment
  (lambda (statement-list environment return break continue throw)
    (if (null? statement-list)
        environment
        (if (eq? 'return (operator (topstatement statement-list)))
            (remainingframes environment)
            (interpret-statement-list-return-environment (remainingstatements statement-list) (interpret-statement (topstatement statement-list) environment return break continue throw) return break continue throw)))))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement environment return break continue throw)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return break continue throw))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment return break continue throw))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment return break continue throw))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return break continue throw))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement)) (break environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment return break continue throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw))
      ((eq? 'funcall (statement-type statement)) (interpret-funcall-return-environment statement environment break continue throw))
      ((eq? 'function (statement-type statement)) (insert (get-function-name-in-parser statement) (list (get-function-param statement) (get-function-body statement)) environment))
      ;(pop-frame (insert (get-function-name-in-parser statement) (list (get-function-param statement) (get-function-body statement)) environment)))
      ((eq? 'new (statement-type statement)) (interpret-object statement environment return break continue throw))
      ((eq? 'dot (statement-type statement)) (interpret-dot statement environment return break continue throw))
      (else (myerror "Unknown statement:" (statement-type statement))))))

;interpret dot
(define interpret-dot
  (lambda (statement environment return break continue throw)
    (if (list? (get-dot-instance statement))
        (if (eq? 'new (operator (get-dot-instance statement)))
            (unbox (get-instance-field (interpret-object (get-dot-instance statement) environment return break continue throw) (get-dot-field statement)))
            (if (eq? 'funcall (operator (get-dot-instance statement)))
                (unbox (get-instance-field (interpret-funcall (get-dot-instance statement) environment break continue throw) (get-dot-field statement)))
                (unbox (get-instance-field (interpret-dot (get-dot-instance statement) environment return break continue throw) (get-dot-field statement)))))
        (if (eq? 'super (get-dot-instance statement))
            (let ((instance-closure (interpret-dot (list 'dot 'this 'super) environment return break continue throw)))
              (unbox (get-instance-field instance-closure (get-dot-field statement))))
            (let ((instance-closure (unbox (lookup (get-dot-instance statement) environment))))
          ;(if (is-class-function (class-name-in-instance-closure instance-closure) (get-dot-field statement) environment)
              ;(myerror "should not happend")
              (unbox (get-instance-field instance-closure (get-dot-field statement))))))))

(define get-instance-field
  (lambda (instance-closure field-name)
    (let ((field-closure (field-closure-in-instance-closure instance-closure)))
        (if (exists-in-list? field-name (variables field-closure))
           (lookup field-name (list field-closure))
           (if (exists-in-list? 'super (variables field-closure))
               (get-instance-field (unbox (lookup 'super (list field-closure))) field-name)
               (myerror "no such field" field-name))))))
    
(define is-class-function
  (lambda (classname fieldname environment)
    (let ((class-closure (unbox (lookup classname (get-class-closure-list environment)))))
      (let ((function-closure (function-closure-in-class-closure class-closure)))
        (if (null? (extend-class-in-class-closure class-closure))
            (exists-in-list? fieldname (variables function-closure))
            (or (exists-in-list? fieldname (variables function-closure)) (is-class-function (extend-class-in-class-closure class-closure) fieldname environment)))))))
    
;interpret new statement
(define interpret-object
  (lambda (statement environment return break continue throw)
       (list (get-class-name statement) (copy-var-closure (get-class-name statement) (list (bottomframe environment))))))
      
;copies the variable closure from the class with new boxings for each variable          
(define copy-var-closure
  (lambda (classname environment)
    (rebox-var-closure (field-closure-in-class-closure (unbox(lookup classname environment))) environment)))

(define rebox-var-closure
  (lambda (var-closure environment)
    (cons (first* var-closure) (list (rebox-var-list (operand1 var-closure) '() environment)))))
    
(define rebox-var-list
  (lambda (var-list result environment)
    (if (null? var-list)
     result
     (if (list? (unbox (first* var-list)))
         (rebox-var-list (rest* var-list) (append result (list (box (interpret-object (unbox (first* var-list)) environment default-continuation default-continuation default-continuation default-continuation)))) environment)
         (rebox-var-list (rest* var-list) (append result (list (box (unbox (first* var-list))))) environment)))))

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return break continue throw)
    (let ((result (interpret-eval-expression (get-expr statement) environment return break continue throw)))
      (if (box? result)
          (return (unbox result))
          (return result)))))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment return break continue throw)
    (if (exists-declare-value? statement)
        (insert (get-declare-var statement) (interpret-eval-expression (get-declare-value statement) environment return break continue throw) environment)
        (insert (get-declare-var statement) 'novalue environment))))

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement environment return break continue throw)
    (begin (set-box! (lookup (get-assign-lhs statement) environment) (interpret-eval-expression (get-assign-rhs statement) environment return break continue throw)) environment)))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw)
    (cond
      ((interpret-eval-expression (get-condition statement) environment return break continue throw) (interpret-statement (get-then statement) environment return break continue throw))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw))
      (else environment))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return break continue throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (interpret-eval-expression condition environment return break continue throw)
                            (loop condition body (interpret-statement body environment return break (lambda (env) (break (loop condition body env))) throw))
                         environment))))
         (loop (get-condition statement) (get-body statement) environment))))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment return break continue throw)
    (pop-frame (interpret-statement-list (rest* statement)
                                         ;(add-parameters (list 'current 'this) (list (unbox (lookup 'current environment)) (unbox (lookup 'this environment))) (push-frame environment))
                                         (cons (topframe environment) environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))))))

;(define copy-parameters-to-block
 ; (lambda (second-frame-vars second-frame-values environment)
  ;  (if (null? second-frame-vars)
   ;     environment
    ;    (copy-parameters-to-block (rest* second-frame-vars) (rest* second-frame-values) (insert (first* second-frame-vars) (first* second-frame-values))))))
     
; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment return break continue throw)
    (throw (interpret-eval-expression (get-expr statement) environment return break continue throw) environment)))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env return break continue throw)))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (pop-frame (interpret-statement-list 
                                                 (get-body catch-statement) 
                                                 (insert (catch-var catch-statement) ex (push-frame env))
                                                 return 
                                                 (lambda (env2) (break (pop-frame env2))) 
                                                 (lambda (env2) (continue (pop-frame env2))) 
                                                 (lambda (v env2) (throw v (pop-frame env2)))))
                                     return break continue throw)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw jump finally-block)))
         (interpret-block finally-block
                          (interpret-block try-block environment new-return new-break new-continue new-throw)
                          return break continue throw))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; funcall
(define interpret-funcall
  (lambda (statement environment break continue throw)
    (call/cc
      (lambda (return)
        (if (exists-in-list? (cadr statement) (variables (topframe environment)))
            ;(interpret-statement-list (lookup-function-body (cadr statement) environment)
                                      ;(add-parameters (cons 'current (cons 'this (lookup-parameters (cadr statement) environment))) (cons (unbox (lookup 'current environment)) (cons (unbox (lookup 'this environment)) (get-values statement))) (push-frame environment)) return break continue throw) 
            (remainingframes (interpret-statement-list (lookup-function-body (cadr statement) environment)
                                      (add-parameters (lookup-parameters (operand1 statement) environment) (get-values statement) (cons (topframe environment) environment)) return break continue throw))
            (if (list? (operand1 statement))
            (if (list? (operand1 (operand1 statement)))
                (if (eq? 'new (operator (operand1 (operand1 statement))))
                    ; if it is funcall (new ...)
                    (interpret-funcall-new statement environment return break continue throw)
                    ; if it is funcall (dot ...)
                    (let ((this (interpret-funcall (operand1 (operand1 statement)) environment break continue throw)))
                      (let ((function-class-name (function-location (get-function-name statement) (+ (length (get-values statement)) 1) (class-name-in-instance-closure this) (get-class-closure-list environment))))
                        (let ((function-closure (get-function-in-class (get-function-name statement) (+ (length (get-values statement)) 1) function-class-name (get-class-closure-list environment)))
                              (current (real-type-instance this function-class-name)))
                          (remainingframes (interpret-statement-list (function-body-in-function-closure function-closure) (add-parameters (cons 'current (parameter-list-in-function-closure function-closure)) (cons current (cons this (get-values statement))) (push-frame environment)) return break continue throw))))))
                ; if it is funcall instance-name
                (interpret-funcall-object statement environment return break continue throw))
            (if (null? (get-values statement)) 
                (interpret-funcall-no-dot (list 'funcall (list 'dot 'current (operand1 statement))) environment return break continue throw)
                (interpret-funcall-no-dot (cons 'funcall (cons (list 'dot 'current (operand1 statement)) (get-values statement))) environment return break continue throw))))))))

; funcall for without dot
(define interpret-funcall-no-dot
  (lambda (statement environment return break continue throw)
    (if (list? (operand1 statement))
            (if (list? (operand1 (operand1 statement)))
                ; if it is funcall (new ...)
                (interpret-funcall-new statement environment return break continue throw)
                ; if it is funcall instance-name
                (interpret-funcall-object statement environment return break continue throw))
            (if (null? (get-values statement))
                (interpret-funcall-no-dot (list 'funcall (list 'dot 'this (operand1 statement))) environment return break continue throw)
                (interpret-funcall-no-dot (list 'funcall (list 'dot 'this (operand1 statement)) (get-values statement)) environment return break continue throw)))))

; funcall with a new statement for dot
(define interpret-funcall-new
  (lambda (statement environment return break continue throw)
    (let ((this (interpret-object (operand1 (operand1 statement)) environment default-continuation default-continuation default-continuation default-continuation)))
      (let ((function-class-name (function-location (get-function-name statement) (+ (length (get-values statement)) 1) (class-name-in-instance-closure this) (get-class-closure-list environment))))
        (let ((function-closure (get-function-in-class (get-function-name statement) (+ (length (get-values statement)) 1) function-class-name (get-class-closure-list environment)))
              (current (real-type-instance this function-class-name)))
          (remainingframes (interpret-statement-list (function-body-in-function-closure function-closure) (add-parameters (cons 'current (parameter-list-in-function-closure function-closure)) (cons current (cons this (get-values statement))) (push-frame  environment)) return break continue throw)))))))

; funcall with an object
(define interpret-funcall-object
  (lambda (statement environment return break continue throw)
    (if (eq? 'super (operand1 (operand1 statement)))
        ; if it is funcall (dot super ...)
        (let ((current (unbox (lookup 'current environment)))
              (current-super (interpret-dot (list 'dot 'current 'super) environment default-continuation default-continuation default-continuation default-continuation)))
          (let ((function-closure (get-function-in-class (get-function-name statement) (+ (length (get-values statement)) 1) (class-name-in-instance-closure current-super) (get-class-closure-list environment)))) 
            (remainingframes (interpret-statement-list (function-body-in-function-closure function-closure) (add-parameters (cons 'current (parameter-list-in-function-closure function-closure)) (cons current-super (cons current (get-values statement))) (push-frame environment)) return break continue throw))))
        ; otherwise
        (let ((this (unbox (lookup (operand1 (operand1 statement)) environment))))
          (let ((function-class-name (function-location (get-function-name statement) (+ (length (get-values statement)) 1) (class-name-in-instance-closure this) (get-class-closure-list environment))))
            (let ((function-closure (get-function-in-class (get-function-name statement) (+ (length (get-values statement)) 1) function-class-name (get-class-closure-list environment)))
                  (class-closure (unbox (lookup function-class-name (get-class-closure-list environment))))
                  (current (real-type-instance this function-class-name)))
              (remainingframes (interpret-statement-list (function-body-in-function-closure function-closure) (add-parameters (cons 'current (parameter-list-in-function-closure function-closure)) (cons current (cons this (get-values statement))) (push-frame  environment)) return break continue throw))))))))
         
; get the super instance closure or return current instance closure
(define real-type-instance
  (lambda (instance-closure classname)
    (if (box? instance-closure)
        (let ((result-closure (unbox instance-closure)))
          (if (eq? classname (class-name-in-instance-closure result-closure))
              result-closure
              (real-type-instance (lookup 'super (list (field-closure-in-instance-closure result-closure))) classname)))
        (if (eq? classname (class-name-in-instance-closure instance-closure))
            instance-closure
            (real-type-instance (lookup 'super (list (field-closure-in-instance-closure instance-closure))) classname)))))

; get the class where the function locate
(define function-location
  (lambda (function-name parameter-count initial-class-name  class-closure-list)
    (let ((initial-class-closure (lookup initial-class-name class-closure-list)))
      (let ((initial-function-closure (function-closure-in-class-closure (unbox initial-class-closure))))
        (if (is-function-in-function-closure function-name parameter-count (variables initial-function-closure) (function-body-in-function-closure initial-function-closure))
            initial-class-name
            (function-location function-name parameter-count (extend-class-in-class-closure (unbox initial-class-closure)) class-closure-list))))))

;check if it is a function in the function closure
(define is-function-in-function-closure
  (lambda (function-name parameter-count function-closure-variables function-closure-values)
    (if (null? function-closure-variables)
        #f
        (if (and (eq? function-name (first* function-closure-variables)) (eq? parameter-count (length (first* (unbox (first* function-closure-values))))))
            #t
            (is-function-in-function-closure function-name parameter-count (rest* function-closure-variables) (rest* function-closure-values))))))
              
; get function in class
(define get-function-in-class
  (lambda (function-name parameter-count initial-class-name class-closure-list)
    (let ((initial-class-closure (lookup initial-class-name class-closure-list)))
      (let ((initial-function-closure (function-closure-in-class-closure (unbox initial-class-closure))))
        (if (is-function-in-function-closure function-name parameter-count (variables initial-function-closure) (function-body-in-function-closure initial-function-closure))
            (get-function-in-function-closure function-name parameter-count (variables initial-function-closure) (function-body-in-function-closure initial-function-closure))
            (get-function-in-class function-name parameter-count (extend-class-in-class-closure (unbox initial-class-closure)) class-closure-list))))))

; get the function body in the function closure
(define get-function-in-function-closure
  (lambda (function-name parameter-count function-closure-variables function-closure-values)
    (if (null? function-closure-variables)
        (myerror "Function not in closure" function-name)
        (if (and (eq? function-name (first* function-closure-variables)) (eq? parameter-count (length (first* (unbox (first* function-closure-values))))))
            (unbox (first* function-closure-values))
            (get-function-in-function-closure function-name parameter-count (rest* function-closure-variables) (rest* function-closure-values))))))
         
; funcall that will always return the environment
(define interpret-funcall-return-environment
  (lambda (statement environment break continue throw)
    (call/cc
     (lambda (return)
       (if (exists-in-list? (cadr statement) (variables (topframe environment)))
             (remainingframes (interpret-statement-list (lookup-function-body (cadr statement) environment)
                                      (add-parameters (lookup-parameters (operand1 statement) environment) (get-values statement) (cons (topframe environment) environment)) return break continue throw))
            (if (list? (operand1 statement))
            (if (list? (operand1 (operand1 statement)))
                (if (eq? 'new (operator (operand1 (operand1 statement))))
                    ; if it is funcall (new ...)
                    (interpret-funcall-new statement environment return break continue throw)
                    ; if it is funcall (dot ...)
                    (let ((this (interpret-funcall (operand1 (operand1 statement)) environment break continue throw)))
                      (let ((function-class-name (function-location (get-function-name statement) (+ (length (get-values statement)) 1) (class-name-in-instance-closure this) (get-class-closure-list environment))))
                        (let ((function-closure (get-function-in-class (get-function-name statement) (+ (length (get-values statement)) 1) function-class-name (get-class-closure-list environment)))
                              (current (real-type-instance this function-class-name)))
                          (remainingframes (interpret-statement-list (function-body-in-function-closure function-closure) (add-parameters (cons 'current (parameter-list-in-function-closure function-closure)) (cons current (cons this (get-values statement))) (push-frame  environment)) return break continue throw))))))
                ; if it is funcall instance-name
                (interpret-funcall-object statement environment return break continue throw))
            (if (null? (get-values statement)) 
                (interpret-funcall-no-dot (list 'funcall (list 'dot 'current (operand1 statement))) environment return break continue throw)
                (interpret-funcall-no-dot (list 'funcall (list 'dot 'current (operand1 statement)) (get-values statement)) environment return break continue throw))))))))

; add the parameters to the new layer    
(define add-parameters
  (lambda (parameters values environment)
    (cond
      ((and (null? parameters) (null? values)) environment)
      ((null? values) (myerror "Error: Not given enough values"))
      ((null? parameters) (myerror "Error: Given too many values"))
      (else
       (if (eq? 'this (first* parameters))
           (add-parameters (rest* parameters) (rest* values) (insert 'this (first* values) environment))
           (if (eq? 'current (first* parameters))
                (add-parameters (rest* parameters) (rest* values) (insert 'current (first* values) environment))
                (add-parameters (rest* parameters) (rest* values) (insert (first* parameters) (interpret-eval-expression (first* values) (remainingframes environment) default-continuation default-continuation default-continuation default-continuation) environment))))))))

; eval expression
(define interpret-eval-expression
  (lambda (expr environment return break continue throw)
    (if (list? expr)
        (if (eq? 'funcall (operator expr))
            (interpret-funcall expr environment break continue throw)
            (if (eq? 'new (operator expr))
                (interpret-object expr environment return break continue throw)
                (if (eq? 'dot (operator expr))
                    (interpret-dot expr environment return break continue throw)
                    (eval-expression expr environment return break continue throw))))
        (eval-expression expr environment return break continue throw))))
  
; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment return break continue throw)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (unbox (lookup expr environment)))
      (else (eval-operator expr environment return break continue throw)))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment return break continue throw)
    (cond
      ((eq? '! (operator expr)) (not (interpret-eval-expression (operand1 expr) environment return break continue throw)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (interpret-eval-expression (operand1 expr) environment return break continue throw)))
      (else (eval-binary-op2 expr (interpret-eval-expression (operand1 expr) environment return break continue throw) environment return break continue throw)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment return break continue throw)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (interpret-eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '- (operator expr)) (- op1value (interpret-eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '* (operator expr)) (* op1value (interpret-eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '/ (operator expr)) (quotient op1value (interpret-eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '% (operator expr)) (remainder op1value (interpret-eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '== (operator expr)) (isequal op1value (interpret-eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '!= (operator expr)) (not (isequal op1value (interpret-eval-expression (operand2 expr) environment return break continue throw))))
      ((eq? '< (operator expr)) (< op1value (interpret-eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '> (operator expr)) (> op1value (interpret-eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '<= (operator expr)) (<= op1value (interpret-eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '>= (operator expr)) (>= op1value (interpret-eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '|| (operator expr)) (or op1value (interpret-eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '&& (operator expr)) (and op1value (interpret-eval-expression (operand2 expr) environment return break continue throw)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))


;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)
(define bottomframe
  (lambda (list)
    (if (null? (cdr list))
        (car list)
        (bottomframe (cdr list)))))

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))
        

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (if (list? var)
     (if (list? (operand1 var))
         (get-instance-field (unbox(interpret-object (operand1 var) environment default-continuation default-continuation default-continuation default-continuation)) (caddr var))
         (get-instance-field (unbox(lookup (operand1 var) environment)) (caddr var)))
     (lookup-variable var environment))))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      ((exists-in-list? 'current (variables (topframe environment)))
       (let ((this (unbox (lookup 'current environment))))
         (get-instance-field this var)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame)))
       (if (exists-in-list? 'this (variables frame))
             (myerror "error: undefined variable" var)))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car  l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var (box val) (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (scheme->language val) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (cons (scheme->language val) (cdr vallist)))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))



; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

(define default-continuation (lambda (v) v))
(define get-function-name
  (lambda (statement)
    (caddr (operand1 statement))))
(define get-function-param caddr)
(define get-function-body cadddr)
(define get-values cddr)
(define lookup-parameters
  (lambda (func-name environment)
    (car (unbox (lookup func-name environment)))))
(define lookup-function-body
  (lambda (func-name environment)
    (operand1 (unbox (lookup func-name environment)))))
(define get-if-exists
  (lambda (var-name environment)
    (if (exists? var-name environment)
      (unbox (lookup var-name))
      var-name)))
(define get-base-layer
  (lambda (environment)
    (if (null? (cdr environment))
        environment
        (get-base-layer (cdr environment)))))
(define function-body
  (lambda (statement)
    (cadddr statement)))

(define topstatement car)
(define remainingstatements cdr)
(define first* car)
(define rest* cdr)

(define parameter-list-in-function-closure car)
(define get-class-name cadr)
(define get-class-body cadddr)
(define remainingclasses cdr)
(define get-function-name-in-parser cadr)
(define extend-class-statement caddr)
(define extend-class cadr)
(define get-dot-instance cadr)
(define get-dot-field caddr)
(define get-object-field-list cadr)
(define get-class-closure-list
  (lambda (environment)
    (list (bottomframe environment))))
(define function-closure-in-class-closure caddr)
(define field-closure-in-class-closure cadr)
(define extend-class-in-class-closure car)
(define class-name-in-instance-closure car)
(define field-closure-in-instance-closure cadr)
(define function-body-in-function-closure cadr)