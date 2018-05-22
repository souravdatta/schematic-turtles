#lang racket


(define default-env (make-hash
                     (list (cons '+ +)
                           (cons '- -)
                           (cons '* *)
                           (cons '/ /))))

(define (env-get env sym)
  (hash-ref env sym))

(define (env-set! env sym val)
  (hash-set! env sym val))

(define (env-extend env1 env2)
  (let ([new-env (hash-copy env1)])
    (hash-for-each env2 (λ (k v)
                          (env-set! new-env k v)))
    new-env))

(define (env-from-lists args vals)
  (if (not (= (length args) (length vals)))
      (error "mismatched number of arguments passed")
      (make-hash (map cons args vals))))

(define (st-eval expr #:env [env default-env])
  (cond
    ((real? expr) expr)
    ((symbol? expr) (env-get env expr))
    ((list? expr) (st-eval-list expr env))
    (else (error "Not a valid expression"))))


(define (st-eval-list expr env)
  (cond
    ((empty? expr) '())
    ((define-expr? expr) (st-eval-define expr env))
    (else (st-apply (st-eval (first expr) #:env env)
                    (map (λ (e) (st-eval e #:env env)) (rest expr))
                    #:env env))))

(define (define-expr? expr)
  (tagged-with? expr 'define))

(define (tagged-with? expr sym)
  (and (list? expr) (eq? (car expr) sym)))

(define (st-eval-define expr env)
  (let [(var (second expr))
        (val (third expr))]
    (cond
      ((not (symbol? var)) (error "Cannot define non symbols"))
      ((st-lambda-expr? val) (st-add-fn var val env))
      (else (begin (env-set! env var (st-eval val)) env)))))

(define (st-lambda-expr? expr)
  (tagged-with? expr 'lambda))

(define (st-add-fn name-sym expr env)
  (env-set! env name-sym (list 'function
                               (list 'args (second expr))
                               (list 'body (cddr expr))))
  env)

(define (st-function? expr)
  (tagged-with? expr 'function))
  
(define (st-apply fn arg-list #:env [env default-env])
    (if (not (st-function? fn))
        (native-apply fn arg-list)
        (let ([args (second (second fn))]
              [body (second (third fn))])
          (last (map (λ (expr) (st-eval expr #:env (env-extend env (env-from-lists args arg-list)))) body)))))

(define (native-apply fn arg-list)
  (if (procedure? fn)
      (apply fn arg-list)
      (error "native-apply: do not know how to do that!")))



