;;; Simple lisp-like interpreter, modeled after
;;; Figure 7 in The Art of the Interpreter
;;; (http://www.scribd.com/doc/2416804/Lisp-Art-of-the-interpreter-sussman) 

;; utils

(def cdar (xs) (cdr (car xs)))
(def cadar (xs) (car (cdr (car xs))))
(def caddr (xs) (car (cdr (cdr xs))))
(def cadddr (xs) (car (cdr (cdr (cdr xs)))))

;; main

; bind and value

(def bind (vars args env)
       (if (is (len vars) (len args))
            (cons (cons vars args) env)
            (err "in bind")))

(def value (name env)
       (value1 name (lookup name env)))

(def value1 (name slot)
       (if (is slot '&unbound)
            (err "in value1")
            (car slot)))

(def lookup (name env)
       (if (no env)
            '&unbound
            (lookup1 name (caar env) (cdar env) env)))

(def lookup1 (name vars vals env)
  (if (no vars)
       (lookup name (cdr env))
      (is name (car vars))
       vals
       (lookup1 name (cdr vars) (cdr vals) env)))

; evcond and evlis (not tested)

(def evcond (clauses env)
  (if (no clauses)
       (err "in evcond")
      (ev (caar clauses) env)
       (ev (cadar clauses) env)
       (evcond (cdr clauses) env)))

(def evlis (arglist env)
  (if (no arglist)
         '()
         (cons (ev (car arglist) env)
               (evlis (cdr arglist) env))))

; ev (eval) and ply (apply)

(def ev (exp env)
  (if (atom exp)
       ; omitted number case, numbers are just symbols!
       (value exp env)
      (is (car exp) 'lambda)
       (list '&procedure (cadr exp) (caddr exp) env)
      (is (car exp) 'vau)
       (list '&fexpr (cadr exp) (caddr exp) env)
      (is (car exp) 'cond)
       (evcond (cdr exp) env)
       (let fun (ev (car exp) env)
         (if (is (car fun) '&procedure)
              (ply fun (evlis (cdr exp) env))
             (is (car fun) '&fexpr)
              (ply fun (cdr exp))
              (err "in ev")))))

(def ply (fun args)
  (if ; primop case omitted
      (or (is (car fun) '&procedure)
          (is (car fun) '&fexpr))
       (ev (caddr fun)
           (bind (cadr fun) args (cadddr fun)))
       (err "in ply")))

;; lib

; (vau (x) x)       ; quote

(mac ev-with-lib body
  `(ev '((lambda (quote)
           ,@body)
         (vau (x) x)
         ; not easy to can't add more args here since
         ; quote not yet defined
         )
       nil))
