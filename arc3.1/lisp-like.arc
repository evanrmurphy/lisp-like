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

(def evif (clauses env)
  (if (no clauses)
       nil
      (no (cdr clauses))
       (ev (car clauses) env)
      (ev (car clauses) env)
       (ev (cadr clauses) env)
       (evif (cddr clauses) env)))

(def evlis (arglist env)
  (if (no arglist)
       '() ; arc will turn to nil at read-time
       (cons (ev (car arglist) env)
             (evlis (cdr arglist) env))))

; ev (eval) and ply (apply)

(def ev (exp env)
  (if (atom exp)
       ; omit number case so all atoms are symbols
       (value exp env)
      (is (car exp) 'fn)
       (list '&procedure (cadr exp) (caddr exp) env)
      (is (car exp) 'vau)
       (list '&fexpr (cadr exp) (caddr exp) env)
      (is (car exp) 'eval)
       (ev (ev (cadr exp) env) env)
      (is (car exp) 'if)
       (evif (cdr exp) env)
       (evproc (ev (car exp) env) (cdr exp) env)))

(def evproc (fun args env)
  (if (is (car fun) '&procedure)
       (ply fun (evlis args env))
      (is (car fun) '&fexpr)
       (ply fun args)
       (err "in ev")))

(def ply (fun args)
  (if ; primop case omitted
      (or (is (car fun) '&procedure)
          (is (car fun) '&fexpr))
       (ev (caddr fun)
           (bind (cadr fun) args (cadddr fun)))
       (err "in ply")))

;; lib

; (vau (x) x)       ; quote

(mac ev-with-quote body
  `(ev '((fn (quote)
           ,@body)
         (vau (x) x))
       nil))

(mac ev-with-lib body
  `(ev-with-quote
     ((fn (nil)
        ,@body)
      'nil)))
