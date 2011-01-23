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
            nil
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

; evif and evlis (not tested)

(def evif (clauses env)
  (if (no clauses)
       nil
      (no (cdr clauses))
       (ev (car clauses) env)
      (ev (car clauses) env)
       (ev (cadr clauses) env)
       (evif (cddr clauses) env)))

; don't like name
(def evlis (arglist env)
  (if (no arglist)
       '() ; arc will turn to nil at read-time
       (cons (ev (car arglist) env)
             (evlis (cdr arglist) env))))

; eveq

(def eveq (x y env)
  (if (is (ev x env) (ev y env))
       (ev x env) ; return the match instead of 't
       nil))

; ev (eval) and ply (apply)

(def evatom (exp env)
  ; omit number case so all atoms are symbols
  (value exp env))

(def evcons (exp env)
  (case (car exp)
    fn   (list '&procedure (cadr exp) (caddr exp) env)
    vau  (list '&fexpr (cadr exp) (caddr exp) env)
    eval (ev (ev (cadr exp) env) env)
    if   (evif (cdr exp) env)
    eq   (eveq (cadr exp) (caddr exp) env)
    car  (car (ev (cadr exp) env))
    cdr  (cdr (ev (cadr exp) env))
    cons (cons (ev (cadr exp) env) (ev (caddr exp) env))
         (evproc (ev (car exp) env) (cdr exp) env)))

(def ev (exp env)
  (if (atom exp) (evatom exp env)
                 (evcons exp env)))

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

; alias for convenience

(= e ev-with-lib)
