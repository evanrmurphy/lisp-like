; utils

(def cdar (xs) (cdr (car xs)))

; main

(def bind (vars args env)
       (if (is (len vars) (len args))
            (cons (cons vars args) env)
            (err "")))

(def value (name env)
       (value1 name (lookup name env)))

(def value1 (name slot)
       (if (is slot '&unbound)
            (err "")
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
