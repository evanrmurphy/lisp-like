; utils

(def cdar (xs) (cdr (car xs)))

; main

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
