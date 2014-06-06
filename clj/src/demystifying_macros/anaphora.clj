(ns demystifying-macros.anaphora)

(defmacro afn
  [params body]
  `(letfn [(~(symbol "self") [~@params] ~@body)]
     #'self))
