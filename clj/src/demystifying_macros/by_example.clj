(ns demystifying-macros.by-example)

(defmacro nu-let
  [bindings body]
  `((fn [~@(map first (partition 2 bindings))]
      ~body)
    ~@(map second (partition 2 bindings))))

(defmacro nu-let
  [bindings body]
  (let [binding-pairs (partition 2 bindings)
        vars          (map first binding-pairs)
        rhss          (map second binding-pairs)]
    `((fn [~@vars] ~body) ~@rhss)))
