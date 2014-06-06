(ns demystifying-macros.core)

(def x 4)

(comment
  "Delaying evaluation with Quote"
  x
  
  'x

  #'x)

(def y 5)

(comment
  "Delaying evaluation with Syntax Quote (Backquote)"
  [x y]

  '[x y]

  `[x y])

(comment
  "Selectively controlling evaluation inside the scope of Syntax Quote
with Unquote"
  (eval `[x y])
  (eval `[(+ x x) (+ y y)])

  `[~x y]
  `[~(+ x x) (+ y y)])

(comment
  "Splicing a list of expressions into another expression"
  (take 3 (repeat 'x))
  (+ (4 4 4))
  (+ 4 4 4)
  `(+ ~(take 3 (repeat 'x)))
  `(+ ~@(take 3 (repeat 'x))))

(comment
  "Wrap up the intent to use seconds as an abstraction"
  (Thread/sleep 3000)
  (Thread/sleep (* 3 1000)))

(defn sleep-seconds
  [m]
  (Thread/sleep (* m 1000)))

(comment
  "Defining a sleep function for every unit of time is repetitive and tedious"
  (defn sleep-minutes ...)
  (defn sleep-hours   ...)
  (defn sleep-days    ...)
  (defn sleep-years   ...))

(defn sleep-units
  [value unit]
  (Thread/sleep
   (* value
      (case unit
        :ms 1
        :s 1000
        :m 60000
        :h 3600000
        :d 86400000
        :us 1/1000))))

(comment
  "The function requires runtime dispatch"
  (sleep-units 5 :m)
  ;; evaluates at runtime to
  (Thread/sleep (* 5 (case :m
                       :ms 1
                       :s 1000
                       :m 60000
                       :h 3600000
                       :d 86400000
                       :us 1/1000)))
  ;; evaluates to
  (Thread/sleep (* 5 60000))
  ;; evaluates to
  (Thread/sleep 300000))

(defmacro sleep-units
  [value unit]
  `(Thread/sleep
    (* ~value
       ~(case unit
          :ms 1
          :s 1000
          :m 60000
          :h 3600000
          :d 86400000
          :us 1/1000))))

(comment
  "The macro eliminates runtime dispatch"
  (sleep-units 5 :m)
  ;; *compiles* to
  (Thread/sleep (* 5 60000))
  ;; evaluates to
  (Thread/sleep 300000)

  "But at the cost of compositionality"
  ;; blows up when compiling
  (sleep-units 5 (if (hibernate? user-input) :d :h)))

(comment
  "Decouple the abstraction of sleeping and the abstraction of time units")

(defmacro unit-of-time
  [value unit]
  `(* ~value
      ~(case unit
         :s  1
         :m  60
         :h  3600
         :d  86400
         :ms 1/1000
         :us 1/1000000)))

(comment
  "Examples of use"
  (unit-of-time 1 :s)
  (unit-of-time 5 :m)
  (unit-of-time 3 :d))

(comment
  "Digression #1: Nested Syntax Quote and Unquote"
  `x
  `~x
  ``x
  ```x
  ```~x
  ```~~x
  ```~~~x
  `~~x)

(comment
  "Digression #2: Lexical Scope"
  x
  (let [x 101] x)
  (let [x 101] (resolve-to-x))

  (resolve-to-hmm)
  (let [x 101] (resolve-to-hmm)))

(defmacro resolve-to-x [] `x)
(defmacro resolve-to-hmm [] `~'x)

(comment
  "Digression #3: Symbol generation"
  (gensym)

  (let [x-sym (gensym "x")
        y-sym (gensym "y")]
    [x-sym y-sym])

  (let [x-sym (gensym "x")
        y-sym (gensym "y")]
    `(fn [~x-sym ~y-sym]
       (+ ~x-sym ~y-sym))))

(comment
  "DRY out the act of defining the above macro for every type of unit"
  (defunits time :s
    {:m  60
     :h  3600
     :d  86400
     :ms 1/1000
     :us 1/1000000})

  (defunits distance :in
    {:foot 12
     :yard 36
     :mile 63360}))

(defmacro defunits
  [qty base-unit spec]
  (let [unit-of-qty-sym (symbol (str "unit-of-" (name qty)))
        value-sym       (gensym "value")
        unit-sym        (gensym "unit")
        cases           (mapcat identity spec)]
    `(defmacro ~unit-of-qty-sym
       [~value-sym ~unit-sym]
       `(* ~~value-sym
           ~(case ~unit-sym
              ~base-unit 1
              ~@cases)))))

(comment
  "Enhance macro generation with relative unit specs"
  (defrelunits time s
    {:m  60
     :h  [60 :m]
     :d  [24 :h]
     :ms [1/1000 :s]
     :us [1/1000 :ms]}))

(comment
  "Define a helper function to normalize relative unit specs"
  ;; => 3600
  (eval-step :h {:s 1 :m 60 :h [60 :m]}))

(defn eval-step
  ([unit units]
     (eval-step unit units 1))
  ([unit units acc]
     (let [spec-chain (units unit)]
       (cond (number? spec-chain)
             (* acc spec-chain)

             (seq spec-chain)
             (let [[factor chain-unit] spec-chain]
               (recur chain-unit units (* acc factor)))

             :else
             (throw (IllegalArgumentException. (str "Unknown unit " unit)))))))

(defmacro defrelunits
  [qty base-unit spec]
  (let [unit-of-qty-sym (symbol (str "unit-of-" (name qty)))
        value-sym       (gensym "value")
        unit-sym        (gensym "unit")
        spec-with-base  (assoc spec base-unit 1)
        cases           (mapcat (fn [[spec-unit spec-chain]]
                                  [spec-unit
                                   (eval-step spec-unit spec-with-base)])
                                spec-with-base)]
    `(defmacro ~unit-of-qty-sym
       [~value-sym ~unit-sym]
       `(* ~~value-sym
           ~(case ~unit-sym
              ~@cases)))))

(comment
  "Enhance the step evaluator to handle cyclical specification errors"
  (eval-step :h {:s 1
                 :m [1/60 :h]
                 :h [60 :m]}))

(defn eval-step
  ([unit units]
     (eval-step unit units #{} 1))
  ([unit units visited acc]
     (if (visited unit)
       (throw (IllegalArgumentException. (str unit " depends on " (second (units unit)) " depends on " unit)))
       (let [spec-chain (units unit)]
         (cond (number? spec-chain)
               (* acc spec-chain)

               (seq spec-chain)
               (let [[factor chain-unit] spec-chain]
                 (recur chain-unit
                        units
                        (conj visited unit)
                        (* acc factor)))

               :else
               (throw (IllegalArgumentException. (str "Unknown unit " unit))))))))

(comment
  "Enhancing error messages for macro usage")

(comment)
(defmacro defrelunits
  [qty base-unit spec]
  (let [unit-of-qty-sym (symbol (str "unit-of-" (name qty)))
        value-sym       (gensym "value")
        unit-sym        (gensym "unit")
        spec-with-base  (assoc spec base-unit 1)
        cases           (mapcat (fn [[spec-unit spec-chain]]
                                  [spec-unit
                                   (try
                                     (eval-step spec-unit spec-with-base)
                                     (catch IllegalArgumentException e
                                       (throw (IllegalArgumentException.
                                               (format "`%s` provided to `%s` on line `%s` has an error: `%s`"
                                                       spec
                                                       (first &form)
                                                       (-> &form meta :line)
                                                       (.getMessage e))))))])
                                spec-with-base)]
    `(defmacro ~unit-of-qty-sym
       [~value-sym ~unit-sym]
       `(* ~~value-sym
           ~(case ~unit-sym
              ~@cases)))))

(comment
  "Erroneous spec #1"
  (defrelunits time :s
    {:m 60
     :d [24 :h]}))

(comment
  "Erroneous spec #2"
  (defrelunits time :s
    {:m [1/60 :h]
     :h [60 :m]}))

(comment
  "Leverage the macro to easily and declaratively generate a DSL for distance units")

(defrelunits distance :m
  {:km 1000
   :cm 1/100
   :mm [1/10 :cm]
   :nm [1/1000 :mm]

   :yard    9144/10000
   :foot    [1/3 :yard]
   :inch    [1/12 :foot]
   :mile    [1760 :yard]
   :furlong [1/8 :mile]

   :nautical-mile 1852
   :cable         [1/10 :nautical-mile]
   :fathom        [2 :yard]

   :old-brit-nautical-mile [6080/3 :yard]
   :old-brit-cable         [1/10 :old-brit-nautical-mile]
   :old-brit-fathom        [1/100 :old-brit-cable]})

(comment
  ;; => 75/76
  (/ (unit-of-distance 1 :fathom)
     (unit-of-distance 1 :old-brit-fathom)))
