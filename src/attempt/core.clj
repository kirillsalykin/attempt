(ns attempt.core)

(defrecord Failure [payload])


(defn fail
  ([] (fail {}))
  ([payload] (->Failure payload)))

(defn fail-when [x pred payload]
  (when (pred x)
    (fail payload)))


(defn failure? [x]
  (instance? Failure x))

(def success?
  (complement failure?))


(defmacro if-let-success?
  ([[sym form] success-branch]
   `(if-let-success? [~sym ~form] ~success-branch ~sym))
  ([[sym form] success-branch failure-branch]
   `(let [result# ~form
          ~sym    result#]
      (if (success? result#)
        ~success-branch
        ~failure-branch))))

(defmacro if-let-failure?
  ([[sym form] failure-branch]
   `(if-let-failure? [~sym ~form] ~failure-branch ~sym))
  ([[sym form] failure-branch success-branch]
   `(let [result# ~form
          ~sym    result#]
      (if (failure? result#)
        ~failure-branch
        ~success-branch))))


(defn- attempt*
  [bindings body]
  (->> bindings
       (partition 2)
       (reverse)
       (reduce (fn [next [sym form]]
                 `(let [result# ~form
                        ~sym    result#]
                    (if (failure? result#)
                      result#
                      ~next)))
               body)))

(defmacro attempt
  ([bindings body]
   (attempt* bindings body))
  ([bindings body when-failed]
   `(if-let-failure? [result# (attempt ~bindings ~body)]
      (~when-failed result#)
      result#)))

(defmacro attempt->
  [expr & forms]
  (let [g     (gensym)
        steps (map (fn [step] `(if (failure? ~g) ~g (-> ~g ~step)))
                   forms)]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro attempt->>
  [expr & forms]
  (let [g     (gensym)
        steps (map (fn [step] `(if (failure? ~g) ~g (->> ~g ~step)))
                   forms)]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))
