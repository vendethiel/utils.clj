(ns venutils.compose)

(defmacro juxt-apply
  "Macro form of the apply juxt pattern, for performance reasons.
  Recognizes literals and `(constantly _)` forms."
  [res-fn fns]
  (let [msym (gensym "comp-apply-object")
        constantly-form? #(and (list? %)
                              (= 2 (count %))
                              (= 'constantly (first %)))
        format-fn (fn [f]
                    (cond (constantly-form? f) (second f) ; unwrap `(constantly ...)`
                          (or (list? f) (ifn? f)) (list f msym) ; looks callable
                          :else f ; literal
                          ))]
     `(fn [~msym]
       (~res-fn ~@(map format-fn fns)))))
