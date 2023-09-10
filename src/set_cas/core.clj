(ns set-cas.core)

(def op first)

(def args rest)

(defn nassoc? [op] (= '= op))

(def rassoc-ops #{'+ '* 'd})
(defn rassoc? [op] (rassoc-ops op))

(defn lassoc? [op] (= '- op))

(defn to-bin [expr]
  (cond (coll? expr)
        (let [[op & args] expr
              args (map to-bin args)]
          (cond (or (nassoc? op)
                    (<= (count args) 2))
                (cons op args)

                (rassoc? op)
                (list op (first args)
                      (to-bin (cons op (rest args))))

                (lassoc? op)
                (to-bin (concat (list op (cons op (take 2 args)))
                                (drop 2 args)))

                :else (throw (ex-info "unexpected op" {:expr expr}))))

        :else expr))


