(ns set-cas.core)

(def op first)

(def args rest)

(defn nassoc? [op] (= '= op))

(def rassoc-ops #{'+ '* 'd})
(defn rassoc? [op] (rassoc-ops op))

(defn lassoc? [op] (= '- op))

(def union-comm '(= (+ X Y) (+ Y X)))

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

(defn replace-vars [expr vars]
  (cond (coll? expr)
        (let [[op & args] expr
              args (map #(replace-vars % vars) args)]
          (cons op args))

        :else (vars expr expr)))

(defn subst' [expr lhs rhs]
  (cond (= lhs expr) rhs

        (coll? expr)
        (let [[op & args] expr
              args (map #(subst' % lhs rhs) args)]
          (cons op args))

        :else expr))

(defn subst [expr eq vars]
  (assert (= '= (op eq)))
  (assert (<= 2 (count (args eq))))
  (let [[lhs rhs] (map #(replace-vars % vars) (rest eq))]
    (subst' expr lhs rhs)))
