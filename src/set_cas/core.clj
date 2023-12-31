(ns set-cas.core
  (:require [com.rpl.specter :as sp]))

(def op first)

(def args rest)

(defn nassoc? [op] (= '= op))

;; TODO make +/*/d lassoc?
(def rassoc-ops #{'+ '* 'd})
(defn rassoc? [op] (rassoc-ops op))

(defn lassoc? [op] (= '- op))

(defn eq [vars lhs rhs]
  {:vars vars
   :eq (list '= lhs rhs)})

(def union-comm (eq '#{X Y} '(+ X Y) '(+ Y X)))

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

;; TODO write replace-one

(defn replace-all
  "Replace in expression [expr]
  all occurences of keys from map [m] to its values"
  [expr m]
  (cond (contains? m expr) (m expr)

        (coll? expr)
        (let [[op & args] expr
              args (map #(replace-all % m) args)]
          (cons op args))

        :else expr))

(defn match
  "Try to match an expression [expr] with a pattern [p] with free variables [vars].
  Return map of variables values or [nil] if the pattern doesn't match.
  Already assigned values passed as [vs] map"
  ([expr p vars] (match expr p vars {}))
  ([expr p vars vs]
   (cond (and (symbol? p) (vars p))
         (if-let [val (vs p)]
           (if (= expr val) vs nil)
           (assoc vs p expr))

         (and (symbol? p) (= p expr)) vs

         (and (coll? p) (coll? expr)
              (= (first p) (first expr)))
         (loop [exprs (rest expr)
                ps (rest p)
                vs vs]
           (cond (and (empty? ps) (empty? exprs)) vs
                 (or (empty? ps) (empty? exprs)) nil
                 :else (when-let [vs (match (first exprs) (first ps) vars vs)]
                         (recur (rest exprs) (rest ps) vs)))))))

(defn subexpr-dfs
  "Return sequence of pairs [path expression] for all subexpressions of [expr]"
  ([expr] (subexpr-dfs expr []))
  ([expr path-prefix]
   (lazy-seq
    (cons [path-prefix expr]
          (if (coll? expr)
            (reduce
             concat '()
             (map-indexed
              (fn [idx val]
                (subexpr-dfs val (conj path-prefix (inc idx))))
              (rest expr))))))))

(defn find-subexpr
  "Find the first match of pattern [p]
  through all subexpressions (dfs) of [expr].
  Return map with :path in original expression and matched values :vals"
  [expr p vars]
  (->> (subexpr-dfs expr)
       (map (fn [[path expr]]
              (if-let [vs (match expr p vars)]
                {:path path :vals vs})))
       (some identity)))

(defn apply-eq [expr eq]
  (let [[_ lhs rhs] (:eq eq eq)
        vars (:vars eq #{})]
    (if-let [{:keys [path vals]} (find-subexpr expr lhs vars)]
      (sp/setval [(apply sp/nthpath path)]
                 (replace-all rhs vals)
                 expr)
      expr)))

(defn apply-eq-all
  ([expr eq]
   (let [[_ lhs rhs] (:eq eq eq)
         vars (:vars eq #{})]
     (apply-eq-all expr lhs rhs vars)))

  ([expr lhs rhs vars]
   (cond (symbol? expr)
         (if-let [m (match expr lhs vars)]
           (replace-all rhs m)
           expr)

         (coll? expr)
         (let [[op & args] expr
               args (map #(apply-eq-all % lhs rhs vars) args)
               new-expr (cons op args)]
           (if-let [m (match new-expr lhs vars)]
             (replace-all rhs m)
             new-expr))

         :else expr)))
