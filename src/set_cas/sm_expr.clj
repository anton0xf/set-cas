(ns set-cas.sm-expr
  "convert from S-expression syntax to M-expressn"
  (:require [clojure.string :as str]))

(defn to-m-expr [expr & {parent-op :op}]
  (cond (coll? expr)
        (let [[op & args] expr
              args (map #(to-m-expr % :op op) args)
              s (str/join " " (interpose op args))]
          (if parent-op (format "(%s)" s) s))

        :else (str expr)))

