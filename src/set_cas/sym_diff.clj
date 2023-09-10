(ns set-cas.sym-diff
  "Definition and formulas for symmetric difference"
  (:require [set-cas.sm-expr :refer [to-m-expr]]
            [set-cas.core :refer :all]))

(def d-def1 '(= (d X Y) (+ (- X Y) (- Y X))))
(def d-def2 '(= (d X Y) (- (+ X Y) (* Y X))))

(comment
  ;; TODO rm braces around sides of equality
  (to-m-expr d-def1) ;; => "(X d Y) = ((X - Y) + (Y - X))"
  (to-m-expr d-def2) ;; => "(X d Y) = ((X + Y) - (Y * X))"

  (-> '(d A B C)
      to-bin
      (subst d-def1 {'X 'A, 'Y '(d B C)})
      to-m-expr)
  ;; => "(A - (B d C)) + ((B d C) - A)"
  )
