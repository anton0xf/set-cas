(ns set-cas.sym-diff
  "Definition and formulas for symmetric difference"
  (:require [set-cas.sm-expr :refer [to-m-expr]]
            [set-cas.core :refer :all]))

(def d-def1 (eq '#{X Y} '(d X Y) '(+ (- X Y) (- Y X))))
(def d-def2 (eq '#{X Y} '(d X Y) '(- (+ X Y) (* Y X))))

(comment
  (to-m-expr (:eq d-def1)) ;; => "X d Y = (X - Y) + (Y - X)"
  (to-m-expr (:eq d-def2)) ;; => "X d Y = (X + Y) - (Y * X)"

  (-> '(d A B C)
      to-bin
      (apply-eq-all d-def1)
      to-m-expr)
  ;; => "(A - ((B - C) + (C - B))) + (((B - C) + (C - B)) - A)"
  )

