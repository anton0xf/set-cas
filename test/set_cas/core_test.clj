(ns set-cas.core-test
  (:require [set-cas.core :refer :all]
            [clojure.test :refer :all]))

(deftest test-to-bin
  (is (= '(+) (to-bin '(+))))
  (is (= '(+ A) (to-bin '(+ A))))
  (is (= '(+ A B) (to-bin '(+ A B))))
  (is (= '(+ A (+ B C)) (to-bin '(+ A B C))))
  (is (= '(+ (* A (* B C)) D) (to-bin '(+ (* A B C) D))))
  (is (= '(+ A (+ B (+ C D))) (to-bin '(+ A B C D))))
  (is (= '(+ A (+ B (* C (* D E)))) (to-bin '(+ A B (* C D E)))))
  (is (= '(- (- A B) C) (to-bin '(- A B C))))
  (is (= '(- (- (- A B) C) D) (to-bin '(- A B C D))))
  (is (= '(= A (+ B (+ C D)) E) (to-bin '(= A (+ B C D) E)))))

(deftest test-replace-all
  (is (= 'A (replace-all 'X '{X A})))
  (is (= 'Y (replace-all 'Y '{X A})))
  (is (= '(+ A Y) (replace-all '(+ X Y) '{X A})))
  (is (= '(+ A B) (replace-all '(+ X Y) '{X A, Y B})))
  (is (= '(+ (- A B) Y) (replace-all '(+ X Y) '{X (- A B)})))
  (is (= '(+ A B) (replace-all '(+ X Y) '{X A, Y B})))
  (is (= '(+ A (- A B)) (replace-all '(+ A (- X B)) '{X A})))
  (is (= 'X (replace-all '(+ A B) '{(+ A B) X})))
  (is (= '(+ B A) (replace-all '(+ A B) '{(+ A B) (+ B A)})))
  (is (= '(+ A B) (replace-all '(+ A (- A B)) '{(- A B) B}))))

(deftest test-match
  (is (= {} (match 'A 'A #{})))
  (is (nil? (match 'A 'B #{})))
  (is (= '{X A} (match 'A 'X #{'X})))
  (is (= '{X A, Y 0}
         (match 'A 'X '#{X Y} '{Y 0})))
  (is (= '{X 0} (match 0 'X #{'X})))
  (is (= '{X A} (match 'A 'X '#{X Y})))
  (is (= {'X '(+ A B)} (match '(+ A B) 'X #{'X})))
  (is (= '{X A} (match 'A 'X #{'X} '{X A})))
  (is (nil? (match 'A 'X #{'X} '{X B})))
  (is (= {} (match '(+) '(+) #{})))
  (is (nil? (match '(+) '(-) #{})))
  (is (= {} (match '(+) '(+) #{'X})))
  (is (nil? (match '(+ A) '(+ A B) #{})))
  (is (nil? (match '(+ A) '(+ B) #{'X})))
  (is (nil? (match '(+ A) '(+ B) #{})))
  (is (= {} (match '(+ A) '(+ A) #{})))
  (is (= '{X A} (match '(+ A) '(+ X) #{'X})))
  (is (nil? (match '(+ A) '(+ X) #{'X} '{X B})))
  (is (nil? (match '(+ A B) '(+ X X) #{'X})))
  (is (= '{X A} (match '(+ A A) '(+ X X) #{'X})))
  (is (= '{X A, Y B} (match '(+ A B) '(+ X Y) '#{X Y})))
  (is (= '{X A, Y A} (match '(+ A A) '(+ X Y) '#{X Y})))
  (is (= '{X A, Y B} (match '(+ A B A) '(+ X Y X) '#{X Y})))
  (is (nil? (match '(+ A B B) '(+ X Y X) '#{X Y})))
  (is (= '{X A, Y B} (match '(+ A (- B A)) '(+ X (- Y X)) '#{X Y})))
  (is (nil? (match '(+ A (- B C)) '(+ X (- Y X)) '#{X Y}))))

(deftest test-subexpr-dfs
  (is (= '([[] A]) (subexpr-dfs 'A)))
  (is (= '([[] (+ A)] [[1] A]) (subexpr-dfs '(+ A))))
  (is (= '([[] (+ A B)] [[1] A] [[2] B])
         (subexpr-dfs '(+ A B))))
  (is (= '([[] (+ A B C)] [[1] A] [[2] B] [[3] C])
         (subexpr-dfs '(+ A B C))))
  (is (= '([[] (+ (* A B) (- C D))]
           [[1] (* A B)] [[1 1] A] [[1 2] B]
           [[2] (- C D)] [[2 1] C] [[2 2] D])
         (subexpr-dfs '(+ (* A B) (- C D))))))

(deftest test-find-subexpr
  (is (= {:path [] :vals '{}}
         (find-subexpr 'A 'A #{})))
  (is (= {:path [] :vals '{X A}}
         (find-subexpr 'A 'X #{'X})))
  (is (nil? (find-subexpr '(+ A B) '(+ X X) #{'X})))
  (is (= {:path [] :vals {'X '(+ A B)}}
         (find-subexpr '(+ A B) 'X #{'X})))
  (is (= {:path [] :vals '{X A}}
         (find-subexpr '(+ A A) '(+ X X) #{'X})))
  (is (= {:path [] :vals '{X A, Y B}}
         (find-subexpr '(+ A B) '(+ X Y) '#{X Y})))
  (is (= {:path [1] :vals '{X A, Y B}}
         (find-subexpr '(* (+ A (- B A)) C) '(+ X (- Y X)) '#{X Y})))
  (is (= {:path [2] :vals '{X B, Y A}}
         (find-subexpr '(+ A (- B A)) '(- X Y) '#{X Y})))
  (is (= {:path [2 1] :vals '{X C, Y D}}
         (find-subexpr '(+ A (* (- C D) B)) '(- X Y) '#{X Y}))))

(deftest test-apply-eq
  (is (= '(+ B A) (apply-eq '(+ A B) '(= (+ A B) (+ B A)))))
  (is (= '(+ B A) (apply-eq '(+ A B) union-comm)))
  (is (= '(- A B) (apply-eq '(- A B) union-comm)))
  (is (= '(+ A B C) (apply-eq '(+ A B C) union-comm)))

  (is (= '(* (+ B A) C) (apply-eq '(* (+ A B) C) '(= (+ A B) (+ B A)))))
  (is (= '(* (+ B A) C) (apply-eq '(* (+ A B) C) union-comm)))
  (is (= '(+ (+ B C) A) (apply-eq '(+ A (+ B C))
                                  '(= (+ A (+ B C)) (+ (+ B C) A)))))
  (is (= '(+ A (+ C B)) (apply-eq '(+ A (+ B C))
                                  '(= (+ B C) (+ C B)))))

  (is (= '(+ (+ B C) A) (apply-eq '(+ A (+ B C)) union-comm))))

(deftest test-apply-eq-all
  (is (= '(+ B A) (apply-eq-all '(+ A B) '(= (+ A B) (+ B A)))))
  (is (= '(+ B A) (apply-eq-all '(+ A B) union-comm)))
  (is (= '(- A B) (apply-eq-all '(- A B) union-comm)))
  (is (= '(+ A B C) (apply-eq-all '(+ A B C) union-comm)))

  (is (= '(* (+ B A) C) (apply-eq-all '(* (+ A B) C) '(= (+ A B) (+ B A)))))
  (is (= '(* (+ B A) C) (apply-eq-all '(* (+ A B) C) union-comm)))
  (is (= '(+ (+ B C) A) (apply-eq-all '(+ A (+ B C))
                                      '(= (+ A (+ B C)) (+ (+ B C) A)))))
  (is (= '(+ A (+ C B)) (apply-eq-all '(+ A (+ B C))
                                      '(= (+ B C) (+ C B)))))

  (is (= '(+ (+ C B) A) (apply-eq-all '(+ A (+ B C)) union-comm)))
  (is (= '(+ (+ E D) (* (+ B A) C))
         (apply-eq-all '(+ (* (+ A B) C) (+ D E)) union-comm))))
