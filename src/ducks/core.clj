(ns ducks.core
  (:require [clojure.set])
  (:require [clojure.tools.trace])
  (:gen-class))

(def duck-color-limits
  {:green 3
   :red 4
   :yellow 5})


;; board representation
(def starting-pattern-4
  [
   [nil nil nil nil]
   [{:color :red :n 2} nil {:color :green :n 3} nil]
   [nil {:color :red :n 4} nil {:color :yellow :n 4}]
   ])

(def empty-pattern
  [
   [nil nil nil nil]
   [nil nil nil nil]
   [nil nil nil nil]
   ])


(def all-ducks
  (set
   (for [color (keys duck-color-limits)
         n     (range 1 (inc (duck-color-limits color)))]
     {:color color :n n})))

(defn prev-duck [duck]
  (all-ducks {:n (dec (:n duck)) :color (:color duck)})
  )

(defn next-duck [duck]
  (all-ducks {:n (inc (:n duck)) :color (:color duck)}))


(defn on-board
  "returns location"
  [board duck]
  (when duck
    (first
     (for [i (range (count board))
           j (range (count (first board)))
           :when (= duck (get-in board [i j]))]
       [i j]))))



(defn locations-adjacent
  [loc1 loc2]
  (= 1 (apply + (map #(* %1 %1) (map - loc1 loc2))))
  )

(defn used-ducks [board]
  (set
   (remove empty? (flatten board)))
  )


(defn add-duck;; 🦆
  "returns new board, only allows valid inserts
  location = [row col]"
  [board location duck]
  (let [n1     (next-duck duck)
        n2     (prev-duck duck)
        n1-loc (on-board board n1)
        n2-loc (on-board board n2)]

    (cond
      (seq (get-in board location))                           nil ;; already filled location
      ((used-ducks board) duck)                               nil ;; duck already used
      ;; duck out of seq
      (and n1-loc (not (locations-adjacent location n1-loc))) nil
      (and n2-loc (not (locations-adjacent location n2-loc))) nil
      :else                                                   (assoc-in board location duck))))

(defn empty-locations [board]
  (for [i (range (count board))
        j (range (count (first board)))
        :when (empty? (get-in board [i j]))]
    [i j])
  )

(defn unused-ducks [board]
  (clojure.set/difference all-ducks (used-ducks board))
  )


(defn solve [board]
  (let [-unused-ducks (unused-ducks board)
        d             (first -unused-ducks)]
    (cond
      (empty? -unused-ducks) [board]
      :else                  (apply concat (map solve (remove empty? (for [l (empty-locations board)]
                                                                       (add-duck board l d))))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [solutions-4 (solve starting-pattern-4)
        solutions-all (solve empty-pattern)]
    (println (count solutions-all) "total solutions")
    (println "solution 4"
     (with-out-str (clojure.pprint/pprint  solutions-4)))))

                                        ;
;(clojure.tools.trace/trace-vars solve add-duck)
