(ns advent-of-code.day4
  (:require [advent-of-code.core :refer [puzzle]]))

(def content (puzzle 4))

(defn combinations [[x y]]
  [(mapv (fn [i] [(+ x i) y]) (range 4))
   (mapv (fn [i] [(- x i) y]) (range 4))
   (mapv (fn [i] [x (+ y i)]) (range 4))
   (mapv (fn [i] [x (- y i)]) (range 4))
   (mapv (fn [i] [(- x i) (- y i)]) (range 4))
   (mapv (fn [i] [(+ x i) (+ y i)]) (range 4))
   (mapv (fn [i] [(- x i) (+ y i)]) (range 4))
   (mapv (fn [i] [(+ x i) (- y i)]) (range 4))])

(defn word [positions]
  (reduce str (map #(get-in content %) positions)))

(def solution1
  (->> (for [x (range (count content))
             y (range (count (first content)))]
         (map word (combinations [x y])))
       (flatten)
       (filter (partial = "XMAS"))
       (count)))

(defn combinations2 [[x y]]
  [[(dec x) (dec y)]
   [(inc x) (inc y)]
   [(inc x) (dec y)]
   [(dec x) (inc y)]])

(def solution2
  (->> (for [x (range (count content))
             y (range (count (first content)))
             :when (= \A (get-in content [x y]))]
         (mapv #(get-in content %) (combinations2 [x y])))
       (filter (fn [[lt rb lb rt]]
                 (and (or (and (= lt \S) (= rb \M))
                          (and (= lt \M) (= rb \S)))
                      (or (and (= rt \S) (= lb \M))
                          (and (= rt \M) (= lb \S))))))
       (count)))
