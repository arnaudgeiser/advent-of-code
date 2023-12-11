(ns advent-of-code.day11
  (:require [advent-of-code.core :refer [puzzle]]))

(def content (puzzle 11))

(defn spaces? [line] (every? (partial = \.) line))
(defn transpose [universe] (apply map vector universe))
(defn empties [universe] (vec (keep-indexed #(when (spaces? %2) %1) universe)))
(def empty-rows (empties content))
(def empty-columns (empties (transpose content)))

(def locations
  (for [x (range (count content))
        y (range (count (first content)))
        :let [v (get-in content [x y])]
        :when (= v \#)]
    [x y]))

(defn expansions [[x y] [x' y']]
  (->> (concat (filter #(< (min x x') % (max x x')) empty-rows)
               (filter #(< (min y y') % (max y y')) empty-columns))
       (count)))

(defn solve [expansion-factor]
  (->> (for [a locations b locations :when (pos? (compare a b))] [a b])
       (map set)
       (set)
       (map #(into [] %))
       (map (fn [[[x y :as c] [x' y' :as c2]]]
              (+ (Math/abs (- x x'))
                 (Math/abs (- y y'))
                 (* (dec expansion-factor) (expansions c c2)))))
       (reduce +)))

(solve 2)
(solve 1000000)
