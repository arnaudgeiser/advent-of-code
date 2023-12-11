(ns advent-of-code.day11
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.math.combinatorics :as comb]))

(def content (puzzle 11))

#_
(def content ["...#......"
              ".......#.."
              "#........."
              ".........."
              "......#..."
              ".#........"
              ".........#"
              ".........."
              ".......#.."
              "#...#....."])

(defn spaces? [line]
  (every? (partial = \.) line))

(defn expand [universe]
  (reduce (fn [acc line]
            (cond-> (conj acc line)
              (spaces? line)
              (conj line))) [] universe))

(defn transpose [universe]
  (apply map vector universe))

(def expanded
  (->> content
       expand
       transpose
       expand
       transpose
       vec))

(def locations
 (for [x (range (count expanded))
       y (range (count (first expanded)))
       :let [v (get-in expanded [x y])]
       :when (= v \#)]
   [x y]))

(/ (->> (comb/permuted-combinations locations 2)
      set
      (map (fn [[[x y] [x' y']]] (+ (Math/abs (- x x')) (Math/abs (- y y')))))
      (reduce +)) 2)
