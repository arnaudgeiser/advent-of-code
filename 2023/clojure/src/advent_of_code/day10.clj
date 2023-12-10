(ns advent-of-code.day10
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 10))

#_
(def content
  ["....."
   ".S-7."
   ".|.|."
   ".L-J."
   "....."])

#_
(def content
  ["..F7."
   ".FJ|."
   "SJ.L7"
   "|F--J"
   "LJ..."])

(def pipes
  (->> (map (fn [coord] [coord (get-in content coord)])
            (for [x (range (count content))
                  y (range (count (first content)))]
              [x y]))
       (into {})))

(def start (ffirst (filter (fn [[_ pipe]] (= pipe \S)) pipes)))

(def bend-directions
  {\S [:right :left :top :bottom]
   \L [:top :right]
   \J [:left :top]
   \7 [:left :bottom]
   \F [:bottom :right]
   \- [:left :right]
   \| [:top :bottom]})

(def bends (disj (set (keys bend-directions)) \- \|))
(def tb-bends (conj bends \|))
(def lf-bends (conj bends \-))

(defn find-target [[x y] direction]
  (condp = direction
    :left
    (when (lf-bends (get-in content [x (dec y)]))
      [x (dec y)])
    :right
    (when (lf-bends (get-in content [x (inc y)]))
      [x (inc y)])
    :top
    (when (tb-bends (get-in content [(dec x) y]))
      [(dec x) y])
    :bottom
    (when (tb-bends (get-in content [(inc x) y]))
      [(inc x) y])))

(defn find-target2 [[x y :as coord] direction cost]
  (when-let [[x' y' :as target] (find-target coord direction)]
    [target (+ cost (Math/abs (- x x')) (Math/abs (- y y')))]))

(defn follow [[coord cost]]
 (->> (map #(find-target2 coord % cost) (bend-directions (get-in content coord)))
      (remove nil?)))
  
(defn explore []
 (loop [computed {}
        visited #{}
        unvisited [[start 0]]]
   (let [to-follow (first unvisited)
         bends (follow to-follow)
         news (remove (fn [[bend]] (visited bend)) bends)
         unvisited' (sort #(compare (second %1) (second %2)) (concat (rest unvisited) news))]
     (if (seq unvisited')
       (recur (update computed (first to-follow) (fnil max 0) (second to-follow))
              (conj visited (first to-follow))
              unvisited')
       (update computed (first to-follow) (fnil max 0) (second to-follow))))))

(->> (explore)
     (map second)
     (apply max))
