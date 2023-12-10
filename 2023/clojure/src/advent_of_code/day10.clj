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

(def bends #{\L \J \7 \F})

(def bend-directions
  {\S [:right :left :top :bottom]
   \L [:top :right]
   \J [:left :top]
   \7 [:left :bottom]
   \F [:bottom :right]
   \- [:left :right]
   \| [:top :bottom]})

(defn find-target [[x y] direction]
  (condp = direction
    :left
    (loop [x x
           y (dec y)]
     (cond
        (= (get-in content [x y]) \-)
        (recur x (dec y))
        (bends (get-in content [x y]))
        [x y]))
    :right
    (loop [x x
           y (inc y)]
      (cond
         (= (get-in content [x y]) \-)
         (recur x (inc y))
         (bends (get-in content [x y]))
         [x y]))
    :top
    (loop [x (dec x)
           y y]
      (cond
         (= (get-in content [x y]) \|)
         (recur (dec x) y)
         (bends (get-in content [x y]))
         [x y]))
    :bottom
    (loop [x (inc x)
           y y]
      (cond
         (= (get-in content [x y]) \|)
         (recur (inc x) y)
         (bends (get-in content [x y]))
         [x y]))))

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
       (recur (update computed (first to-follow) (fnil min Integer/MAX_VALUE) (second to-follow))
              (conj visited (first to-follow))
              unvisited')
       (assoc computed (first to-follow) (second to-follow))))))

(->> (explore)
     (map second)
     (apply max))
