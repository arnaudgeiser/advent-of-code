(ns advent-of-code.day10
  (:require [advent-of-code.core :refer [puzzle]]))

(def content (puzzle 10))

(def h (count content))
(def w (count (first content)))
(def pipes (for [x (range h) y (range w)] [x y]))
(def start (first (filter #(= (get-in content %) \S) pipes)))

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

(defn find-target+cost [[x y :as coord] direction cost]
  (when-let [[x' y' :as target] (find-target coord direction)]
    [target (+ cost (Math/abs (- x x')) (Math/abs (- y y')))]))

(defn follow [[coord cost]]
  (->> (map #(find-target+cost coord % cost) (bend-directions (get-in content coord)))
       (remove nil?)))

(defn explore []
  (loop [computed {}
         visited #{}
         unvisited [[start 0]]]
    (let [to-follow (first unvisited)
          bends (follow to-follow)
          news (remove (fn [[bend]] (visited bend)) bends)
          unvisited' (sort #(compare (second %2) (second %1)) (concat (rest unvisited) news))]
      (if (seq unvisited')
        (recur (update computed (first to-follow) (fnil max 0) (second to-follow))
               (conj visited (first to-follow))
               unvisited')
        (update computed (first to-follow) (fnil max 0) (second to-follow))))))

(defn solution1 []
  (->> (explore)
       (map second)
       (apply max)
       (inc)
       (#(/ % 2))))

(def explored (set (map first (explore))))

(defn crossed? [[x y]]
  (loop [x2 x
         y2 y
         crossed 0]
    (if (and (< x2 h)
             (< y2 w)
             (not (explored [x y])))
      (let [c1 (get-in content [x2 y2])]
        (if (and (explored [x2 y2])
                 (not= \L c1)
                 (not= \7 c1))
          (recur (inc x2)
                 (inc y2)
                 (inc crossed))
          (recur (inc x2)
                 (inc y2)
                 crossed)))
      (odd? crossed))))

(defn solution2 []
  (->> (reduce (fn [acc point] (conj acc (crossed? point))) [] pipes)
       (filter identity)
       (count)))

(solution1) ;; 7097
(solution2) ;; 355
