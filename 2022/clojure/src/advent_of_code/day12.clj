(ns advent-of-code.day12
  (:require [advent-of-code.core :refer [puzzle]]))

(def content (puzzle 12))

(def height (count content))
(def width (count (first content)))

(defn eligible? [v1 v2]
  (let [curr (get-in content v1)
        next (get-in content v2)]
    (or
     (and (= curr \S) (#{\a \b} next))
     (and (= next \E) (#{\z \y} curr))
     (<= (- (int next) (int curr)) 1))))

(defn start-points [letters]
  (->> (for [x (range height) y (range width)] [x y])
       (filter #(letters (get-in content %)))))

(defn adjacents [[x1 y1]]
  (->> [[0 1] [0 -1] [1 0] [-1 0]]
       (map (fn [[x2 y2]] [(+ x1 x2) (+ y1 y2)]))
       (remove (fn [[x y]] (or (neg-int? x) (neg-int? y) (>= x height) (>= y width))))))

(defn djikstra [start]
  (loop [unsettled {start 0}
         visited   {}
         found     nil]
    (if (and (not found) (seq unsettled))
      (let [[position cost] (first (sort-by val unsettled))
            adjacents       (adjacents position)
            eligible        (remove (fn [pos] (or (contains? visited pos) (not (eligible? position pos)))) adjacents)]
        (recur (reduce (fn [acc pos]
                         (update acc pos (fnil min Integer/MAX_VALUE) (inc cost)))
                       (dissoc unsettled position)
                       eligible)
               (assoc visited position cost)
               (when (= \E (get-in content position)) cost)))
      found)))

(def solution1 (djikstra (first (start-points #{\S}))))
(def solution2 (reduce min (keep djikstra (start-points #{\S \a}))))
