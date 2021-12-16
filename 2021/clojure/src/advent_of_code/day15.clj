(ns advent-of-code.day15
  (:require [advent-of-code.core :refer [puzzle]]))

(def content (puzzle 15))

(def deltas [[0 1] [0 -1] [1 0] [-1 0]])
(def start [0 0])

(defn goal [cave]
  [(dec (count cave)) (dec (count (first cave)))])

(defn create-cave [content]
  (->> content
       (mapv (fn [line] (mapv #(parse-long (str %)) line)))))

(defn map-cost [cave]
  (->> cave
       (map-indexed (fn [x v] (map-indexed (fn [y v] [[x y] v]) v)))
       (mapcat identity)
       (into {})))

(defn adjacents-fn [cave]
  (let [cost-by-position (map-cost cave)
        goal (goal cave)]
    (fn [[x1 y1]]
      (->> deltas
           (map (fn [[x2 y2]] [(+ x1 x2) (+ y1 y2)]))
           (remove (fn [[x y :as pos]] (or (> x (first goal))
                                           (> y (last goal))
                                           (some neg-int? pos))))
           (map (juxt identity (partial get cost-by-position)))))))

(defn solve [adjacents-fn]
  (let [cheapest #(first (sort-by val %))]
    (loop [unsettled {start 0}
           visited {}]
      (if (seq unsettled)
        (let [[position cost] (cheapest unsettled)
              adjacents (adjacents-fn position)
              eligible (remove (fn [[pos]] (contains? visited pos)) adjacents)]
          (recur (reduce (fn [acc [position' cost']]
                           (update acc position' (fnil min Integer/MAX_VALUE)
                                   (+ cost cost')))
                         (dissoc unsettled position)
                         eligible)
                 (assoc visited position cost)))
        visited))))

(defn solve1 []
  (let [goal (goal content)
        cave (create-cave content)]
    (get (solve (adjacents-fn cave)) goal)))

(defn real-cave [coll size]
  (let [new-size (* size 5)]
    (partition
     new-size
     (for [x (range new-size)
           y (range new-size)
           :let [to-add (+ (quot y size) (quot x size))
                 value (get-in coll [(+ (mod x size))
                                     (+ (mod y size))])
                 new-value (+ value to-add)]]
       (if (> new-value 9)
         (- new-value 9)
         new-value)))))

(defn solve2 []
  (let [cave (create-cave content)
        size (count cave)
        real-cave (real-cave cave size)
        goal (goal real-cave)]
    (get (solve (adjacents-fn real-cave)) goal)))

(solve1)
(solve2)
