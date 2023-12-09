(ns advent-of-code.day9
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 9))

(defn parse-line [line]
 (mapv parse-long (str/split line #" ")))

(defn expand [line]
 (loop [acc [line]]
   (let [curr (last acc)]
     (if (every? zero? curr)
       acc
       (let [acc' (partition 2 1 (last acc))
             acc' (map (comp (partial apply -) reverse) acc')]
         (recur (conj acc acc')))))))

(defn extrapolate-forward [lines]
 (let [reversed (reverse lines)]
    (reduce (fn [acc coll]
              (+ acc (or (last coll) 0))) 0 reversed)))

(defn extrapolate-backward [lines]
 (let [reversed (reverse lines)]
    (reduce (fn [acc coll]
              (- (or (first coll) 0) acc)) 0 reversed)))

(defn solve [extrapolate-fn]
  (->> (map (comp expand parse-line) content)
       (map extrapolate-fn)
       (reduce +)))

(def solution1 (solve extrapolate-forward)) ;; 1798691765
(def solution2 (solve extrapolate-backward)) ;; 1104
