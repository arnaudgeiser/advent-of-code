(ns advent-of-code.day9
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 9))

(def content
  ["0 3 6 9 12 15"
   "1 3 6 10 15 21"
   "10 13 16 21 30 45"])

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

(defn extrapolated-value [lines]
 (let [reversed (reverse lines)]
    (reduce (fn [acc coll]
              (when-not (last coll)
                (prn reversed))
              (+ acc (or (last coll) 0))) 0 reversed)))

(defn solution1 []
  (->> (map (comp expand parse-line) content)
       (map extrapolated-value)
       (reduce +)))

(prn (solution1))
