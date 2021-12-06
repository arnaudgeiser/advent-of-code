(ns advent-of-code.day3
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 3))

(->> content
     (map (fn [el] (map #(Integer/parseInt %) (str/split el #""))))
     (apply map list)
     (map frequencies)
     (map-indexed (fn [i {zeros 0 ones 1}]
                    (let [pow (dec (- 12 i))]
                      (if (> zeros ones) [pow nil] [nil pow]))))
     (apply map list)
     (map (partial filter identity))
     (map #(reduce (fn [acc pow] (+ acc (Math/pow 2 pow))) 0 %))
     (apply *))

(defn compute [pred]
  (loop [i 0
         start (map (fn [el] (map #(Integer/parseInt %) (str/split el #""))) content)]
    (let [{zeros 0 ones 1 :or {zeros 0 ones 0}} (frequencies (nth (apply map list start) i))
          bit (pred ones zeros)
          current (filter #(= (nth % i) bit) start)]
      (if (= 1 (count current))
        (map-indexed (fn [i e] (* e (Math/pow 2 i))) (reverse (first current)))
        (recur (inc i)
               (filter #(= (nth % i) bit) start))))))

(reduce + (compute #(if (>= %1 %2) 1 0)))
(reduce + (compute #(if (< %1 %2) 1 0)))
