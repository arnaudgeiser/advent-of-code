(ns advent-of-code.day3
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.set :as set]))

(def content (puzzle 3))

(def coords
  (for [i (range (count content))
        j (range (count (first content)))]
    [i j]))

(defn neighbors [[x y]]
  (->> (for [x' (range (max 0 (dec x)) (min (count content) (+ x 2)))
             y' (range (max 0 (dec y)) (min (count content) (+ y 2)))]
         [x' y'])
       (into #{})))

(def numbers
  (loop [coll coords
         numbers []
         curr nil]
    (if (empty? coll)
      numbers
      (let [coord (first coll)
            s (str (get-in content coord))]
        (if (re-find #"[0-9]" s)
          (recur (rest coll) numbers (-> curr
                                         (update :number str s)
                                         (update :neighbors (fnil set/union #{}) (neighbors coord))))
          (recur (rest coll) (if-let [number (:number curr)]
                               (conj numbers [(parse-long number) (:neighbors curr)])
                               numbers) nil))))))

(defn coords-of [re]
  (->> (for [coord coords
             :when (re-find re (str (get-in content coord)))]
         coord)
       (into #{})))

(defn solution1 []
  (let [symbols (coords-of #"[^0-9\.]")]
    (->> numbers
         (filter (fn [[_ positions]] (seq (set/intersection positions symbols))))
         (map first)
         (reduce +))))

(defn adjacents [coord]
  (filter (fn [[_ positions]] (positions coord)) numbers))

(defn solution2 []
  (->> (coords-of #"\*")
       (map adjacents)
       (filter #(= (count %) 2))
       (map #(reduce * (map first %)))
       (reduce +)))

(solution1) ;; 538046
(solution2) ;; 81709807
