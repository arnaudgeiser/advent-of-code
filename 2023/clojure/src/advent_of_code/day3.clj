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
                                         (update :positions (fnil conj #{}) coord)))
          (recur (rest coll) (if-let [number (:number curr)]
                               (conj numbers [(parse-long number) (:positions curr)])
                               numbers) nil))))))

(def symbols
  (->> (for [coord coords
             :when (re-find #"[^0-9\.]" (str (get-in content coord)))]
         coord)
       (into #{})))

(defn solution1 []
  (->> numbers
       (filter (fn [[_ positions]] (some (fn [position] (seq (set/intersection (neighbors position) symbols))) positions)))
       (map first)
       (reduce +)))

(def gears
  (->> (for [coord coords
             :when (re-find #"\*" (str (get-in content coord)))]
         coord)
       (into #{})))

(defn adjacents [coord]
  (filter (fn [[_ positions]] (some #((neighbors coord) %) positions)) numbers))

(defn solution2 []
  (->> gears
       (map adjacents)
       (filter #(= (count %) 2))
       (map #(reduce * (map first %)))
       (reduce +)))

(solution1) ;; 538046
(solution2) ;; 81709807
