(ns advent-of-code.day5
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.set :as set]
            [clojure.string :as str]))

(def content (puzzle 5))

(def content
  ["seeds: 79 14 55 13"
   ""
   "seed-to-soil map:"
   "50 98 2"
   "52 50 48"
   ""
   "soil-to-fertilizer map:"
   "0 15 37"
   "37 52 2"
   "39 0 15"
   ""
   "fertilizer-to-water map:"
   "49 53 8"
   "0 11 42"
   "42 0 7"
   "57 7 4"
   ""
   "water-to-light map:"
   "88 18 7"
   "18 25 70"
   ""
   "light-to-temperature map:"
   "45 77 23"
   "81 45 19"
   "68 64 13"
   ""
   "temperature-to-humidity map:"
   "0 69 1"
   "1 0 69"
   ""
   "humidity-to-location map:"
   "60 56 37"
   "56 93 4"])

(def chain [:soil :fertilizer :water :light :temperature :humidity :location])

(def seeds (-> (first content) (str/split #" ") rest (->> (map parse-long))))

(def maps
  (->> content
       (drop 2)
       (partition-by #(= "" %))
       (remove #(= [""] %))
       (map #(map (fn [numbers] (prn numbers) (vec (map parse-long (str/split numbers #" ")))) (rest %)))
       (zipmap chain)))

(defn transport [seed mapping]
  (let [a (-> (keep (fn [[dst src offset]]
                      (when (and (>= seed src)
                                 (<= seed (+ src offset)))
                        (+ dst (- seed src)))) mapping)
              (first))]
    (if (some? a)
      a
      seed)))

(->> (map #(reduce (fn [seed m] (transport seed (m maps))) % chain) seeds)
     (apply min)
     (prn))
