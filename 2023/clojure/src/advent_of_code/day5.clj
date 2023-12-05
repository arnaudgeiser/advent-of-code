(ns advent-of-code.day5
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.set :as set]
            [clojure.string :as str]))

(def content (puzzle 5))

#_(def content
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
       (map #(map (fn [numbers] (vec (map parse-long (str/split numbers #" ")))) (rest %)))
       (map #(map (fn [[dst src offset]] [src (+ src offset) (- dst src)]) %))
       (zipmap chain)))

(defn transport [seed mapping]
  (let [a (-> (keep (fn [[src src-end diff]]
                      (when (and (>= seed src)
                                 (<= seed src-end))
                        (+ seed diff))) mapping)
              (first))]
    (if (some? a)
      a
      seed)))

(defn solution1 []
  (->> (map #(reduce (fn [seed m] (transport seed (m maps))) % chain) seeds)
       (apply min)))

(solution1)

(defn map-transport [[s e] [src src-end diff]]
  (cond-> {}
    (< s src)
    (update :unmapped conj [s (dec (min (inc e) src))])
    (> e src-end)
    (update :unmapped conj [(max (inc src-end) s) e])
    (< (max s src) (min e src-end))
    (update :mapped conj [(+ (max s src) diff) (+ (min e src-end) diff)])))

(defn reduce-transports [{:keys [unmapped mapped]} conversion]
  (reduce (fn [acc' rr]
            (prn "aa:" acc')
            (let [res (map-transport rr conversion)]
              (-> acc'
                  (update :mapped concat (:mapped res))
                  (update :unmapped concat (:unmapped res)))))
          {:mapped mapped}
          unmapped))

(reduce-transports {:unmapped [[21 25] [10 14]] :mapped [[15 20]]} [20 25 0])

(map-transport [10 12] [11 14 0])
(map-transport [10 12] [0 7 0])

(map-transport [10 25] [15 20 0])

(map-transport [10 25] [5 35 0])

(map-transport [10 25] [30 35 0])
(map-transport [10 25] [0 9 0])

(map-transport [10 12] [11 14 0])
(map-transport [10 12] [10 11 0])

(map-transport [10 12] [0 11 0])
(map-transport [10 12] [0 10 0])

(defn solution2 []
  (->> (partition-all 2 2 seeds)
       (map (fn [[seed size]] [seed (+ seed size)]))
       (map (fn [r]
              (reduce (fn [acc m]
                        (prn "concat:" (concat (:mapped acc) (:unmapped acc)))
                        (let [res (reduce reduce-transports {:unmapped (concat (:mapped acc) (:unmapped acc))} (m maps))]
                          (prn m ":" res)
                          res))
                      {:unmapped [r]}
                      chain)))
       (mapcat vals)
       (flatten)
       (apply min)
       #_#_#_(mapcat (fn [[seed size]] (range seed (+ seed size))))
           (map #(reduce (fn [seed m] (transport seed (m maps))) % chain))
         (apply min)))

#_(solution2)
