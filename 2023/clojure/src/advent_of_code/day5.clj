(ns advent-of-code.day5
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 5))

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
            (let [res (map-transport rr conversion)]
              (-> acc'
                  (update :mapped concat (:mapped res))
                  (update :unmapped concat (:unmapped res)))))
          {:mapped mapped}
          unmapped))

(defn solution2 []
  (->> (partition-all 2 2 seeds)
       (map (fn [[seed size]] [seed (+ seed size)]))
       (map (fn [r]
              (reduce (fn [acc m]
                        (let [res (reduce reduce-transports {:unmapped (concat (:mapped acc) (:unmapped acc))} (m maps))]
                          res))
                      {:unmapped [r]}
                      chain)))
       (mapcat vals)
       (flatten)
       (apply min)))

(solution1)  ;; 484023871
(solution2)  ;; 46294175
