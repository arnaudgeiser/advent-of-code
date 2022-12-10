(ns advent-of-code.day10
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 10))

(defn compute []
  (-> (reduce (fn [{:keys [result value]} line]
                (if (= "noop" line)
                  {:result (conj result value)
                   :value value}
                  (let [v (-> (str/split line #" ") second parse-long)]
                    {:result (conj result value value)
                     :value (+ value v)})))
              {:value 1 :result []}
              content)
      :result))

(def solution1
  (let [result (compute)]
    (->> (map (fn [cycle] (* cycle (nth result (dec cycle)))) [20 60 100 140 180 220])
         (reduce +))))

(def solution2
 (->> (map-indexed (fn [i value] [i value]) (compute))
      (reduce (fn [acc [cycle value]]
                (if (<= (dec value) (mod cycle 40) (inc value))
                  (conj acc "#")
                  (conj acc ".")))
              [])
      (partition-all 40)
      (map (partial str/join ""))
      (run! prn)))
