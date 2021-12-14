(ns advent-of-code.day14
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 14))

(def template (first content))

(def initial
  (->> template
       (#(str/split % #""))
       (partition 2 1)
       (mapv str/join)
       (reduce (fn [acc v] (update acc v (fnil inc 0))) {})))

(def mapping
  (->> content
       (drop 2)
       (map #(str/split % #" -> "))
       (map (fn [[[f s :as k] m]] [k [(str f m) (str m s)]]))
       (into {})))

(defn handle-pair [acc [pair nb]]
  (let [pairs (get mapping pair)]
    (reduce (fn [acc m] (update acc m (fnil + 0) nb)) acc pairs)))

(defn step [polymer]
  (reduce handle-pair {} polymer))

(defn count-second-letters [acc [[_ letter] nb]]
  (update acc letter (fnil + 0) nb))

(defn solve [nb-steps]
  (->> (nth (iterate step initial) nb-steps)
       (reduce count-second-letters {})
       (vals)
       (sort >)
       ((juxt first last))
       (apply -)))

(solve 10)
(solve 40)
