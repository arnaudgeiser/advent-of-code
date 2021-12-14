(ns advent-of-code.day14
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 14))

(def initial
  (->> content
       first
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

(defn step [polymer]
  (reduce (fn [acc [pair nb]]
            (let [m (get mapping pair)]
              (reduce (fn [acc m] (update acc m (fnil + 0) nb)) acc m)))
          {} polymer))

(defn execute [nb-steps]
  (loop [i nb-steps
         result initial]
    (if (= i 0)
      result
      (recur (dec i) (step result)))))

(defn solve [nb-steps]
  (->> (execute nb-steps)
       (reduce (fn [acc [[_ v] nb]]
                 (update acc v (fnil + 0) nb))
               {\N 1})
       (vals)
       (sort >)
       ((juxt first last))
       (apply -)))

(solve 10)
(solve 40)
