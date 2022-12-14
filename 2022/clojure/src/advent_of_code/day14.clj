(ns advent-of-code.day14
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (->> (puzzle 14)))
#_
(def content (str/split-lines (slurp "/home/arnaudgeiser/temp/input")))

(def input (->> content
                (map #(mapv (fn [e] (mapv parse-long (str/split e #","))) (str/split % #" -> ")))
                (mapcat (partial partition 2 1))))

(def max-rows    (->> (mapcat identity input) (map second) (apply max)))
(def max-columns (->> (mapcat identity input) (map first) (apply max)))

(def initial (->> (for [r (range (inc max-rows))
                        c (range (inc max-columns))]
                    [[c r] \.])
                  (into {})))


(def rocks
  (mapcat (fn [[[c1 r1] [c2 r2]]]
            (if (= c1 c2)
              (mapv #(vector c1 %) (range (min r1 r2) (inc (max r1 r2))))
              (mapv #(vector % r1) (range (min c1 c2) (inc (max c1 c2))))))
        input))

(def state (reduce #(assoc %1 %2 \#) initial rocks))

(defn draw [m]
  (doseq [r (range (inc max-rows))]
    (prn (str/join (map #(str (m [% r])) (drop 492 (range (inc max-columns))))))))

(def start [500 0])

(defn pour [state [c r :as sand]]
  (let [bottom [c (inc r)]
        left   [(dec c) (inc r)]
        right  [(inc c) (inc r)]]
    (cond
      (= (state bottom) \.)
      (pour state bottom)

      (nil? (state bottom))
      nil

      (= (state left) \.)
      (pour state left)

      (= (state right) \.)
      (pour state right)

      :else
      (assoc state sand \o))))

(def solution1
  (reduce (fn [acc x]
            (if-let [res (pour acc [500 0])]
              res
              (reduced x)))
          state (range)))
