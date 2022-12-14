(ns advent-of-code.day14
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 14))

(def rocks-lines (->> content
                      (map #(mapv (fn [e] (mapv parse-long (str/split e #","))) (str/split % #" -> ")))
                      (mapcat (partial partition 2 1))))

(def max-rows (->> (mapcat identity rocks-lines) (map second) (apply max)))

(def initial (->> (for [r (range (inc (+ 2 max-rows)))
                        c (range (inc 10000))]
                    [[c r] \.])
                  (into {})))

(def rocks
  (mapcat (fn [[[c1 r1] [c2 r2]]]
            (if (= c1 c2)
              (mapv #(vector c1 %) (range (min r1 r2) (inc (max r1 r2))))
              (mapv #(vector % r1) (range (min c1 c2) (inc (max c1 c2))))))
        rocks-lines))

(def floor (mapv #(vector % (+ 2 max-rows)) (range 10000)))

(defn init-state [rocks]
  (reduce #(assoc %1 %2 \#) initial rocks))

(def start [500 0])

(defn pour [state [c r :as sand]]
  (let [bottom [c (inc r)]
        left   [(dec c) (inc r)]
        right  [(inc c) (inc r)]]
    (cond
      (= (state bottom) \.) (pour state bottom)
      (nil? (state bottom)) nil  ;; sand reached endless void
      (= (state left) \.)   (pour state left)
      (= (state right) \.)  (pour state right)
      (= sand start)        nil  ;; sand reached source
      :else (assoc state sand \o))))

(defn solve [rocks]
  (reduce (fn [acc x]
            (if-let [res (pour acc start)]
              res
              (reduced x)))
          (init-state rocks)
          (range)))

(def solution1 (solve rocks))
(def solution2 (inc (solve (concat rocks floor))))
