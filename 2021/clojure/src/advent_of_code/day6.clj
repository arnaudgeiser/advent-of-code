(ns advent-of-code.day6
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 6))

(defn compute [m]
  (reduce-kv (fn [m k v]
               (if (= k 0)
                 (-> m (assoc 8 v) (update 6 (fnil (partial + v) 0)))
                 (-> m (update (dec k) (fnil (partial + v) 0)))))
             {}
             m))

(defn solve [days]
  (let [content (first (mapv (fn [row] (mapv #(Integer/parseInt %) (str/split row #","))) content))]
    (->> (reduce (fn [acc _] (compute acc)) (frequencies content) (range days))
         (vals)
         (apply +))))

(prn (solve 80))
(prn (solve 256))
