(ns advent-of-code.day5
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def content (puzzle 5))
(def splitted (split-with #(not= "" %) content))
(def pages (first splitted))
(def updates (map #(str/split % #",") (rest (second splitted))))
(def mapping (reduce #(let [[f s] (str/split %2 #"\|")]
                        (update %1 f (fnil conj #{}) s)) {} pages))

(defn find-position [acc value mapping]
  (loop [i 0]
    (cond
      (or (>= i (count acc))
          (seq (set/intersection
                (set (subvec (vec acc) 0 (inc i)))
                (mapping value))))
      i
      :else
      (recur (inc i)))))

(defn sum-middle [updates]
  (->> (map #(nth % (int (/ (count %) 2))) updates)
       (map parse-long)
       (reduce +)))

(defn reorder [mapping update]
  (reduce (fn [acc page]
            (let [pos (find-position acc page mapping)
                  [l r] (split-at pos acc)]
              (prn pos page)
              (vec (concat l [page] r))))
          [] update))

(defn valid-update? [update]
  (not-any?
   (fn [[index page]]
     (seq (set/intersection (set (subvec update 0 (inc index))) (mapping page))))
   (map-indexed (fn [i v] [i v]) update)))

(def solution1
  (->> (filter valid-update? updates)
       (sum-middle)))

(def solution2
  (let [[pages updates] (split-with #(not= "" %) content)
        updates (map #(str/split % #",") (rest updates))
        mapping (reduce #(let [[f s] (str/split %2 #"\|")]
                           (update %1 f (fnil conj #{}) s)) {} pages)]
    (->> (remove valid-update? updates)
         (map (partial reorder mapping))
         (sum-middle))))
