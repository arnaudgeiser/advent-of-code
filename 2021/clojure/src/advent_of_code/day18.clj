(ns advent-of-code.day18
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.math.combinatorics :as combo]))

(def content (->> (puzzle 18)
                  (mapv read-string)))

(defn land-left-location [coll path]
  (let [index (last path)
        path' path]
    (cond
      (empty? path)
      nil
      (> index 0)
      (loop [path' (conj (pop path') 0)]
        (if (coll? (get-in coll path'))
          (recur (conj path' 1))
          path'))
      :else
      (recur coll (pop path')))))

(defn land-right-location [coll path]
  (let [path' path]
    (cond
      (empty? path)
      nil
      (zero? (last path'))
      (loop [path' (conj (pop path') 1)]
        (if (coll? (get-in coll path'))
          (recur (conj path' 0))
          path'))
      :else
      (recur coll (pop path')))))

(defn explode? [tuple path]
  (and (>= (count path) 4) (coll? tuple)))

(defn split? [nb]
  (and (int? nb) (>= nb 10)))

(defn try-explode [coll pair path]
  (cond
    (explode? pair path)
    (let [[left right] pair
          left-location (land-left-location coll path)
          right-location (land-right-location coll path)]
      (cond-> (update-in coll path (constantly 0))
        right-location
        (update-in right-location + right)
        left-location
        (update-in left-location + left)))

    (not (coll? pair))
    coll

    :else
    (let [coll-left (try-explode coll (first pair) (conj path 0))
          coll-right (try-explode coll (second pair) (conj path 1))]
      (cond
        (not= coll-left coll)
        coll-left

        (not= coll-right coll)
        coll-right

        :else coll))))

(defn try-split [coll pair path]
  (cond
    (split? pair)
    (update-in coll path (fn [x] [(quot x 2) (+ (quot x 2) (mod x 2))]))

    (not (coll? pair))
    coll

    :else
    (let [split-left (try-split coll (first pair) (conj path 0))]
      (if (= split-left coll)
        (try-split coll (second pair) (conj path 1))
        split-left))))

(defn solve [coll]
  (loop [prev nil
         curr coll]
    (if (= prev curr)
      curr
      (let [exploded (try-explode curr curr [])]
        (recur curr (if (= exploded curr)
                      (try-split curr curr [])
                      exploded))))))

(defn magnitude [v]
  (if (int? v)
    v
    (+ (* 3 (magnitude (first v)))
       (* 2 (magnitude (second v))))))

(defn solve1 []
  (magnitude
   (reduce #(solve (vector %1 %2))
           (solve (first content))
           (rest content))))

(defn solve2 []
  (->> (combo/permuted-combinations content 2)
       (map (partial into []))
       (map solve)
       (map magnitude)
       (sort >)
       (first)))

(solve1)
(solve2)
