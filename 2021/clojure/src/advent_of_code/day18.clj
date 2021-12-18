(ns advent-of-code.day18
  (:require [advent-of-code.core :refer [puzzle]]))

(puzzle 18)


(defn land-left-location [coll path]
  (let [index (last path)
        path' (pop path)]
    (cond
      (empty? (pop path))
      nil
      (> index 0)
      (loop [path' (conj path' 0)]
        (if (coll? (get-in coll path'))
          (recur (conj path' 1))
          path'))
      :else
      (recur coll path'))))

(defn land-right-location [coll path]
  (let [index (last path)
        path' (pop path)]
    (cond
      (empty? path)
      nil
      (zero? index)
      (loop [path' (conj path' 1)]
        (if (coll? (get-in coll path'))
          (recur (conj path' 0))
          path'))
      :else
      (recur coll path'))))

(defn explode? [tuple path]
  (and (= (count path) 3) (coll? tuple)))

(defn process2 [coll [left right :as tuple] path]
  (cond
    (explode? left path)
    (let [left-location (land-left-location coll path)
          right-location (land-right-location coll path)
          v (get-in coll right-location)]
      (-> coll (update-in path (fn [[[_ v2] _]] [(+ v v2) 0]))
          (update-in left-location + (second left))))

    (explode? right path)
    (let [left-location (land-left-location coll (conj path 1))
          right-location (land-right-location coll (conj path 1))
          v (get-in coll left-location)]
      (-> coll (update-in path (fn [[_ [v2 _]]] [(+ v v2) 0]))
          (update-in right-location + (second right))))

    :else
    (cond-> coll
      (coll? left)
      (process2 left (conj path 0))

      (coll? right)
      (process2 right (conj path 1)))))


(def sample [[[[[9,8],1],2],3],4])
(def sample2 [7,[6,[5,[4,[3,2]]]]])

(process2 sample sample [])
(process2 sample2 sample2 [])

(def sample3 [[6,[5,[4,[3,2]]]],1])
(process2 sample3 sample3 [])

(def sample4 [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]])
(process2 sample4 sample4 [])

(def sample5 [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]])
(process2 sample5 sample5 [])

(get-in sample2 [1 1 1])
'

(defn process [coll]
  (loop [i 0
         path []
         acc []]
    (if (= i (count coll))
      acc
      (let [v (nth coll i)]
        (if (seq coll)
          (let [[mid-left mid-right] coll
                left (if (zero? i) 0 (nth coll (dec i)))
                right (nth coll (inc i))]
            (recur (inc i) (conj acc [(+ mid-left left) right])))
          (recur (inc i) (conj acc v)))))))
