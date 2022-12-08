(ns advent-of-code.day9
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (->> (puzzle 9)
                  (mapv #(str/split % #" "))
                  (mapv (fn [[o v]] [o (parse-long v)]))))

(defn distance [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (Math/pow (- x1 x2) 2) (Math/pow (- y1 y2) 2))))

(defn close-enough? [v1 v2]
  (< (distance v1 v2) 2))

(defn move-right [[x y]] [x (inc y)])
(defn move-left [[x y]] [x (dec y)])
(defn move-top [[x y]] [(inc x) y])
(defn move-bottom [[x y]] [(dec x) y])

(defn follow [[hx hy :as head] [tx ty :as tail]]
  (if (close-enough? head tail)
    tail
    (cond
      (= hx tx) [tx (if (> hy ty) (inc ty) (dec ty))]
      (= hy ty) [(if (> hx tx) (inc tx) (dec tx)) ty]
      (> hx tx) [(inc tx) (if (< hy ty) (dec ty) (inc ty))]
      :else     [(dec tx) (if (< hy ty) (dec ty) (inc ty))])))

(defn direction->fn [move]
  (case move
    "R" move-right
    "U" move-top
    "L" move-left
    "D" move-bottom))

(defn move [{:keys [rope] :as state}]
  (reduce (fn [{:keys [head] :as state} [pos tail]]
            (let [tail' (follow head tail)]
              (-> state
                  (assoc :head tail')
                  (assoc-in [:rope pos] tail')
                  (update-in [:visited pos] (fnil conj #{}) tail'))))
          state
          (rest (map-indexed (fn [i v] [i v]) rope))))

(defn handle-motion [state [direction value]]
  (let [move-fn (direction->fn direction)]
    (reduce (fn [{:keys [rope] :as state} _]
              (let [head' (move-fn (first rope))
                    state (-> state
                              (assoc-in [:rope 0] head')
                              (assoc :head head'))]
                (move state)))
            state
            (range value))))

(defn make-rope [size]
  (into [] (repeat size [0 0])))

(defn init [size]
  {:visited {} :rope (make-rope size)})

(defn solve [size]
  (-> (reduce handle-motion (init size) content)
      :visited
      (get (dec size))
      (count)))

(def solution1 (solve 2))
(def solution2 (solve 10))
