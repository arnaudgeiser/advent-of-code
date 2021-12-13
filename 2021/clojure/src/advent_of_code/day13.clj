(ns advent-of-code.day13
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 13))

(defn parse-dots [line]
  (reverse (mapv parse-long (str/split line #","))))

(defn parse-instructions [lines]
  (->> lines
       (remove str/blank?)
       (reduce (fn [acc line]
                 (let [[_ axis at] (re-find #"fold along ([xy])=(\d+)" line)]
                   (if axis
                     (update acc :folds conj [axis (parse-long at)])
                     (update acc :dots conj (parse-dots line)))))
               {:dots []
                :folds []})))

(defn init-dots [max-x max-y]
  (into [] (repeat max-x (mapv (constantly 0) (range max-y)))))

(defn max-for-axis [axis folds]
  (->> folds
       (filter (fn [[axis']] (= axis axis')))
       (map second)
       (first)
       (* 2)))

(defn build-dots [dots folds]
  (let [max-x (max-for-axis "y" folds)
        max-y (max-for-axis "x" folds)]
    (reduce #(assoc-in %1 %2 1) (init-dots (inc max-x) (inc max-y)) dots)))

(defn split-at-y [x dots]
  (let [[fold fold2] (split-at x dots)]
    [fold (reverse fold2)]))

(defn split-at-x [x dots]
  (let [[fold fold2] (->> dots (apply map vector) (split-at x))]
    [(apply mapv vector fold)
     (apply mapv (comp reverse vector) (rest fold2))]))

(defn compose [fold1 fold2]
  (->> (map vector fold1 fold2)
       (mapv #(apply mapv
                     (comp (partial apply +)
                           vector)
                     %))))

(defn solve1 []
  (let [{:keys [dots folds]} (parse-instructions content)]
    (->> (reduce (fn [acc [axis pos]]
                   (let [[fold1 fold2]
                         (condp = axis
                           "x" (split-at-x pos acc)
                           "y" (split-at-y pos acc))]
                     (compose fold1 fold2)))
                 (build-dots dots folds)
                 (subvec folds 0 1))
         (mapcat identity)
         (filter (complement zero?))
         (count))))

(defn solve2 []
  (let [{:keys [dots folds]} (parse-instructions content)]
    (->> (reduce (fn [acc [axis pos]]
                   (let [[fold1 fold2]
                         (condp = axis
                           "x" (split-at-x pos acc)
                           "y" (split-at-y pos acc))]
                     (compose fold1 fold2)))
                 (build-dots dots folds)
                 folds)
         (mapv #(println (str/join " " (map (fn [nb] (if (pos-int? nb) "X" " ")) %)))))))

(solve1)
(solve2)
