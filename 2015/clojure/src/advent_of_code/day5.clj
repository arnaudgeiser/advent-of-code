(ns advent-of-code.day5
  (:require [advent-of-code.core :refer [puzzle]]))

(def content (puzzle 5))

(def vowels #{\a \e \i \o \u})
(def forbidden #{"ab" "cd" "pq" "xy"})

(defn parse-nice [s]
  (reduce
   (fn [{:keys [previous] :as acc} c]
     (if (forbidden (str previous c))
       (reduced false)
       (cond-> (assoc acc :previous c)
         (vowels c)
         (update :vowels conj c)

         (= previous c)
         (assoc :dual true))))
   {:vowels []}
   s))

(defn nice? [s]
  (when-let [{:keys [dual vowels]} (parse-nice s)]
    (and dual
         (> (count vowels) 2))))

(def solution1
 (->> content
      (filter nice?)
      (count)))

(defn parse-nice2 [s]
  (reduce
   (fn [{:keys [previous] :as acc} c]
     (let [[c1 c2 c3] previous]
      (cond-> (update acc :previous #(cons c %))
        (not (and (not= c2 c3) (= c c1 c2)))
        (update :pairs #(conj % (str c1 c)))

        (= c c2)
        (assoc :p1 true))))
   {:previous []
    :pairs []
    :p1 false}
   s))

(defn nice2? [s]
  (when-let [{:keys [p1 pairs]} (parse-nice2 s)]
    (and p1 (seq (filter (fn [[_ i]] (> i 1)) (frequencies pairs))))))

(def solution2
 (->> content
      (filter nice2?)
      (count)))
