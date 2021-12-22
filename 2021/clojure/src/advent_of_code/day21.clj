(ns advent-of-code.day21
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def content (puzzle 21))

(defn infinite-rolls []
  (->> (iterate (fn [i] (if (= i 100) 1(inc i))) 1)
       (partition 3)))

(take 50 (infinite-rolls))

(defn new-position [position score]
  (let [sum (+ position score)]
    (if (zero? (mod sum 10)) 10 (mod sum 10))))

(defn solve []
  (loop [rolls (infinite-rolls)
         player1 [8 0]
         player2 [6 0]
         turn true
         nb-rolls 0]
    (let [[p1-position p1-score] player1
          [p2-position p2-score] player2]
      (if (or (>= p1-score 1000) (>= p2-score 1000))
        [[p1-score p2-score] nb-rolls]
        (let [score (reduce + (first rolls))
              player1' (if turn [(new-position p1-position score) (+ p1-score (new-position p1-position score))] player1)
              player2' (if (not turn) [(new-position p2-position score) (+ p2-score (new-position p2-position score))] player2)]
          (prn (first rolls))
          (recur (rest rolls)
                 player1'
                 player2'
                 (= turn false)
                 (inc nb-rolls)))))))


(let [[score nb-rolls] (solve)]
  (* (apply min score) (* 3 nb-rolls)))
