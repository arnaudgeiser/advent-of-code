(ns advent-of-code.day8
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(def content (puzzle 8))

(defn split [s]
  (str/split s #" "))

(def input (map (fn [line]
                  (map split (str/split line #" \| ")))
                content))


(defn solve1 []
  (->> input
       (mapcat second)
       (filter #(#{2 3 4 7} (count %)))
       (count)))

(def letters "abcdefg")

(def signal->digit {"abcefg" 0
                    "cf" 1
                    "acdeg" 2
                    "acdfg" 3
                    "bcdf" 4
                    "abdfg" 5
                    "abdefg" 6
                    "acf" 7
                    "abcdefg" 8
                    "abcdfg" 9})

(def mapping (set (keys signal->digit)))

(defn solve-line [[signals output]]
  ;; Compute every possible permutation with the available letters... Yes, it's not optimal...
  (loop [permutations (combo/permutations letters)]
    ;; Create a mapping digit to digit with the current permutation
    (let [digit->digit (zipmap letters (first permutations))
          ;; Function to convert the current permutation signal into the base one
          signal->signal-fn (fn [signal] (str/join (sort (map (partial get digit->digit) [signal]))))
          ;; Compute a set corresponding to the `mapping` defined above
          mapped-signals (set (map signal->signal-fn signals))]
      (if (= mapping mapped-signals)
        ;; Convert the signal into its corresponding digits
        (let [digits (map (fn [signal] (get signal->digit (signal->signal-fn signal))) output)
              number-as-string (str/join digits)]
          (parse-long number-as-string))
        (recur (rest permutations))))))

(defn solve2 []
  (->> input
       (map solve-line)
       (reduce +)))

(solve1)
(solve2)
