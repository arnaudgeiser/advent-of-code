(ns advent-of-code.day11
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 11))

(defn ->divisor [line]
  (-> (str/split line #"by ") second parse-long))

(def gcd (->> content
          (filter #(re-find #"by " %))
          (mapv ->divisor)
          (reduce *)))

(defn op->fn [operation]
  (let [[f arg] (str/split operation #" ")
        f' (resolve (symbol f))]
    #(f' (if (= "old" arg) % (parse-long arg)) %)))

(defn turn [{:keys [items op throw-to]} worry-fn]
  (reduce
   (fn [acc item]
     (let [new-item (worry-fn (op item))
           target (throw-to new-item)]
       (update acc target conj (mod new-item gcd))))
   {}
   items))

(defn round [acc worry-fn]
  (reduce
   (fn [acc id]
     (let [monkey (nth acc id)
           items  (:items monkey)
           throws (turn monkey worry-fn)
           monkey'(-> monkey
                     (assoc :items [])
                     (update :inspections + (count items)))]
       (-> (reduce (fn [acc [id vec]] (-> (update-in acc [id :items] concat vec))) acc throws)
           (assoc id monkey'))))
   acc
   (range (count acc))))

(defn ->monkey [lines]
  (let [[_ items-line operation-line test-line true-line false-line] lines
        items (-> (str/split items-line #":") second str/trim (str/split #", ") (->> (mapv parse-long)))
        op (-> (str/split operation-line #"old ") second op->fn)
        divisor (->divisor test-line)
        if-true (parse-long (str (last true-line)))
        if-false (parse-long (str (last false-line)))]
    {:items items
     :op op
     :inspections 0
     :throw-to #(if (zero? (mod % divisor)) if-true if-false)}))

(def monkeys (->> content
                  (partition-all 7)
                  (mapv ->monkey)))

(defn solve [worry-fn rounds]
  (->> (reduce (fn [acc _] (round acc worry-fn)) monkeys (range rounds))
       (map :inspections)
       (sort)
       (reverse)
       (take 2)
       (reduce *)))

(def solution1 (solve #(Math/floor (/ % 3)) 20))
(def solution2 (solve identity 10000))
