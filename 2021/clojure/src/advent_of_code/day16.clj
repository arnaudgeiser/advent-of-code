(ns advent-of-code.day16
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 16))

(defn hex->bytes [s]
  (->> s
       (partition 2)
       (map (fn [s]
              (let [s (.toString (BigInteger. (str/join s) 16) 2)
                    missing-zeros (mod (count s) 8)]
                (if (zero? missing-zeros)
                  s
                  (str (str/join (repeat (- 8 missing-zeros) "0")) s)))))
       (str/join)))

(defn value [s start end]
  (Integer/parseInt (subs s start end) 2))

(defn drop-bytes [n bytes]
  (str/join (drop n bytes)))

(defn literal [bytes]
  (loop [sum 0
         rem-bytes bytes]
    (let [is-last (= (first rem-bytes) \0)
          sum (if (= 1 (count rem-bytes)) sum
                  (+ (bit-shift-left sum 4)
                     (value rem-bytes 1 (min (count rem-bytes) 5))))]
      (if is-last
        [sum (drop-bytes 5 rem-bytes)]
        (recur sum (drop-bytes 5 rem-bytes))))))

(declare compute)

(defmulti operator first)

(defmethod operator \0 [bytes]
  (let [length 15
        subpacket-length (value bytes 1 (inc length))
        subpacket (str/join (take subpacket-length (drop (inc length) bytes)))]
    (loop [packets []
           rem-bytes subpacket]
      (if (empty? rem-bytes)
        [packets (str/join (drop (+ subpacket-length (inc length)) bytes))]
        (let [[value rem-bytes'] (compute rem-bytes)]
          (recur (conj packets value) rem-bytes'))))))

(defmethod operator \1 [bytes]
  (let [nb-subpackets (value bytes 1 12)]
    (loop [packets []
           nb-subpackets nb-subpackets
           rem-bytes (drop-bytes 12 bytes)]
      (if (zero? nb-subpackets)
        [packets rem-bytes]
        (let [[new-sum rem-bytes'] (compute rem-bytes)]
          (recur (conj packets new-sum)
                 (dec nb-subpackets)
                 rem-bytes'))))))

(defn parse-header [bytes]
  [(value bytes 0 3) ;; version
   (value bytes 3 6) ;; type
   (drop-bytes 6 bytes)])

(defn type->fn [type]
  (condp = type
    0 (partial reduce +)
    1 (partial reduce *)
    2 (partial reduce min)
    3 (partial reduce max)
    5 #(if (apply > %) 1 0)
    6 #(if (apply < %) 1 0)
    7 #(if (apply = %) 1 0)))

(defn compute [bytes]
  (let [[_ type bytes] (parse-header bytes)]
    (if (= 4 type)
      (literal bytes)
      (let [[value rem-bytes] (operator bytes)
            f (type->fn type)]
        [(f value) rem-bytes]))))

(->> content
     first
     hex->bytes
     compute
     first)
