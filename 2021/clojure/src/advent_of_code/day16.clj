(ns advent-of-code.day15
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 16))

(defn hex->bytes [s]
  (str/join
   (map (fn [s]
          (let [s (.toString (BigInteger. (str/join s) 16) 2)
                missing-zeros (mod (count s) 8)]
            (if (zero? missing-zeros)
              s
              (str (str/join (repeat (- 8 missing-zeros) "0")) s))))
        (partition 2 s))))

(defn val [s start end]
  (Integer/parseInt (subs s start end) 2))

(defn literal [bytes]
  (loop [sum 0
         rem-bytes bytes]
    (let [is-last (= (first rem-bytes) \0)
          new-sum (if (= 1 (count rem-bytes)) sum
                      (+ (bit-shift-left sum 4) (val rem-bytes 1 (min (count rem-bytes) 5))))]
      (if is-last
        [0 (str/join (drop 5 rem-bytes))]
        (recur new-sum (str/join (drop 5 rem-bytes)))))))


(defn operator-length [bit]
  (if (= bit \0) 15 11))

(declare compute)

(defmulti operator first)

(defmethod operator \0 [bytes]
  (let [length 15
        subpacket-length (val bytes 1 (inc length))
        subpacket (str/join (take subpacket-length (drop (inc length) bytes)))]
    (loop [sum 0
           rem-bytes subpacket]
      (if (empty? rem-bytes)
        [sum (str/join (drop (+ subpacket-length (inc length)) bytes))]
        (let [version (val rem-bytes 0 3)
              type (val rem-bytes 3 6)
              [new-sum rem-bytes'] (compute type (str/join (drop 6 rem-bytes)))]
          (recur (+ sum new-sum version) rem-bytes'))))))

(defmethod operator \1 [bytes]
  (let [nb-subpackets (val bytes 1 12)]
    (loop [sum 0
           nb-subpackets nb-subpackets
           rem-bytes (str/join (drop-bytes bytes 12))]
      (if (zero? nb-subpackets)
        [sum rem-bytes]
        (let [version (val rem-bytes 0 3)
              type (val rem-bytes 3 6)
              [new-sum rem-bytes'] (compute type (str/join (drop 6 rem-bytes)))]
          (recur (+ sum new-sum version)
                 (dec nb-subpackets)
                 rem-bytes'))))))

(def sample "D2FE28")
(def sample2 "38006F45291200")
(def sample3 "EE00D40C823060")
(def sample4 "8A004A801A8002F478")
(def sample5 "620080001611562C8802118E34")
(def sample6 "C0015000016115A2E0802F182340")

(defn drop-bytes [bytes n]
  (str/join (drop n bytes)))

(defn parse-header [bytes]
  [(val bytes 0 3) (val bytes 3 6) (drop-bytes bytes 6)])

(defn compute [type bytes]
  (cond
    (= type 4) (literal bytes)
    :else (operator bytes)))

(loop [bytes (hex->bytes (first content))
       sum 0]
  (if (seq (remove (partial = \0) bytes))
    (let [[version type rem-bytes] (parse-header bytes)
          [sum-version rem-bytes] (compute type rem-bytes)]
      (recur rem-bytes (+ sum sum-version version)))
    sum))

(hex->bytes sample6)
