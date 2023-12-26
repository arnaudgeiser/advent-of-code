(ns advent-of-code.day20
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 20))

(def content
  ["broadcaster -> a, b, c"
   "%a -> b"
   "%b -> c"
   "%c -> inv"
   "&inv -> a"])

#_(def content
    ["broadcaster -> a"
     "%a -> inv, con"
     "&inv -> b"
     "%b -> con"
     "&con -> output"])

(defn parse-line [line]
  (let [[_ source dest] (re-find #"(.*) -> (.*)" line)
        dests (str/split dest #", ")
        name (apply str (rest source))
        type (first source)]
    (case type
      \%
      [name {:dests dests :type :flipflop :state :off}]
      \&
      [name {:dests dests :type :conjunction :memory {}}]
      ["broadcaster" {:dests dests :type :broadcaster}])))

(def targets
  (->> (map parse-line content)
       (mapcat (fn [[module-name {:keys [dests]}]] (map (fn [target] [target module-name]) dests)))
       (reduce (fn [acc [target module]] (update acc target (fnil conj []) module)) {})))

(def machine (->> (map parse-line content)
                  (map (fn [[module-name module]] [module-name (assoc module :memory (into {} (map (fn [dest] [dest :low]) (get targets module-name))))]))
                  (into {})))

(defn update-memory [machine pulse src dest]
  (let [{:keys [type]} (get machine dest)]
    (if (= :conjunction type)
      (update-in machine [dest :memory] assoc src pulse)
      machine)))

(defn flip-flop-module [machine pulse module-name {:keys [dests state]}]
  (reduce (fn [machine dest]
            (let [pulse' (if (= :off state) :high :low)
                  state' (if (= :off state) :on :off)]
              #_(when (= module-name "b")
                  (prn "sending :" module-name " " pulse'  "-> " dest))
              (if (not= pulse :high)
                (-> (update-memory machine pulse' module-name dest)
                    (update module-name assoc :state state'))
                machine)))

          machine
          dests))

(defn conjunction-module [machine module-name src-pulse {:keys [memory dests]}]
  (let [related (vals memory)
        high? (and (seq related) (every? (partial = :high) related))
        pulse (if high? :low :high)]
    {:machine (reduce
               (fn [machine dest]
                 (update-memory machine src-pulse module-name dest))
               machine
               dests)
     :pulse pulse}))

(defn handle-module [machine pulse module-name]
  (loop [machine machine
         queue [[module-name pulse]]
         lows 0
         highs 0]
    (if (seq queue)
      (let [[module-name pulse] (first queue)
            {:keys [type memory state dests] :as module} (get machine module-name)]
        #_(prn (first queue) dests type module-name (get machine module-name))
        (case type
          :broadcaster
          (recur machine
                 (concat (rest queue) (mapv (fn [dest] [dest pulse]) dests))
                 (if (= pulse :low) (inc lows) lows)
                 (if (= pulse :high) (inc highs) highs))
          :flipflop
          (let [pulse' (if (= :off state) :high :low)
                machine (flip-flop-module machine pulse module-name module)]
            (recur
             machine
             (if (= pulse :high) (rest queue) (concat (rest queue) (mapv (fn [dest] [dest pulse']) dests)))
             (if (= pulse :low) (inc lows) lows)
             (if (= pulse :high) (inc highs) highs)))
          :conjunction
          (let [res (conjunction-module machine module-name pulse module)
                machine' (:machine res)
                pulse' (:pulse res)]
            (recur
             machine'
             (concat (rest queue) (mapv (fn [dest] [dest pulse']) dests))
             (if (= pulse :low) (inc lows) lows)
             (if (= pulse :high) (inc highs) highs)))
          (recur machine
                 (rest queue)
                 lows
                 (if (= pulse :high) (inc highs) highs))))
      [machine lows highs])))

(defn press-button [machine]
  (handle-module machine :low "broadcaster"))

(press-button (first (press-button (first (press-button (first (press-button machine)))))))

#_(->> (reduce (fn [[machine lows highs] _]
                 (let [[machine' lows' highs'] (press-button machine)]
                   [machine' (+ lows lows') (+ highs highs')])) [machine 0 0] (range 1000))
       (rest)
       (apply *)
       (prn))
