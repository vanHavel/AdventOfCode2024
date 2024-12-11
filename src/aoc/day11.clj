(ns aoc.day11
  (:require [clojure.string :as str]))

(defn succs [n] (let [s (str n) d (count s) half (quot d 2)] (cond
                  (zero? n) [1]
                  (even? d) [(parse-long (subs s 0 half)) (parse-long (subs s half))]
                  :else [(* 2024 n)])))

(defn blink [m] (->>
                  (for [[k v] m s (succs k)] [s v])
                  (reduce (fn [m [k v]] (update m k (fnil + 0) v)) {})))

(defn -main []
  (let [nums (frequencies (map parse-long (str/split (slurp "input/day11.txt") #" ")))
        states (iterate blink nums)]
    (println (reduce + (vals (nth states 25))))
    (println (reduce + (vals (nth states 75))))))