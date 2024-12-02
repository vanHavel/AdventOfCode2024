(ns aoc.day2
  (:require [clojure.string :as str]))

(defn is-safe [seq]
  (let [pairs (partition 2 1 seq)]
    (or (every? #(< (first %) (second %) (+ 4 (first %))) pairs)
        (every? #(> (first %) (second %) (- (first %) 4)) pairs))))

(defn is-safe-2 [seq]
  (let [seqs (for [i (range (count seq))]
               (concat (take i seq) (drop (inc i) seq)))]
    (some is-safe seqs)))

(defn -main []
  (let [input (slurp "input/day2.txt")
        lines (str/split-lines input)
        words (map #(str/split % #" +") lines)
        nums (map #(map parse-long %) words)
        safe (filter is-safe nums)
        safe2 (filter is-safe-2 nums)
        ]
    (println (count safe))
    (println (count safe2))))