(ns aoc.day5
  (:require [clojure.string :as str]))

(defn sort-seq [pairs seq]
  (sort #(if (contains? pairs [%1 %2]) -1 1) seq))

(defn get-ans [seqs]
  (reduce + (map #(nth % (unchecked-divide-int (count %) 2)) seqs)))

(defn -main []
  (let [input (slurp "input/day5.txt")
        [rules rows] (str/split input #"\n\n")
        pairs (set (map #(map parse-long (str/split % #"\|")) (str/split-lines rules)))
        seqs (map #(map parse-long (str/split % #",")) (str/split-lines rows))
        {good-seqs true bad-seqs false} (group-by #(= % (sort-seq pairs %)) seqs)
        sorted-seqs (map #(sort-seq pairs %) bad-seqs)]
    (println (get-ans good-seqs))
    (println (get-ans sorted-seqs))))