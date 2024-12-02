(ns aoc.day1
  (:require [clojure.string :as str]))

(defn -main
  []
  (let [input (slurp "input/day1.txt")
        lines (str/split-lines input)
        words (map #(str/split % #" +") lines)
        left (map #(parse-long (first %)) words)
        right (map #(parse-long (nth % 1)) words)
        distances (map #(abs (- %1 %2)) (sort left) (sort right))
        total-distance (reduce + distances)
        right-counts (frequencies right)
        result (reduce (fn [acc x] (+ acc (* x (get right-counts x 0)))) 0 left)]
  (println total-distance)
  (println result)))