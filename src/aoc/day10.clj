(ns aoc.day10
  (:require [aoc.util.grid :as grid]))

(defn trailheads [grid pos]
  (let [val (grid/get grid pos)]
    (if (= val 9) [pos] (->>
                           (for [n (grid/neighbors grid pos)
                                 :when (= (grid/get grid n) (inc val))]
                             (trailheads grid n))
                           (apply concat)
                           (into [])))))

(defn -main []
  (let [grid (grid/fmap (grid/from-string (slurp "input/day10.txt")) #(parse-long (str %)))
        trailheadss (map #(trailheads grid %) (grid/find grid 0))]
    (println (reduce + (map count trailheadss)))
    (println (reduce + (map #(count (set %)) trailheadss)))))