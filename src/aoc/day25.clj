(ns aoc.day25
  (:require [aoc.util.grid :as grid] [clojure.string :as str]))

(defn -main []
  (let [grids (map grid/from-string (str/split (slurp "input/day25.txt") #"\n\n"))
        locks (filter #(= \# (grid/get % [0 0])) grids)
        keys (filter #(= \. (grid/get % [0 0])) grids)
        valid (filter identity (for [lock locks key keys]
                                 (every? #(or (= \. (grid/get lock %)) (= \. (grid/get key %))) (grid/all-indices lock))))]
    (println (count valid))))