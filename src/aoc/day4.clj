(ns aoc.day4
  (:require [aoc.util.grid :as grid]))

(defn -main []
  (let [input (slurp "input/day4.txt")
        grid (grid/from-string input)
        [h w] [(count grid) (count (first grid))]
        dirs [[0 1] [1 0] [1 1] [-1 1]]
        offsetss (map (fn [dir] (take 4 (iterate (fn [pos] (grid/move pos dir)) [0 0]))) dirs)
        xmas (count (for [y (range h) x (range w) offsets offsetss
                          :when (let [poss (map #(grid/move [y x] %) offsets)]
                                  (contains? #{"XMAS" "SAMX"} (apply str (map #(grid/get-or grid % \#) poss))))]
                      true))
        cross [[-1 -1] [-1 1] [0 0] [1 -1] [1 1]]
        cross-mas (count (for [y (range h) x (range w)
                               :when (let [poss (map #(grid/move [y x] %) cross)]
                                       (contains? #{"SSAMM" "SMASM" "MSAMS" "MMASS"} (apply str (map #(grid/get-or grid % \#) poss))))]
                           true))]
    (println xmas)
    (println cross-mas)))