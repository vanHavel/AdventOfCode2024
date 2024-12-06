(ns aoc.day6
  (:require [aoc.util.grid :as grid]))

(defn step [grid pos dir]
  (let [target (grid/move pos (get grid/dir2 dir))]
    (if (= \# (grid/get-or grid target \$))
      (step grid pos (grid/turn-right dir))
      [target dir])))

(defn loop? ([grid pos dir] (loop? grid pos dir #{}))
  ([grid pos dir seen]
  (if-not (grid/in-bounds grid pos)
    false
    (let [[pos1 dir1] (step grid pos dir)]
      (if (contains? seen [pos1 dir1])
        true
        (loop? grid pos1 dir1 (conj seen [pos dir]))))
    )))

(defn -main []
  (let [input (slurp "input/day6.txt")
        grid (grid/from-string input)
        start (first (grid/find grid \^))
        pos-dirs (iterate (fn [[pos dir]] (step grid pos dir)) [start :up])
        path (take-while #(grid/in-bounds grid %) (map first pos-dirs))
        loops (filter #(loop? (grid/adjust grid % \#) start :up) (disj (set path) start))]
    (println (count (set path)))
    (println (count loops))))