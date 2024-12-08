(ns aoc.day8
  (:require [aoc.util.grid :as grid]))

(defn get-antinodes [poss] (->>
    (for [p poss q poss :when (< 0 (compare p q)) :let [diff (grid/diff p q)]]
      [(grid/move p diff) (grid/move q (grid/invert diff))])
    (apply concat)))

(defn get-antinodes-2 [grid poss] (->>
    (for [p poss q poss :when (< 0 (compare p q)) :let [diff (grid/diff p q)]
          :let [forward (iterate #(grid/move % diff) p) backward (iterate #(grid/move % (grid/invert diff)) q)]]
      (mapcat #(take-while (partial grid/in-bounds grid) %) [forward backward]))
    (apply concat)))

(defn -main []
  (let [grid (grid/from-string (slurp "input/day8.txt"))
        clusters (filter #(not= \. (first %)) (group-by #(grid/get grid %) (grid/all-indices grid)))
        antinodes (set (apply concat (for [[_ poss] clusters] (get-antinodes poss))))
        antinodes-2 (set (apply concat (for [[_ poss] clusters] (get-antinodes-2 grid poss))))]
    (println (count (filter #(grid/in-bounds grid %) antinodes)))
    (println (count antinodes-2))))