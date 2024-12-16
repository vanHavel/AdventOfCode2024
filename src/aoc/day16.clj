(ns aoc.day16
  (:require [aoc.util.grid :as grid] [loom.graph :as graph] [loom.alg :as alg] [clojure.set :as set]))

(def INF 1000000)

(defn node [pos dir]
  (str (first pos) "," (second pos) dir))

(defn get-adj [grid] (->>
  (for [pos (grid/all-indices grid)
        :when (not= \# (grid/get grid pos))
        dir (keys grid/dir2)
        :let [succ (grid/movedir pos dir)
              ahead [(node pos dir) (node succ dir) 1]
              left [(node pos dir) (node pos (grid/turn-left dir)) 1000]
              right [(node pos dir) (node pos (grid/turn-right dir)) 1000]]]
    (if (and (grid/in-bounds grid succ) (not= \# (grid/get grid succ)))
      [ahead left right]
      [left right]))
  (apply concat)))

(defn good [grid forward backward best] (->>
  (for [pos (grid/all-indices grid)
        :when (not= \# (grid/get grid pos))
        dir (keys grid/dir2)
        :let [cur (node pos dir)
              part1 (get forward cur INF)
              part2 (get backward cur INF)]
        :when (= best (+ part1 part2))]
    pos)
  (set)))

(defn -main []
  (let [grid (grid/from-string (slurp "input/day16.txt"))
        g (apply graph/weighted-graph (get-adj grid))
        start (node (first (grid/find grid \S)) :right)
        end (node (first (grid/find grid \E)) :up)
        result (second (alg/dijkstra-path-dist g start end))
        forward-dists (first (alg/bellman-ford g start))
        backward-dists (first (alg/bellman-ford g end))
        seats (good grid forward-dists backward-dists result)]
    (println result)
    (println (count seats))))