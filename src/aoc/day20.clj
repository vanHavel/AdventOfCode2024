(ns aoc.day20
  (:require [aoc.util.grid :as grid]))

(defn bfs [grid start]
  (let [visited (volatile! #{start})
        distances (volatile! {start 0})
        queue (volatile! (conj clojure.lang.PersistentQueue/EMPTY start))]
    (while (not-empty @queue)
      (let [top (peek @queue)
            d (get @distances top)]
        (vswap! queue pop)
        (doseq [dir (keys grid/dir2)
                :let [n (grid/movedir top dir)]
                :when (not= \# (grid/get grid n))
                :when (not (contains? @visited n))]
          (vswap! visited conj n)
          (vswap! distances assoc n (inc d))
          (vswap! queue conj n))))
    @distances))

(defn get-cheats [start-dists end-dists normal length target]
  (for [[y1 x1] (keys start-dists)
        :let [pos [y1 x1]]
        y2 (range (- y1 length) (+ y1 (inc length)))
        :let [diffy (grid/manhattan [y2] [y1])]
        x2 (range (- x1 (- length diffy)) (+ x1 (inc (- length diffy))))
        :let [pos2 [y2 x2]]
        :when (contains? end-dists pos2)
        :let [dist (grid/manhattan pos pos2)
              maybe (+ dist (get start-dists pos) (get end-dists pos2))
              saved (- normal maybe)]
        :when (>= saved target)]
    saved))

(defn -main []
  (let [grid (grid/from-string (slurp "input/day20.txt"))
        start (first (grid/find grid \S))
        end (first (grid/find grid \E))
        start-dists (bfs grid start)
        end-dists (bfs grid end)
        normal (get start-dists end)
        cheats (get-cheats start-dists end-dists normal 2 100)
        cheats2 (get-cheats start-dists end-dists normal 20 100)]
    (println (count cheats))
    (println (count cheats2))))