(ns aoc.day18
  (:require [aoc.util.grid :as grid]
            [clojure.string :as str]
            [loom.alg :as alg]
            [loom.graph :as graph]))

(def N 71)
(def TAKE 1024)

(defn path-length [bytes]
  (let [stones (set bytes)
        start [0 0]
        goal [(dec N) (dec N)]
        edges (for [x (range N)
                    y (range N)
                    :when (not (contains? stones [y x]))
                    dir (keys grid/dir2)
                    :let [[ny nx] (grid/movedir [y x] dir)]
                    :when (and (< -1 nx N) (< -1 ny N))
                    :when (not (contains? stones [ny nx]))]
                [[y x] [ny nx]])
        g (apply graph/graph edges)
        h (fn [[y x] [ty tx]] (+ (abs (- y ty)) (abs (- x tx))))
        dist (try (alg/astar-dist g start goal h) (catch Exception e -1))]
    dist))

(defn bsta [lo hi bytes]
  (if (= lo hi)
    (nth bytes (dec lo))
    (let [mid (+ lo (quot (- hi lo) 2))]
      (if (= -1 (path-length (take mid bytes)))
        (bsta lo mid bytes)
        (bsta (inc mid) hi bytes)))))

(defn -main []
  (let [lines (str/split-lines (slurp "input/day18.txt"))
        bytes (map (fn [line] (map parse-long (str/split line #","))) lines)]
    (println (path-length (take TAKE bytes)))
    (println (bsta 0 (count bytes) bytes))))