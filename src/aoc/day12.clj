(ns aoc.day12
  (:require [aoc.util.grid :as grid]
            [clojure.set :as set]))

(defn explore [grid seen pos]
  (if (contains? seen pos)
    seen
    (let [ns (filter #(= (grid/get grid %) (grid/get grid pos)) (grid/neighbors grid pos))
          updated (conj seen pos)]
      (reduce (fn [seen pos] (explore grid seen pos)) updated ns))))

(defn explore-all [grid poss]
  (if (empty? poss)
    [#{} []]
    (let [[seen patches] (explore-all grid (rest poss))
          pos (first poss)]
      (if (contains? seen pos)
        [seen patches]
        (let [patch (explore grid #{} (first poss))]
          [(set/union patch seen) (conj patches patch)])))))

(defn area [patch] (count patch))

(defn perimeter [patch] (->>
                          (for [pos patch
                                n (map #(grid/move pos %) (vals grid/dir2))
                                :when (not (contains? patch n))]
                            1)
                          (reduce +)))

(defn count-vertical [grid patch row col in-up in-down]
  (if (not (grid/in-bounds grid [row col]))
    0
    (let [now-in-up (and (contains? patch [row col]) (not (contains? patch [(dec row) col])))
          now-in-down (and (contains? patch [row col]) (not (contains? patch [(inc row) col])))
          update-up (if (and now-in-up (not in-up)) 1 0)
          update-down (if (and now-in-down (not in-down)) 1 0)]
      (+ update-up update-down (count-vertical grid patch row (inc col) now-in-up now-in-down)))))

(defn count-horizontal [grid patch col row in-left in-right]
  (if (not (grid/in-bounds grid [row col]))
    0
    (let [now-in-left (and (contains? patch [row col]) (not (contains? patch [row (dec col)])))
          now-in-right (and (contains? patch [row col]) (not (contains? patch [row (inc col)])))
          update-left (if (and now-in-left (not in-left)) 1 0)
          update-right (if (and now-in-right (not in-right)) 1 0)]
      (+ update-left update-right (count-horizontal grid patch col (inc row) now-in-left now-in-right)))))

(defn count-all [grid patch]
  (let [h (count grid)
        w (count (first grid))]
  (+
    (reduce + (map #(count-vertical grid patch % 0 false false) (range h)))
    (reduce + (map #(count-horizontal grid patch % 0 false false) (range w))))))

(defn -main []
  (let [grid (grid/from-string (slurp "input/day12.txt"))
        [_ patches] (explore-all grid (grid/all-indices grid))]
    (println (reduce + (map #(* (area %) (perimeter %)) patches)))
    (println (reduce + (map #(* (area %) (count-all grid %)) patches)))))