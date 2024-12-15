(ns aoc.day15
  (:require [aoc.util.grid :as grid] [clojure.set :as set] [clojure.string :as str]))

(defn transform-row [row]
  (if (empty? row)
    '()
    (let [[head & rest] row
          [s1 s2] (case head \. [\. \.] \# [\# \#] \@ [\@ \.] \O [\[ \]])]
      (cons s1 (cons s2 (transform-row rest))))))

(defn affected [grid pos dir]
  (let [succ (grid/movedir pos dir)]
    (case (grid/get grid pos)
      (\. \#) #{pos}
      \[ (case dir
           (:up :down) (set/union #{pos (grid/movedir pos :right)} (affected grid succ dir) (affected grid (grid/movedir succ :right) dir))
           (:left :right) (conj (affected grid succ dir) pos))
      \] (case dir
           (:up :down) (set/union #{pos (grid/movedir pos :left)} (affected grid succ dir) (affected grid (grid/movedir succ :left) dir))
           (:left :right) (conj (affected grid succ dir) pos))
      \O (conj (affected grid succ dir) pos))))

(defn shift [grid pos dir affected]
  (let [succ (grid/movedir pos dir)]
    (if (not (contains? affected succ))
      grid
      (-> grid
        (grid/adjust succ (grid/get grid pos))
        (grid/adjust pos \.)))))

(defn update [robot grid dir]
  (let [newbot (grid/movedir robot dir)
        tentative (affected grid newbot dir)
        sorted (case dir
                 :up (sort-by first tentative)
                 :down (reverse (sort-by first tentative))
                 :left (sort-by second tentative)
                 :right (reverse (sort-by second tentative)))]
    (if (some #(= \# %) (map #(grid/get grid %) tentative))
      [robot grid]
      [newbot (reduce (fn [grid pos] (shift grid pos dir tentative)) grid sorted)])))

(defn -main []
  (let [[maze instructions] (str/split (slurp "input/day15.txt") #"\n\n")
        grid (grid/from-string maze)
        grid2 (vec (map #(vec (transform-row %)) grid))
        dirs (map grid/dir-from-char (filter #(not= \newline %) instructions))

        robot (first (grid/find grid \@))
        robot2 (first (grid/find grid2 \@))
        grid (grid/adjust grid robot \.)
        grid2 (grid/adjust grid2 robot2 \.)

        target (second (reduce (fn [[robot grid] dir] (update robot grid dir)) [robot grid] dirs))
        target2 (second (reduce (fn [[robot grid] dir] (update robot grid dir)) [robot2 grid2] dirs))
        gps (map (fn [[y, x]] (+ x (* y 100))) (grid/find target \O))
        gps2 (map (fn [[y, x]] (+ x (* y 100))) (grid/find target2 \[))]
    (println (reduce + gps))
    (println (reduce + gps2))))