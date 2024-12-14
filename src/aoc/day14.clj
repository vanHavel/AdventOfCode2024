(ns aoc.day14
  (:require [clojure.string :as str]))

(def h 103) (def w 101)

(defn parse [s]
  (let [[left right] (str/split s #" ")
        [px py] (map parse-long (str/split (subs left 2) #","))
        [vx vy] (map parse-long (str/split (subs right 2) #","))]
    [[px py] [vx vy]]))

(defn move-robot [robot time]
  (let [[[px py] [vx vy]] robot]
    [(mod (+ px (* time vx)) w) (mod (+ py (* time vy)) h)]))

(defn inside [pos minx maxx miny maxy]
  (let [[x y] pos]
    (and (<= minx x maxx) (<= miny y maxy))))

(defn print-grid [positions]
  (doseq [line (range h)]
    (println (apply str
                    (for [col (range w)] (if (contains? positions [col line]) \# \.))))))

(defn variance [numbers]
  (let [n (count numbers)
        mean (/ (reduce + numbers) n)]
    (/ (reduce + (map (fn [x] (Math/pow (- x mean) 2)) numbers))
       n)))

(defn -main []
  (let [robots (map parse (str/split-lines (slurp "input/day14.txt")))
        goals (map #(move-robot % 100) robots)
        halfx (quot w 2) halfy (quot h 2)
        quadrants [[0 (dec halfx) 0 (dec halfy)] [0 (dec halfx) (inc halfy) (dec h)] [(inc halfx) (dec w) 0 (dec halfy)] [(inc halfx) (dec w) (inc halfy) (dec h)]]
        quad-counts (map (fn [[minx maxx miny maxy]] (count (filter #(inside % minx maxx miny maxy) goals))) quadrants)]
    (println (reduce * quad-counts))
    (doseq [i (range 10000)]
      (let [state (map #(move-robot % i) robots)
            xvar (variance (map first state))]
        (if (< xvar 500)
          (do (println i) (print-grid (set state)))
          nil)))))