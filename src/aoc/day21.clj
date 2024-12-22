(ns aoc.day21
  (:require [aoc.util.grid :as grid]
            [clojure.string :as str]))

(defn update-costs [prev keymap]
  (into {} (for [key (keys keymap)]
             [key (into {} (for [other (keys keymap)]
                             [other (apply min
                                           (for [path (get-in keymap [key other])
                                                 :let [moves (partition 2 1 (concat [\A] path [\A]))]]
                                             (reduce + (map (fn [[start end]] (get-in prev [start end])) moves))))]))])))

(defn pathmap [grid]
  (into {} (for [pos (grid/all-indices grid)
                 :let [key (grid/get grid pos)]
                 :when (not= key \#)]
             [key (into {} (for [other (grid/all-indices grid)
                                 :let [other-key (grid/get grid other)]
                                 :when (not= other-key \#)
                                 :let [ydist (- (first other) (first pos))
                                       xdist (- (second other) (second pos))
                                       ypath (repeat (abs ydist) (if (< ydist 0) :up :down))
                                       xpath (repeat (abs xdist) (if (< xdist 0) :left :right))
                                       seqs (cond
                                              (empty? xpath) [ypath]
                                              (empty? ypath) [xpath]
                                              (= \# (grid/get grid (reduce grid/movedir pos xpath))) [(concat ypath xpath)]
                                              (= \# (grid/get grid (reduce grid/movedir pos ypath))) [(concat xpath ypath)]
                                              :else [(concat xpath ypath) (concat ypath xpath)])]]
                        [other-key seqs]))])))

(defn get-best-seqs [pad code cur]
  (if (empty? code)
    [[]]
    (let [[c & cs] code
          rec (get-best-seqs pad cs c)]
      (mapcat (fn [seq] (map (fn [path] (concat path (cons \A seq))) (get-in pad [cur c]))) rec))))


(defn -main []
  (let [codes (str/split-lines (slurp "input/day21.txt"))
        numeric (map (fn [code] (parse-long (apply str (filter #(Character/isDigit %) code)))) codes)
        numpad (pathmap (grid/from-string "789\n456\n123\n#0A"))
        arrowgrid [[\# :up \A][:left :down :right]]
        arrowpad (pathmap arrowgrid)
        r1s (map #(get-best-seqs numpad % \A) codes)
        r2s (map (fn [seqs] (mapcat #(get-best-seqs arrowpad % \A) seqs)) r1s)
        r3s (map (fn [seqs] (mapcat #(get-best-seqs arrowpad % \A) seqs)) r2s)
        bests (map (fn [seqs] (apply min (map count seqs))) r3s)
        res (reduce + (map * numeric bests))
        initial-costs (into {} (for [key (keys arrowpad)]
                                 [key (into {} (for [other (keys arrowpad)]
                                                 [other 1]))]))
        later-costs (iterate (fn [costs] (update-costs costs arrowpad)) initial-costs)
        relevant-costs (nth later-costs 25)
        bests-2 (map #(apply min (map (fn [seq] (reduce + (map (partial get-in relevant-costs) (partition 2 1 (cons \A seq))))) %)) r1s)
        res-2 (reduce + (map * numeric bests-2))]
    (println res)
    (println res-2)))