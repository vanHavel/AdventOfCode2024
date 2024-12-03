(ns aoc.day3
  (:require [clojure.string :as str]))

(defn sum-of-products [str]
  (let [matches (re-seq #"mul\((\d+),(\d+)\)" str)
        nums (map #(map parse-long (rest %)) matches)
        products (map #(reduce * %) nums)]
    (reduce + products)))

(defn -main []
  (let [input (slurp "input/day3.txt")
        active (str/replace input #"don't\(\)(.|\n)+?do\(\)" "")]
    (println (sum-of-products input))
    (println (sum-of-products active))))