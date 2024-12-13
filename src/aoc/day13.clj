(ns aoc.day13
  (:require [clojure.string :as str]))

(defn det [a b c d] (- (* a d) (* b c)))
(defn solve [ax ay bx by cx cy]
  (let [d (det ax bx ay by)
        dx (det cx bx cy by)
        dy (det ax cx ay cy)]
    [(/ dx d) (/ dy d)]))

(defn -main []
  (let [important (apply str (filter #(or (= % \space) (Character/isDigit %)) (slurp "input/day13.txt")))
        numbers (map parse-long (str/split (str/trim important) #" +"))
        systems (partition 6 numbers)
        systems2 (map (fn [[ax ay bx by cx cy]] [ax ay bx by (+ 10000000000000 cx) (+ 10000000000000 cy)]) systems)
        solutions (filter (fn [nums] (every? #(== % (Math/floor %)) nums)) (map (partial apply solve) systems))
        solutions2 (filter (fn [nums] (every? #(== % (Math/floor %)) nums)) (map (partial apply solve) systems2))]
    (println (reduce + (map (fn [[a b]] (+ b (* a 3))) solutions)))
    (println (reduce + (map (fn [[a b]] (+ b (* a 3))) solutions2)))))