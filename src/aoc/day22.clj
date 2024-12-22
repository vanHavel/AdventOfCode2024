(ns aoc.day22
  (:require [clojure.string :as str]))

(def MODULUS 16777216)
(defn hash [n]
  (let [stage1 (mod (bit-xor n (bit-shift-left n 6)) MODULUS)
        stage2 (mod (bit-xor stage1 (bit-shift-right stage1 5)) MODULUS)
        stage3 (mod (bit-xor stage2 (bit-shift-left stage2 11)) MODULUS)]
    stage3))

(defn sell [prices digits-derivatives]
  (let [[digits changes] digits-derivatives
        first-found (java.util.Collections/indexOfSubList changes prices)]
    (if (= -1 first-found) 0 (get digits (+ 3 first-found)))))

(defn viable [seq]
  (let [n (count seq)]
    (and
      (> (reduce + seq) 0)
      (every? identity (for [i (range (dec n))
                             j (range (inc i) n)
                             :let [sub (take (inc (- j i)) (drop i seq))]]
                        (< -10 (reduce + sub) 10))))))

(defn -main []
  (let [inits (map parse-long (str/split-lines (slurp "input/day22.txt")))
        hashes (map #(take 2001 (iterate hash %)) inits)
        digits-derivatives (map (fn [hs] [(vec (map #(mod % 10) (rest hs))) (vec (map (fn [[a b]] (mod b 10) (- (mod b 10) (mod a 10))) (partition 2 1 hs)))]) hashes)
        ans1 (reduce + (map last hashes))
        price-sequences (filter viable (for [a (range -9 10) b (range -9 10) c (range -9 10) d (range -9 10)] [a b c d]))
        outcomes (map (fn [prices] (reduce + (map (partial sell prices) digits-derivatives))) price-sequences)
        ans2 (apply max outcomes)]
    (println ans1)
    (println ans2)))