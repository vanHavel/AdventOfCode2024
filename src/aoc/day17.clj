(ns aoc.day17
  (:require [clojure.string :as str]))

(defn parse [s]
  (let [[fst snd] (str/split s #"\n\n")
        [a b c] (map parse-long (str/split-lines (apply str (filter #(or (Character/isDigit %) (= \newline %)) fst))))
        code (vec (map parse-long (str/split (apply str (filter #(or (Character/isDigit %) (= \, %)) snd)) #",")))]
    {:a a :b b :c c :code code :ip 0}))

(defn run [state]
  (let [{:keys [a b c code ip]} state]
    (if (>= ip (count code))
      '()
      (let [op (nth code ip)
            lit (nth code (inc ip))
            combo (case lit (0, 1, 2, 3) lit 4 a 5 b 6 c)
            next (+ 2 ip)
            updated
        (case op
          0 (assoc state :a (quot a (bit-shift-left 1 combo)) :ip next)
          1 (assoc state :b (bit-xor b lit) :ip next)
          2 (assoc state :b (mod combo 8) :ip next)
          3 (assoc state :ip (if (zero? a) next lit))
          4 (assoc state :b (bit-xor b c) :ip next)
          5 (assoc state :ip next)
          6 (assoc state :b (quot a (bit-shift-left 1 combo)) :ip next)
          7 (assoc state :c (quot a (bit-shift-left 1 combo)) :ip next))]
        (if (= op 5)
          (cons (mod combo 8) (run updated))
          (run updated))))))

(defn reverse-engineer [bits code]
  (let [num (if (empty? bits) 0 (Long/parseLong bits 2))]
    (if (= (count bits) (* 3 (count code)))
      num
      (->>
        (for [b1 "01" b2 "01" b3 "01"
              :let [extension (str b1 b2 b3)
                    option (+ (Long/parseLong extension 2) (* 8 num))
                    result (run {:a option :b 0 :c 0 :code code :ip 0})
                    expected-len (inc (quot (count bits) 3))]
              :when (= (take-last expected-len code) result)]
            (reverse-engineer (str bits extension) code))
        (some identity)))))

(defn -main []
  (let [initial (parse (slurp "input/day17.txt"))
        output (run initial)]
    (println (str/join "," output))
    (println (reverse-engineer "" (:code initial)))))