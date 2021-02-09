(ns day10.main
  (:require [aoc-commons.core :refer :all]))

(defn seq-freq-list [num-str]
  (loop [symbols num-str
         curr-freq 1
         occurs []]
    (let [curr (first symbols)
          remain (rest symbols)
          next-sym (first remain)]
      (if (nil? curr)
        occurs
        (if (= curr next-sym)
          (recur remain (inc curr-freq) occurs)
          (recur remain 1 (conj occurs [curr-freq curr])))))))

(defn next-look-say [prev]
  (apply str (flatten (seq-freq-list prev))))

(defn look-and-say
  ([] (look-and-say "1"))
  ([seed] 
   (iterate next-look-say seed)))

(defn -main [& args]
  (let [input (first args)
        times-day1 40
        times-day2 50]
    (println "look-and-say input:" input)
    (println "after" times-day1 "iterations it develops length " (count (nth (look-and-say input) times-day1)))
    (println "after" times-day2 "iterations it develops length " (count (nth (look-and-say input) times-day2)))))
