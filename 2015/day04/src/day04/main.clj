(ns day04.main
  (:require [aoc-commons.core :refer :all]))

(import java.security.MessageDigest)

(def input-key "yzbqklnj")

; function taken from https://gist.github.com/jizhang/4325757
(defn md5-bytes[s]
  (->> s
       .getBytes
       (.digest (MessageDigest/getInstance "MD5"))))

(defn md5-05? [bytes]
  (and 
   (zero? (first bytes))
   (zero? (second bytes))
   (<= 0 (nth bytes 2) 15)))

(defn md5-06? [bytes]
  (every? zero? (take 3 bytes)))

(defn form-input [num]
  (str input-key num))

(defn key-inputs []
  (map form-input (range)))

(defn num-hash-pairs []
  (map #(vector % (md5-bytes %)) (key-inputs)))

(defn find-numbers [md5-pred]
  (find-first #(md5-pred (second %)) (num-hash-pairs)))

(defn do-part-one []
  (println "lowest num with hash 00000....: " (find-numbers md5-05?)))

(defn do-part-two []
  (println "lowest num with hash 000000...: " (find-numbers md5-06?)))

(defn -main [& args]
  (do-part-one)
  (do-part-two))
