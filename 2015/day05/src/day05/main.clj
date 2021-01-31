(ns day05.main
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(def naughty-strings ["ab" "cd" "pq" "xy"])
(def vowels "aeiou")


(defn excludes-all? [word, substrings]
  (empty? (filter #(str/includes? word %) substrings)))

(defn has-n-vowels? [word n vowels]
  (>= (count (filter (into #{} vowels) word)) n))

(defn has-double-letter? [word]
  (loop [letters word]
    (let [letter1 (first letters)
          letter2 (second letters)]
      (if (nil? letter2)
        false
        (if (= letter1 letter2)
          true
          (recur (rest letters)))))))

(defn is-nice? [word]
  (and
   (excludes-all? word naughty-strings)
   (has-n-vowels? word 3 vowels)
   (has-double-letter? word)))


(defn do-part-one [words-list]
  (count (filter is-nice? words-list)))


(defn read-input [path]
  (with-open [reader (io/reader path)]
    (doall (map identity (line-seq reader)))))


(defn -main [& args]
  (let [path (first args)
        words-list (read-input path)]
    (println "Reading " path "...")
    (println "Num words total: " (count words-list))
    (println "Num nice words: " (do-part-one words-list))))
