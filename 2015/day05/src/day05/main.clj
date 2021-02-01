(ns day05.main
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(def naughty-strings ["ab" "cd" "pq" "xy"])
(def vowels "aeiou")


(defn excludes-all? [word, substrings]
  (empty? (filter #(str/includes? word %) substrings)))

(defn has-n-vowels? [word n vowels]
  (>= (count (filter (into #{} vowels) word)) n))

(defn has-letter-repeat-offset?
  ([word] (has-letter-repeat-offset? word 1))
  ([word offset]
   (loop [letters word]
     (let [letter1 (first letters)
           letter2 (nth letters offset nil)]
       (case (= letter1 letter2)
         true true
         false (if (nil? letter2)
                 false
                 (recur (rest letters))))))))


(defn has-repeat-sequence? [word]
  (loop [letters word]
    (let [letter1 (first letters)
          letter2 (second letters)]
      (if (nil? letter2)
        false
        (if (str/includes? (apply str (drop 2 letters)) (str letter1 letter2))
          true
          (recur (rest letters)))))))


(defn is-nice-1? [word]
  (and
   (excludes-all? word naughty-strings)
   (has-n-vowels? word 3 vowels)
   (has-letter-repeat-offset? word)))


(defn is-nice-2? [word]
  (and
   (has-repeat-sequence? word)
   (has-letter-repeat-offset? word 2)))



(defn do-part [words-list niceness-pred]
  (count (filter niceness-pred words-list)))


(defn do-part-one [words-list]
  (do-part words-list is-nice-1?))

(defn do-part-two [words-list]
  (do-part words-list is-nice-2?))

(defn read-input [path]
  (with-open [reader (io/reader path)]
    (doall (map identity (line-seq reader)))))


(defn -main [& args]
  (let [path (first args)
        words-list (read-input path)]
    (println "Parsed file at " path "...")
    (println "Num words total: " (count words-list))
    (println "Num nice words day1: " (do-part-one words-list))
    (println "Num nice words day2: " (do-part-two words-list))))
