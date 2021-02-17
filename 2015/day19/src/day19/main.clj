(ns day19.main
  (:require [aoc-commons.core :as aoc])
  (:require [clojure.string :as str]))


(defn parse-inputs [data-string]
  (let [all-lines (str/split data-string #"\n")
        input-molecule (last all-lines)
        replacements (for [replacement all-lines
                           :while (not (str/blank? replacement))
                           :let [[[_ from to]] (re-seq #"(\w+) => (\w+)" replacement)]]
                       [from to])]
    [replacements input-molecule]))

(defn join-indices [input-parts indices separator]
  (str/join separator (map (partial nth input-parts) indices)))

(defn replace-strand [input-parts insert-idx from to]
  (let [pre-str (join-indices input-parts (range insert-idx) from)
        post-str (join-indices input-parts (range insert-idx (count input-parts)) from)]
    (str
     pre-str
     to
     post-str)))

(defn possible-replacements [input-sequence [from to]]
  (let [padded-input (str "--" input-sequence "--")
        input-parts (str/split padded-input (re-pattern from))]
    (loop [idx 1
           sequences []]
      (if (= idx (count input-parts))
        sequences
        (recur (inc idx) (conj sequences (replace-strand input-parts idx from to)))))))


(defn -main [& args]
  (let [path (first args)
        [replacements input-molecule] (parse-inputs (slurp path))]
    (println replacements)
    (println input-molecule)
    (let [all-replacements (map (partial possible-replacements input-molecule) replacements)
          uniq-replacements (into #{} (flatten all-replacements))]
      (println "uniq replacements" (count uniq-replacements)))))
