(ns day14.main
  (:require [aoc-commons.core :refer :all]))


(defn parse-deer [line]
  (let [deer-name (first (re-seq #"\w+" line))
        line-numbers (map parse-int (re-seq #"\d+" line))]
    {:name deer-name
     :speed (first line-numbers)
     :fly (second line-numbers)
     :rest (nth line-numbers 2)}))

(defn travel-and-rest [deer]
  {:cycle-travel (* (:fly deer) (:speed deer)) 
   :cycle-seconds (+ (:fly deer) (:rest deer))})

(defn deer-after [seconds deer]
  (let [deer-travel (travel-and-rest deer)
        full-cycles (quot seconds (:cycle-seconds deer-travel))
        remainder-seconds (mod seconds (:cycle-seconds deer-travel))
        full-cycle-travel (* full-cycles (:cycle-travel deer-travel))
        remainder-travel (reduce + (take remainder-seconds (repeat (:fly deer) (:speed deer))))]
    (+ full-cycle-travel remainder-travel)))

(defn deer-path-str [[deer-name distance]]
  (str distance "->" deer-name))

(defn deer-timetable [to-seconds deer]
  (map #(deer-after % deer) (range to-seconds)))

(defn leading-at-second [timetable s]
  (let [distances-at-s (map (fn [[deer-name times]]
                              [deer-name (nth times s)]) timetable)
        max-value (max-by second distances-at-s)
        leading-times (filter #(= (second max-value) (second %)) distances-at-s)]
    (println "score snapshot" distances-at-s)
    (println "second" s "max" max-value)
    (println "leaders" leading-times)
    (map first leading-times)))

(defn deer-timetables [deer-list to-seconds]
  (zipmap (map :name deer-list) (map (partial deer-timetable to-seconds) deer-list)))

(defn deer-day2-points [deer-list to-seconds]
  (let [timetables (deer-timetables deer-list to-seconds)
        leaders-lists (map (partial leading-at-second timetables) (range 1 to-seconds))]
    (frequencies (flatten leaders-lists))))

(defn -main [& args]
  (let [path (first args)
        race-length 2503
        deers-list (read-input-lines parse-deer path)]
    (println "Competitors:" (map :name deers-list))
    (println "racing for" race-length "seconds...")
    (let [travel-distances (zipmap (map :name deers-list) (map (partial deer-after race-length) deers-list))
          best-deer (first (reverse (sort-by second travel-distances)))]
      (doall (map (comp println deer-path-str) travel-distances))
      (println "Best boy:" (deer-path-str best-deer)))
    (let [day2-points (deer-day2-points deers-list race-length)]
      (println "Day02 breakdowns:" day2-points)
      (println "Most leading boy:" (max-by second day2-points)))))
