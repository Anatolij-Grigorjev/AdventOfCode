(ns day11.main)

(def char-codes-start (int \a))
(def char-codes-end (int \z))
(def blacklist-chars #{\i \o \l})

(defn char2code [c]
  (- (int c) char-codes-start))

(defn code2char [code]
  (char (+ code char-codes-start)))

(defn char-rollover? [c]
  (= (int c) char-codes-start))

(defn inc-char-rollover [c]
  (-> c
      char2code
      inc
      (mod (inc (- char-codes-end char-codes-start)))
      code2char))

(defn growing-pair? [[item1 item2]]
  (= 1 (- item2 item1)))

(defn excludes-chars? [pass excludes]
  (nil? (some excludes pass)))

(defn chars-increasing? [chars-seq]
  (every? growing-pair? (partition 2 1 (map int chars-seq))))

(defn includes-increasing-straight? [pass min-length]
  (boolean (some chars-increasing? (partition min-length 1 pass))))

(defn adjacent-letters-indices [word letters]
  (let [relevant-letters-set (set letters)
        index-letter (map-indexed vector word)
        relevant-idx-letters (filter #(contains? relevant-letters-set (second %)) index-letter)
        grouped-idx-letter-pairs (group-by second relevant-idx-letters)
        grouped-letter-indices (map (fn [[letter idx-letter-pairs]]
                                      [letter (sort (map first idx-letter-pairs))]) grouped-idx-letter-pairs)]
    (filter (fn [[letter indices]]
              (some growing-pair? (partition 2 1 indices))) grouped-letter-indices)))

(defn contains-pairs? [pass num-pairs]
  (let [frequent-letters (filter #(<= 2 (val %)) (frequencies pass))]
    (if (> num-pairs (count frequent-letters))
      false
      (<= num-pairs (count (adjacent-letters-indices pass (keys frequent-letters)))))))

(defn next-password [prev-password]
  (loop [inc-idx (dec (count prev-password))
         pass-mutation (apply vector prev-password)]
    (let [changed-pass (update pass-mutation inc-idx inc-char-rollover)]
      (if (char-rollover? (nth changed-pass inc-idx))
        (recur (dec inc-idx) changed-pass)
        (apply str changed-pass)))))

(defn pass-compliant? [password]
  (and
   (excludes-chars? password blacklist-chars)
   (includes-increasing-straight? password 3)
   (contains-pairs? password 2)))


(defn next-compliant-password [prev-password]
  (loop [current (next-password prev-password)]
    (if (pass-compliant? current)
      current
      (recur (next-password current)))))

(defn -main [& args]
  (let [input (first args)]
    (println "input password:" input)
    (let [next-allowed (next-compliant-password input)]
      (println "Next allowed:" next-allowed)
      (println "Next next allowed:" (next-compliant-password next-allowed)))))
