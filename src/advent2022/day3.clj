(ns advent2022.day3)

(def instructions (clojure.string/split-lines
                    (slurp "resources/day3.txt")))

(defn split-in-half
  [s]
  (let [l (count s)
        midpoint (/ l 2)]
    (split-at midpoint s)))

(defn get-priority
  [letter]
  (let [int-val-of-letter (int letter)]
    (if (>= int-val-of-letter 97)
      (- int-val-of-letter 96)
      (- int-val-of-letter 38))))

(defn calculate-intersection
  [lists]
  (let [first-half (into #{} (first lists))
        second-half (into #{} (second lists))]
    (clojure.set/intersection first-half second-half)))

(map calculate-intersection (map split-in-half instructions))

(map #(reduce + (map get-priority %))
     (map calculate-intersection
          (map split-in-half instructions)))

(reduce + (map #(reduce + (map get-priority %))
               (map calculate-intersection
                    (map split-in-half instructions))))

;; part 2
(def instructions (clojure.string/split-lines
                    (slurp "resources/day3.txt")))
(reduce + (map #(reduce + (map get-priority %))
               (map #(apply clojure.set/intersection %)
                    (partition 3 (map #(->> (seq %)
                                            (into #{})) instructions)))))
