(ns advent2022.day4
  (:require
    [clojure.set :as set]
    [clojure.string :as string]))

(def instructions (->> (slurp "resources/day4.txt")
                       (clojure.string/split-lines)
                       (map #(clojure.string/split % #","))))

(def so-pair ["2-9" "6-8"])
(def so-range "2-4")
(defn parse-int
  [s]
  (Integer/parseInt s))
(defn parse-range
  [range]
  (map parse-int (string/split range #"-")))

(defn inclusive-range
  [start end]
  (range start (inc end)))

(defn empty-set-difference?
  [s1 s2]
  (= #{} (set/difference s1 s2)))

(defn does-encompass?
  [pair]
  (let [first-range (into #{} (apply inclusive-range (parse-range (first pair))))
        second-range (into #{} (apply inclusive-range (parse-range (second pair))))]
    (or (empty-set-difference? first-range second-range)
        (empty-set-difference? second-range first-range))
    ))

(prn instructions)
(count (filter true? (map does-encompass? instructions)))

;; pt 2
(defn does-overlap?
  [pair]
  (let [first-range (into #{} (apply inclusive-range (parse-range (first pair))))
        second-range (into #{} (apply inclusive-range (parse-range (second pair))))]
    (not-empty (set/intersection first-range second-range))
    ))

(count (filter #(not (nil? %)) (map does-overlap? instructions)))