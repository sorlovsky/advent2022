(ns advent2022.day5)

;[D]
;[N] [C]
;[Z] [M] [P]
;1   2   3
;
;move 1 from 2 to 1
;move 3 from 1 to 3
;move 2 from 2 to 1
;move 1 from 1 to 2

(def instructions (->> (slurp "resources/day5sample.txt")
                       (clojure.string/split-lines)
                       ))

(defn count-initial-state
  [])

(defn split-instructions
  [instructions]
  (let [separator-index (.indexOf instructions "")
        initial-state (take separator-index instructions)
        instructions (drop (+ 1 separator-index) instructions)]
    [initial-state instructions]))