(ns advent2022.day5)

;    [D]
;[N] [C]
;[Z] [M] [P]
; 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2

(def instructions
  (->> (slurp "resources/day5sample.txt")
       (clojure.string/split-lines)))

(defn count-initial-state [initial-state] initial-state)

(defn split-instructions
  [instructions]
  (let [separator-index (.indexOf instructions "")
        initial-state (->> instructions
                           (take separator-index)
                           (map #(clojure.string/split % #" ")))
        moves (drop (+ 1 separator-index) instructions)]
    (prn initial-state)
    [initial-state moves]))

(defn get-rid-of-empties
  [row]
  (map second (filter #(even? (first %)) (map-indexed vector row))))

(defn put-row-on-stacks
  [stacks row]
  (for [index-and-stack (map-indexed vector stacks)]
    (if (nth row (first index-and-stack) nil)
      (conj (second index-and-stack) (nth row (first index-and-stack)))
      (second index-and-stack))))

(defn build-init-state-stacks
  [instructions]
  (let [init-state (first (split-instructions instructions))
        ships (filter not-empty (last init-state))
        cargo (map get-rid-of-empties (drop-last init-state))
        stacks (for [_ ships] [])]
    (reduce put-row-on-stacks stacks cargo)))

