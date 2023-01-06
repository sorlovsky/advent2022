(ns advent2022.day5)

;    [D]
;[N] [C]
;[Z] [M] [P]
; 1   2   3
;
;move 1 from 2 to 1
;move 3 from 1 to 3
;move 2 from 2 to 1
;move 1 from 1 to 2

(def instructions
  (->> (slurp "resources/day5.txt")
       (clojure.string/split-lines)))

;(defn count-initial-state [initial-state] initial-state)

(defn split-instructions
  [instructions]
  (let [separator-index (.indexOf instructions "")
        initial-state (->> instructions
                           (take separator-index)
                           (map #(clojure.string/split % #" ")))
        moves (drop (+ 1 separator-index) instructions)]
    [initial-state moves]))

(defn get-rid-of-empties
  [row]
  (:stack
   (reduce
    (fn [coll val]
      (let [val (-> val
                    (clojure.string/replace #"\[" "")
                    (clojure.string/replace #"\]" ""))]
        (if (= val "")
          (if (= (:prev coll) ["" "" ""])
            (assoc (assoc coll :stack (conj (:stack coll) val)) :prev [])
            (assoc coll :prev (conj (:prev coll) val)))
          (assoc (assoc-in coll [:stack] (conj (:stack coll) val)) :prev [val]))))
    {:stack [] :prev []}
    row)))

(defn put-row-on-stacks
  [stacks row]
  (for [index-and-stack (map-indexed vector stacks)]
    (if (nth row (first index-and-stack) nil)
      (conj (second index-and-stack) (nth row (first index-and-stack)))
      (second index-and-stack))))

(defn execute-instruction
  [instruction stacks]
  (prn stacks)
  (prn instruction)

  (let [parsed-instruction (map
                            #(Integer/parseInt %)
                            (filter #(or (= (first %) \1)
                                         (= (first %) \2)
                                         (= (first %) \3)
                                         (= (count %) 1))
                                    (clojure.string/split instruction #" ")))
        number-to-move (first parsed-instruction)
        start-index (- (second parsed-instruction) 1)
        end-index (- (nth parsed-instruction 2) 1)
        start-stack (into [] (nth stacks start-index))
        end-stack (into [] (nth stacks end-index))
        moving-pieces (reverse (take number-to-move start-stack))
        updated-end-stack (into [] (concat moving-pieces end-stack))
        updated-start-stack (into [] (drop number-to-move start-stack))]
    (assoc stacks start-index updated-start-stack end-index updated-end-stack)))

(defn build-init-state-stacks
  [instructions]
  (let [init-state (first (split-instructions instructions))
        moves (second (split-instructions instructions))
        ships (filter not-empty (last init-state))
        _ (drop-last init-state)
        cargo (map get-rid-of-empties (drop-last init-state))
        stacks (for [_ ships] [])
        stacks (into []
                     (map #(into [] (filter seq %))
                          (reduce put-row-on-stacks stacks cargo)))]
    (reduce (fn [coll val] (execute-instruction val coll)) stacks moves)))



;(nth parsed-instruction 2)
[["[Z]" "[N]"] ["[M]" "[C]" "[D]"] ["[P]"]]

(clojure.string/join (map first (build-init-state-stacks instructions)))
; guessed BLGNRZVCZ