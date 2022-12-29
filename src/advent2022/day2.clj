(ns advent2022.day2)

(def instructions (map #(clojure.string/split % #" ")
                             (clojure.string/split-lines
                               (slurp "resources/day2.txt"))))
(def rock "rock")
(def paper "paper")
(def scissors "scissors")

(defn translate
  [move]
  (get
    {"X" rock
     "Y" paper
     "Z" scissors
     "A" rock
     "B" paper
     "C" scissors} move))

(def point-map {rock 1
                paper 2
                scissors 3})

(defn get-outcome
  [my-move their-move]
  (cond
    (= my-move their-move) 3
    (or
      (and (= my-move paper) (= their-move rock))
      (and (= my-move scissors) (= their-move paper))
      (and (= my-move rock) (= their-move scissors))) 6
    :else 0)
  )

(defn get-points
  [game]
  (let [opponent (translate (first game))
        mine (translate (second game))]
    (+ (get point-map mine) (get-outcome mine opponent))))

(reduce + (map get-points instructions))

;; pt. 2
(def basic-instructions (map #(clojure.string/split % #" ")
                       (clojure.string/split-lines
                         (slurp "resources/day2sample.txt"))))

(def win-map
  {rock paper
   scissors rock
   paper scissors})

(def lose-map
  (clojure.set/map-invert lose-map))

(defn get-points-for-desired-outcome
  [game]
  (let [opponent-move (first game)
        outcome (second game)]
    (prn (get point-map (get lose-map (translate opponent-move))))
    (cond (= outcome "Y") (+ 3 (get point-map (translate opponent-move)))
          (= outcome "X") (get point-map (get lose-map (translate opponent-move)))
          :else (+ 6 (get point-map (get win-map (translate opponent-move)))))))

(reduce + (map get-points-for-desired-outcome instructions))