(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(def does-not-have-value? (complement has-value?))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map (fn [row] (get row col)) board))))

(defn coord-pairs
  ([coords] (for [row coords
                  col coords]
              (vector row col)))
  ([row-coords col-coords] (for [row row-coords
                                 col col-coords]
                             (vector row col))))

(defn top-left [idx]
  (condp >= idx
    2 0
    5 3
    8 6))

(defn block-values [board coord]
  (let [[row col] coord
        top-left-row (top-left row)
        top-left-col (top-left col)
        coord-pairings (coord-pairs (range top-left-row
                                           (+ top-left-row 3))
                                    (range top-left-col
                                           (+ top-left-col 3)))]
    (set (map (fn [co] (value-at board co)) coord-pairings))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn filled? [board]
  (not (some zero? (flatten board))))

(defn rows [board]
  (map (fn [row] (set row)) board))

(defn valid-rows? [board]
  (every? #(= all-values %) (rows board)))

(defn cols [board]
  (map set (apply map vector board)))

(defn valid-cols? [board]
  (every? #(= all-values %) (cols board)))

(defn blocks [board]
  (map (fn [coord] (set (block-values board coord)))
       (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (every? #(= all-values %) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [coord] (does-not-have-value? board coord))
                 (coord-pairs (range 9)))))

(defn solve [board]
  (let [empty-coord (find-empty-point board)]
    (if (empty? empty-coord)
      (if (valid-solution? board)
        board
        [])
      (let [valids (valid-values-for board empty-coord)]
        (for [v valids
              solution (solve (set-value-at board empty-coord v))]
          solution)))))
