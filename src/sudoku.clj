(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board [y x]]
  (set (get board y)))

(defn col-values [board [y x]]
  (set (map (fn [y] (value-at board [y x])) #{0 1 2 3 4 5 6 7 8})))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board [y x]]
  (let [round-left (fn [n] (* (quot n 3) 3))
        top-left [(round-left y) (round-left x)]
        box-range (fn [n] (range n (+ n 3)))
        xs (box-range (second top-left))
        ys (box-range (first top-left))]
    (set (for [x xs
          y ys]
      (value-at board [y x])))))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (block-values board coord) (row-values board coord) (col-values board coord)))))

(defn filled? [board]
  (reduce (fn [filled-so-far row] (and filled-so-far (not (contains? (set row) 0)))) true board))

(defn rows [board]
  (map set board))

(defn all-true? [elements]
  (reduce (fn [one other] (and one other)) elements))

(defn contains-all-values? [elements]
  (all-true? (map (fn [a-set] (empty? (set/difference all-values a-set))) elements)))

(defn valid-rows? [board]
  (contains-all-values? (rows board)))

(defn cols [board]
  (map set (apply map vector board)))

(defn valid-cols? [board]
  (contains-all-values? (cols board)))

(defn blocks [board]
  " create a point in every block "
  (for [y (range 0 7 3)
        x (range 0 7 3)]
    (block-values board [y x])))

(defn valid-blocks? [board]
  (contains-all-values? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))


(defn find-empty-point [board]
  (first (filter #(not= (second %) -1) (map-indexed (fn [y x] [y x]) (map (fn [row] (.indexOf row 0)) board)))))

(defn solve [board]
  nil)
