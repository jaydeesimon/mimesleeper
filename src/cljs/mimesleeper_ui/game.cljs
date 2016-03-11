(ns mimesleeper-ui.game
  (:require [clojure.set :refer [difference]]))

;; A board is made up of a two-dimensional vector. Each element of the
;; vector is a map with the following keys:
;;   1. :revealed? Is this block revealed to the player?
;;   2. :adjacent-mine-cnt The sum of the blocks touching this block
;;      that are mines.
;;   3. mine? Is this block a mine?

(defn board-coords
  "A list of all of the coordinates of a board in the form [row col]."
  [board]
  (for [row (range (count board))
        col (range (count (first board)))]
    [row col]))

(defn init-board
  "Initialize a board of size rows, cols."
  [rows cols]
  (let [initial-block {:revealed?         false
                       :adjacent-mine-cnt 0
                       :mine?             false
                       :marked?           nil}
        initial-row (vec (take cols (cycle [initial-block])))]
    (vec (for [_ (range rows)]
           initial-row))))

(defn add-mines
  "Given a board, add mines to random coordinates."
  [board num-mines]
  (let [rand-coords (take num-mines (shuffle (board-coords board)))]
    (reduce (fn [board [row col]]
              (assoc-in board [row col :mine?] true))
            board
            rand-coords)))

(defn surrounding-coords
  "Given a coordinate, return the surrounding coordinates.
  Note that the coordinates may not exist if you applied
  them to a board."
  [row col]
  (let [offsets (for [row (range -1 (inc 1))
                      col (range -1 (inc 1))]
                  [row col])]
    (->> offsets
         (remove #(= % [0 0]))
         (map (fn [[row-offset col-offset]]
                [(+ row-offset row) (+ col-offset col)])))))

(defn surrounding-blocks
  "Given a board and coordinate, return the surrounding
  blocks with their coordinate and contents."
  [board row col]
  (->> (surrounding-coords row col)
       (map (fn [coord]
              {:coord coord :val (get-in board coord)}))
       (filter #(some? (:val %)))))

(defn update-adjacent-mine-count
  "For each block in the board, count and set the
  number of adjacent mines."
  [board]
  (reduce (fn [board [row col]]
            (let [surrounding-blocks (surrounding-blocks board row col)
                  surrounding-mines (filter #(get-in % [:val :mine?]) surrounding-blocks)]
              (assoc-in board [row col :adjacent-mine-cnt] (count surrounding-mines))))
          board
          (board-coords board)))

(defn generate-board
  "Generate a board intended for a new game."
  [rows cols num-mines]
  (-> (init-board rows cols)
      (add-mines num-mines)
      (update-adjacent-mine-count)))

;; TODO: This defn is a little rough on the eyes.
;; Make this easier to understand.
(defn coords-to-reveal
  "Given a starting [row col], recursively find all of the
  related coordinates to reveal."
  [board row col]
  (loop [blocks-to-traverse [[row col]]
         blocks-to-reveal #{}]
    (if (not (seq blocks-to-traverse))
      blocks-to-reveal
      (let [[cur-row cur-col] (first blocks-to-traverse)]
        (if (pos? (get-in board [cur-row cur-col :adjacent-mine-cnt]))
          (recur (rest blocks-to-traverse) (conj blocks-to-reveal [cur-row cur-col]))
          (let [blocks-to-traverse (concat (rest blocks-to-traverse)
                                           (map :coord (surrounding-blocks board cur-row cur-col)))]
            (recur (vec (difference (set blocks-to-traverse) blocks-to-reveal))
                   (conj blocks-to-reveal [cur-row cur-col]))))))))