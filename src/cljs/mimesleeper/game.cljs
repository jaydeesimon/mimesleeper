(ns mimesleeper.game
  (:require [clojure.set :refer [difference]]))

;; A board is made up of a two-dimensional vector. Each element of the
;; vector is a map with the following keys:
;;   1. :block-state Can be one of #{:not-revealed :revealed :flag :question-mark}
;;   2. :adjacent-mine-cnt The sum of the blocks touching this block
;;      that are mines.
;;   3. :mine? Is this block a mine?
;;   4. :stepped-on-mine? Will be true if the block clicked is a mine.

(defn board-coords
  "A list of all of the coordinates of a board in the form [row col]."
  [board]
  (for [row (range (count board))
        col (range (count (first board)))]
    [row col]))

(defn init-board
  "Initialize a board of size rows, cols."
  [rows cols]
  (let [initial-block {:block-state       :not-revealed
                       :adjacent-mine-cnt 0
                       :mine?             false}
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
              {:coord coord :block (get-in board coord)}))
       (filter #(some? (:block %)))))

(defn update-adjacent-mine-count
  "For each block in the board, count and set the
  number of adjacent mines."
  [board]
  (reduce (fn [board [row col]]
            (let [surrounding-blocks (surrounding-blocks board row col)
                  surrounding-mines (filter #(get-in % [:block :mine?]) surrounding-blocks)]
              (assoc-in board [row col :adjacent-mine-cnt] (count surrounding-mines))))
          board
          (board-coords board)))

(defn generate-board
  "Generate a board intended for a new game."
  ([] (generate-board 30 16 50))
  ([rows cols num-mines]
   (-> (init-board rows cols)
       (add-mines num-mines)
       (update-adjacent-mine-count))))

(defn get-block-coords
  "Returns a sequence of coordinates that satisfy block-pred."
  [board block-pred]
  (filter (fn [[row col]]
            (block-pred (get-in board [row col])))
          (board-coords board)))

(defn mine-revealed?
  "Is there at least one mine revealed on the board?"
  [board]
  (seq (get-block-coords board #(and (:mine? %)
                                     (= (:block-state %) :revealed)))))

;; TODO: This defn is a little rough on the eyes.
;; Make this easier to understand.
(defn traverse-coords
  "Given a starting [row col], recursively traverse the
  coordinates stopping when the a block is adjacent to at
  least one mine."
  [board row col]
  (loop [blocks-to-traverse [[row col]]
         blocks-traversed #{}]
    (if (not (seq blocks-to-traverse))
      blocks-traversed
      (let [[cur-row cur-col] (first blocks-to-traverse)]
        (if (pos? (get-in board [cur-row cur-col :adjacent-mine-cnt]))
          (recur (rest blocks-to-traverse) (conj blocks-traversed [cur-row cur-col]))
          (let [blocks-to-traverse (concat (rest blocks-to-traverse)
                                           (map :coord (surrounding-blocks board cur-row cur-col)))]
            (recur (vec (difference (set blocks-to-traverse) blocks-traversed))
                   (conj blocks-traversed [cur-row cur-col]))))))))
