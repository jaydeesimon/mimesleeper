(ns mimesleeper.event
  (:require [mimesleeper.game :as game]))

(defmulti update-board-state! (fn [event board row col]
                          (cond
                            (= :new-game event) :new-game
                            (game/game-won? @board) :game-won
                            (game/game-lost? @board) :game-lost
                            (and (= :reveal-block event) (game/all-unrevealed? @board)) :generate-board
                            :else event)))

(defn reveal-coords [board coords]
  (reduce (fn [board' [row col]]
            (assoc-in board' [row col :block-state] :revealed))
          board
          coords))

(defn stepped-on-coords [board coords]
  (reduce (fn [board' [row col]]
            (assoc-in board' [row col :stepped-on-mine?] true))
          board
          coords))

(defn reveal-block [board row col]
  (let [new-board (if (get-in @board [row col :mine?])
                    (-> (assoc-in @board [row col :stepped-on-mine?] true)
                        (reveal-coords (game/get-block-coords @board :mine?)))
                    (reveal-coords @board
                                   (filter (fn [[row col]]
                                             (= :not-revealed (get-in @board [row col :block-state])))
                                           (game/traverse-coords @board row col))))]
    (reset! board new-board)))


(defmethod update-board-state! :reveal-block [_ board row col]
  (reveal-block board row col))

(defmethod update-board-state! :toggle-block-state [_ board row col]
  (let [new-board (update-in @board
                             [row col]
                             (fn [block]
                               (let [next-block-state (condp = (:block-state block)
                                                        :not-revealed (if (game/flags-left? @board) :flag :question-mark)
                                                        :flag :question-mark
                                                        :question-mark :not-revealed)]
                                 (assoc block :block-state next-block-state))))]
    (reset! board new-board)))

(defmethod update-board-state! :quick-clear [_ board row col]
  (let [quick-clear-coords (game/quick-clear-coords @board row col)
        mine-coords (filter (fn [[row' col']]
                              (get-in @board [row' col' :mine?])) quick-clear-coords)]
    (as-> @board $
          (stepped-on-coords $ mine-coords)
          (reveal-coords $ quick-clear-coords)
          (reset! board $))))

(defmethod update-board-state! :game-lost [_ board row col]
  board)

(defmethod update-board-state! :game-won [_ board row col]
  board)

(defmethod update-board-state! :generate-board [_ board row col]
  (let [rows (count @board)
        cols (count (first @board))
        num-mines (count (game/get-block-coords @board :mine?))]
    (reset! board (game/generate-board rows cols num-mines [row col]))
    (reveal-block board row col)))


(defmethod update-board-state! :new-game [_ board]
  (reset! board (game/generate-board)))