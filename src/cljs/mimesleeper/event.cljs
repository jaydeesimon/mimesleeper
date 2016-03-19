(ns mimesleeper.event
  (:require [mimesleeper.game :as game]))

(defmulti process-event (fn [& args]
                          (first args)))

(defn reveal-coords [board coords]
  (reduce (fn [board' [row col]]
            (assoc-in board' [row col :block-state] :revealed))
          board
          coords))

(defmethod process-event :reveal-block [_ board row col]
  (let [new-board (if (get-in @board [row col :mine?])
                    (-> (assoc-in @board [row col :stepped-on-mine?] true)
                        (reveal-coords (game/get-block-coords @board :mine?)))
                    (reveal-coords @board
                                   (filter (fn [[row col]]
                                             (= :not-revealed (get-in @board [row col :block-state])))
                                           (game/traverse-coords @board row col))))]
    (reset! board new-board)))

(defmethod process-event :toggle-block-state [_ board row col]
  (let [new-board (update-in @board
                             [row col]
                             (fn [block]
                               (let [next-block-state (condp = (:block-state block)
                                                        :not-revealed :flag
                                                        :flag :question-mark
                                                        :question-mark :not-revealed)]
                                 (assoc block :block-state next-block-state))))]
    (reset! board new-board)))

(defmethod process-event :new-game [_ board]
  (reset! board (game/generate-board)))