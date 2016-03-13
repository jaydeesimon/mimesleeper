(ns mimesleeper.event
  (:require [mimesleeper.game :as game]))

(defmulti process-event (fn [& args]
                          (first args)))

(defmethod process-event :reveal-block [_ board row col]
  (let [new-board (reduce (fn [board' [row' col']]
                            (assoc-in board' [row' col' :revealed?] true))
                          @board
                          (game/coords-to-reveal @board row col))]
    (reset! board new-board)))

(defmethod process-event :toggle-flag [_ board row col]
  (let [new-board (update-in @board
                             [row col]
                             (fn [block]
                               (if (= (:marked? block) :flag)
                                 (assoc block :marked? nil)
                                 (assoc block :marked? :flag))))]
    (reset! board new-board)))