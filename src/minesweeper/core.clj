(ns minesweeper.core
  (:gen-class))

(defn board-coords [board]
  (for [row (range (count board))
        col (range (count (first board)))]
    [row col]))

(defn init-board [rows cols]
  (let [initial-square {:revealed?      false
                        :adjacent-mines 0
                        :mine?          false}
        initial-row (vec (take cols (cycle [initial-square])))]
    (vec (for [_ (range rows)]
           initial-row))))

(defn add-mines [board num-mines]
  (let [rand-coords (take num-mines (shuffle (board-coords board)))]
    (reduce (fn [board [row col]]
              (update-in board [row col :mine?] (constantly true)))
            board
            rand-coords)))

(defn surrounding-coords [row col]
  (let [offsets (for [row (range -1 (inc 1))
                      col (range -1 (inc 1))]
                  [row col])]
    (->> offsets
         (map (fn [[row' col']]
                [(+ row' row) (+ col' col)]))
         (remove (fn [[row' col']]
                   (and (= row' row) (= col' col)))))))

(defn surrounding-cells [board row col]
  (->> (surrounding-coords row col)
       (map (fn [coord]
              {:coord coord :val (get-in board coord)}))
       (filter #(some? (:val %)))))

(defn update-adjacent-mine-count [board]
  (let [non-mine-coords (filter #(not (:mine? %))
                                (board-coords board))]
    (reduce (fn [board [row col]]
              (let [surrounding-cells (surrounding-cells board row col)
                    surrounding-mines (filter #(get-in % [:val :mine?]) surrounding-cells)]
                (assoc-in board [row col :adjacent-mines] (count surrounding-mines))))
            board
            non-mine-coords)))

(defn generate-board [rows cols num-mines]
  (-> (init-board rows cols)
      (add-mines num-mines)
      (update-adjacent-mine-count)))
