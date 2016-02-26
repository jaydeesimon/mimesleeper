(ns minesweeper.core
  (:gen-class))

(defn board-coords [board]
  (for [r (range (count board))
        c (range (count (first board)))]
    [r c]))

(defn generate-blank-board [rows cols]
  (let [initial-square {:revealed? false
                        :adjacent-mines 0
                        :mine? false}
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
  (let [offsets (for [r (range -1 (inc 1))
                      c (range -1 (inc 1))]
                  [r c])]
    (map (fn [[r c]]
           [(+ r row) (+ c col)])
         offsets)))

(defn mine? [board row col]
  (get-in board [row col :mine?]))

(defn surrounding-cells [board row col]
  (->> (surrounding-coords row col)
       (map (fn [coord]
              {:coord coord :val (get-in board coord)}))
       (filter #(some? (:val %)))))

(defn update-adjacent-mine-vals [board]
  (let [non-mine-coords (filter #(not (:mine? %))
                          (board-coords board))]
    (reduce (fn [board [row col]]
              (let [sc (surrounding-cells board row col)
                    (filter )]
                )))))

(defn touches-mine? [board row col]
  (some #(= (:val %) :*)
        (surrounding-cells board row col)))

