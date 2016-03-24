(ns mimesleeper.core
  (:require [reagent.core :as reagent :refer [atom]]
            [mimesleeper.game :as game]
            [mimesleeper.components :as c]))

#_(def board (atom (game/generate-board 8 8 10)))

(def board (atom (game/generate-board)))

#_(def board (atom (game/generate-board 16 16 40)))

(defn mount-root []
  (reagent/render [c/grid board] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
