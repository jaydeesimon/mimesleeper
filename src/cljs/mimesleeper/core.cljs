(ns mimesleeper.core
  (:require [reagent.core :as reagent :refer [atom]]
            [mimesleeper.game :as game]
            [mimesleeper.components :as c]))

(def board (atom (game/generate-board 10 8 15)))

(defn mount-root []
  (reagent/render [c/grid board] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
