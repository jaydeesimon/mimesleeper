(ns mimesleeper-ui.core
  (:require [reagent.core :as reagent :refer [atom]]
            [mimesleeper-ui.game :as game]
            [mimesleeper-ui.components :as c]))

(def board (atom (game/generate-board 10 8 15)))

(defn mount-root []
  (reagent/render [c/grid board] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
