(ns mimesleeper-ui.prod
  (:require [mimesleeper-ui.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
