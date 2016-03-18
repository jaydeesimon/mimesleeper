(ns mimesleeper.components
  (:require [mimesleeper.game :as game]
            [mimesleeper.event :as evt]))

(defn on-right-click [event board row col]
  (fn [e]
    (.preventDefault e)
    (evt/process-event event board row col)))

(defn flag [board row col]
  [:svg {:width "22px" :height "22px" :viewBox "0 0 76 76" :enable-background "new 0 0 76 76"
         :on-context-menu (on-right-click :toggle-flag board row col)}
   [:path {:fill "#FDFCFD" :d "M0.009,0h75.982C75.996,0,76,0.004,76,0.009V75.99c0,0.006-0.004,0.01-0.009,0.01H0.009 C0.004,76,0,75.996,0,75.99V0.009C0,0.004,0.004,0,0.009,0z"}]
   [:path {:fill "#757575" :d "M75.99,0C75.995,0,76,0.005,76,0.01v75.981C76,75.996,75.995,76,75.99,76H0.009 C0.004,76,0,75.996,0,75.991L75.99,0z"}]
   [:polygon {:fill "#B9B9B9" :points "8.508,8.5 67.492,8.5 67.5,8.508 67.5,67.491 67.492,67.5 8.508,67.5 8.5,67.491 8.5,8.508"}]
   [:polygon {:points "35.999,55.5 35.999,16.5 40,16.5 40,55.5 35.999,55.5"}]
   [:polygon {:fill "#FF0000" :points "40,13.875 19.375,27 40,40.125"}]
   [:rect {:x "28.571" :y "51.625" :width "18.857" :height "5.5"}]
   [:rect {:x "20.222" :y "56.459" :width "35.555" :height "7.041"}]])

(defn unopened [board row col]
  [:svg {:width "22px" :height "22px" :viewBox "0 0 76 76" :enable-background "new 0 0 76 76"
         :on-click #(evt/process-event :reveal-block board row col)
         :on-context-menu (on-right-click :toggle-flag board row col)}
   [:path {:fill "#FDFCFD" :d "M0.009,0h75.982C75.996,0,76,0.004,76,0.009V75.99c0,0.006-0.004,0.01-0.009,0.01H0.009 C0.004,76,0,75.996,0,75.99V0.009C0,0.004,0.004,0,0.009,0z"}]
   [:path {:fill "#757575" :d "M75.99,0C75.995,0,76,0.005,76,0.01v75.981C76,75.996,75.995,76,75.99,76H0.009 C0.004,76,0,75.996,0,75.991L75.99,0z"}]
   [:polygon {:fill "#B9B9B9" :points "8.508,8.5 67.492,8.5 67.5,8.508 67.5,67.491 67.492,67.5 8.508,67.5 8.5,67.491 8.5,8.508"}]])

(defn question-mark [board row col]
  [:svg {:width "22px" :height "22px" :viewBox "0 0 76 76" :enable-background "new 0 0 76 76"}
   [:path {:fill "#FDFCFD" :d "M0.009,0h75.982C75.996,0,76,0.004,76,0.009V75.99c0,0.006-0.004,0.01-0.009,0.01H0.009 C0.004,76,0,75.996,0,75.99V0.009C0,0.004,0.004,0,0.009,0z"}]
   [:path {:fill "#757575" :d "M75.99,0C75.995,0,76,0.005,76,0.01v75.981C76,75.996,75.995,76,75.99,76H0.009 C0.004,76,0,75.996,0,75.991L75.99,0z"}]
   [:polygon {:fill "#B9B9B9" :points "8.508,8.5 67.492,8.5 67.5,8.508 67.5,67.491 67.492,67.5 8.508,67.5 8.5,67.491 8.5,8.508"}]
   [:path {:d "M24.684,28.694c0-4.57,1.569-8.347,4.716-11.338c2.219-2.109,5.175-3.164,8.877-3.164c4.065,0,7.213,1.055,9.433,3.164 c2.4,2.285,3.606,5.186,3.606,8.701c0,2.991-1.109,5.537-3.329,7.646c-1.295,1.23-3.052,2.9-5.271,5.01 c-1.664,1.582-2.497,3.955-2.497,7.119h-4.993c0-3.864,0.647-6.765,1.942-8.701c0.924-1.406,2.678-3.337,5.271-5.801 c1.479-1.406,2.22-3.073,2.22-5.01c0-2.461-0.65-4.307-1.942-5.537c-1.109-1.055-2.681-1.582-4.716-1.582 c-1.665,0-3.052,0.527-4.161,1.582c-1.85,1.758-2.774,4.395-2.774,7.91H24.684z M34.671,53.479h6.104v6.328h-6.104V53.479z"}]])

; #cc0000
;#B9B9B9

(defn bomb [board row col]
  [:svg {:width "22px" :height "22px" :viewBox "0 0 97.324 97.324" :enable-background "new 0 0 76 76"
         :on-context-menu (fn [e] (.preventDefault e))}
   [:svg {:viewBox "0 0 76 76"}
    [:polygon {:fill (if (get-in @board [row col :active?]) "#CC0000" "#B9B9B9") :points "75.001,75.001 1,75.001 1,1 75.001,1 75.001,75.001"}]
    [:path {:fill "#757575" :d "M75.991,0C75.996,0,76,0.004,76,0.009V75.99c0,0.006-0.004,0.01-0.009,0.01H0.009 C0.004,76,0,75.996,0,75.99V0.009C0,0.004,0.004,0,0.009,0H75.991 M74,2H2v72h72V2L74,2z"}]]
   [:path {:d "M76.17,21.449c-7.803-1.021-7.188,6.1-8.719,11.448c-0.799,2.797-4.264,1.256-6.268,1.079 c-1.664-0.148-3.312-0.009-4.754,0.725c-5.978-5.064-13.704-8.125-22.148-8.125C15.349,26.576,0,41.924,0,60.857 c0,18.934,15.35,34.281,34.281,34.281c18.931,0,34.281-15.349,34.281-34.281c0-8.524-3.119-16.315-8.269-22.312 c0.779-0.081,1.688-0.041,2.783,0.168c2.683,0.511,5.267,0.126,7.142-1.957c1.469-1.632,1.817-3.988,2.088-6.077 c0.375-2.903,0.309-5.336,3.862-4.872C78.951,26.171,78.916,21.808,76.17,21.449z M22.98,47.736 c-4.586,3.271-7.322,8.581-7.322,14.204c0,2.209-1.791,4-4,4s-4-1.791-4-4c0-8.202,3.99-15.947,10.676-20.717 c1.795-1.282,4.295-0.868,5.578,0.933C25.195,43.955,24.779,46.453,22.98,47.736z"}]
   [:path {:d "M61.131,15.369c0.504,1.093,1.801,1.57,2.893,1.066c1.091-0.502,1.568-1.799,1.066-2.892l-4.654-10.09 c-0.504-1.093-1.799-1.57-2.894-1.066c-0.356,0.165-0.649,0.417-0.864,0.719c-0.435,0.615-0.537,1.438-0.199,2.171L61.131,15.369z"}]
   [:path {:d "M75.166,16.482c1.088,0.513,2.385,0.046,2.9-1.042L82.805,5.39c0.514-1.089,0.049-2.388-1.041-2.9 c-0.972-0.458-2.109-0.137-2.709,0.71c-0.07,0.103-0.136,0.213-0.191,0.331l-4.74,10.051 C73.611,14.67,74.076,15.969,75.166,16.482z"}]
   [:path {:d "M97.252,21.11c-0.307-1.165-1.498-1.859-2.66-1.553l-10.748,2.829c-0.516,0.136-0.938,0.446-1.223,0.85 c-0.361,0.507-0.5,1.166-0.33,1.813c0.307,1.164,1.498,1.858,2.664,1.552l10.744-2.831C96.863,23.466,97.559,22.273,97.252,21.11z"}]
   [:path {:d "M92.422,41.906l-9.074-6.413c-0.983-0.694-2.344-0.461-3.037,0.521c-0.694,0.983-0.461,2.343,0.523,3.037l9.074,6.413 c0.982,0.695,2.342,0.461,3.037-0.521C93.639,43.959,93.406,42.6,92.422,41.906z"}]])

(def block-n-svg-elems
  {0 [[:polygon {:fill "#B9B9B9" :points "75.001,75.001 1,75.001 1,1 75.001,1 75.001,75.001"}]
      [:path {:fill "#757575" :d "M75.991,0C75.996,0,76,0.004,76,0.009V75.99c0,0.006-0.004,0.01-0.009,0.01H0.009 C0.004,76,0,75.996,0,75.99V0.009C0,0.004,0.004,0,0.009,0H75.991 M74,2H2v72h72V2L74,2z"}]]
   1 [[:polygon {:fill "#B9B9B9" :points "75.001,75.001 1,75.001 1,1 75.001,1"}]
      [:path {:fill "#757575" :d "M75.991,0C75.996,0,76,0.004,76,0.009V75.99c0,0.006-0.004,0.01-0.009,0.01H0.009 C0.004,76,0,75.996,0,75.99V0.009C0,0.004,0.004,0,0.009,0H75.991 M74,2H2v72h72V2L74,2z"}]
      [:polygon {:fill "#0000FF" :points "40.857,8.928 18,26.071 18,31.785 32.286,31.785 32.286,54.644 18,54.644 18,66.071 58,66.071 58,54.644 46.571,54.644 46.571,8.928"}]]
   2 [[:polygon {:fill "#B9B9B9" :points "75.001,75.001 1,75.001 1,1 75.001,1"}]
      [:path {:fill "#757575" :d "M75.991,0C75.996,0,76,0.004,76,0.009V75.99c0,0.006-0.004,0.01-0.009,0.01H0.009 C0.004,76,0,75.996,0,75.99V0.009C0,0.004,0.004,0,0.009,0H75.991 M74,2H2v72h72V2L74,2z"}]
      [:path {:fill "#008200" :d "M51.256,26.134v-4.545c0-1.545-1.293-2.796-2.892-2.796H27.393c-1.593,0-2.892,1.251-2.892,2.796v4.545 H8.593v-5.942C8.593,14.011,13.778,9,20.163,9h35.433c6.388,0,11.569,5.011,11.569,11.191c0,4.064,1.287,8.448-2.521,11.417 c-2.649,2.062-5.948,3.822-8.757,5.743c-6.993,4.777-14.083,9.431-21.093,14.188c-0.91,0.621-3.707,3.276-4.87,3.276 c0.012,0,37.602,0,37.602,0V66H8.774c0-4.529-1.605-9.863,2.609-13.253c2.949-2.372,6.773-4.315,9.932-6.502 c8-5.557,16.259-10.779,24.287-16.306C46.188,29.535,51.256,26.874,51.256,26.134z"}]]
   3 [[:polygon {:fill "#B9B9B9" :points "75.001,75.001 1,75.001 1,1 75.001,1"}]
      [:path {:fill "#757575" :d "M75.991,0C75.996,0,76,0.004,76,0.009V75.99c0,0.006-0.004,0.01-0.009,0.01H0.009 C0.004,76,0,75.996,0,75.99V0.009C0,0.004,0.004,0,0.009,0H75.991 M74,2H2v72h72V2L74,2z"}]
      [:path {:fill "#FF0000" :d "M66.322,20.4c0-6.295-5.104-11.4-11.4-11.4H9.678v10.688H48.51c1.575,0,2.85,1.275,2.85,2.85v6.413 c0,1.569-1.274,2.85-2.85,2.85H26.778v12.825H48.51c1.575,0,2.85,1.274,2.85,2.85v6.413c0,1.569-1.274,2.85-2.85,2.85H9.678V66 h45.244c6.296,0,11.4-5.11,11.4-11.4V43.912c0-2.376-0.729-4.586-1.977-6.412c1.247-1.832,1.977-4.036,1.977-6.413V20.4z"}]]
   4 [[:polygon {:fill "#B9B9B9" :points "75.001,75.001 1,75.001 1,1 75.001,1"}]
      [:path {:fill "#757575" :d "M75.991,0C75.996,0,76,0.004,76,0.009V75.99c0,0.006-0.004,0.01-0.009,0.01H0.009 C0.004,76,0,75.996,0,75.99V0.009C0,0.004,0.004,0,0.009,0H75.991 M74,2H2v72h72V2L74,2z"}]
      [:polygon {:fill "#000084" :points "25.8,9 6.922,42.765 47.584,42.765 47.584,66 63.559,65.637 63.559,42.765 69.078,42.765 69.078,31.873 63.559,31.873 63.559,9 47.584,9 47.584,32.236 29.192,32.236 41.774,9"}]]
   5 [[:polygon {:fill "#B9B9B9" :points "75.001,75.001 1,75.001 1,1 75.001,1"}]
      [:path {:fill "#757575" :d "M75.991,0C75.996,0,76,0.004,76,0.009V75.99c0,0.006-0.004,0.01-0.009,0.01H0.009 C0.004,76,0,75.996,0,75.99V0.009C0,0.004,0.004,0,0.009,0H75.991 M74,2H2v72h72V2L74,2z"}]
      [:path {:fill "#840000" :d "M60.78,32.065c1.765,0.757,3.006,1.726,3.869,2.922c1.324,1.837,1.736,4.211,1.736,7.139 c0,0.012,0,10.689,0,10.689c0,6.291-2.494,12.48-11.401,13.182H9.497V54.953h39.074c1.576,0,2.851-1.279,2.851-2.85v-6.412 c0-1.57-1.274-2.852-2.851-2.852L9.497,42.852V9.001l57.006,0v11.396H26.031v11.668"}]]
   6 [[:polygon {:fill "#B9B9B9" :points "75.001,75.001 1,75.001 1,1 75.001,1"}]
      [:path {:fill "#757575" :d "M75.991,0C75.996,0,76,0.004,76,0.009V75.99c0,0.006-0.004,0.01-0.009,0.01H0.009 C0.004,76,0,75.996,0,75.99V0.009C0,0.004,0.004,0,0.009,0H75.991 M74,2H2v72h72V2L74,2z"}]
      [:path {:fill "#008284" :d "M66.856,23.967v-3.565c0-6.296-5.109-11.4-11.399-11.4H20.544c-6.29,0-11.4,5.104-11.4,11.4v34.197 c0,6.29,5.11,11.4,11.4,11.4h34.913c6.29,0,11.399-5.11,11.399-11.4V43.911c0-4.984-4.921-12.11-11.405-12.11H27.669 c-1.569,0-2.85-1.28-2.85-2.85v-6.412c0-1.575,1.28-2.851,2.85-2.851h20.663c1.569,0,2.85,1.275,2.85,2.851v1.005L66.856,23.967z M25.531,47.474c0-1.569,1.28-2.85,2.851-2.85h20.662c1.57,0,2.851,1.28,2.851,2.85v6.412c0,1.57-1.28,2.851-2.851,2.851H28.382 c-1.57,0-2.851-1.28-2.851-2.851V47.474z"}]]
   7 [[:polygon {:fill "#B9B9B9" :points "75.001,75.001 1,75.001 1,1 75.001,1"}]
      [:path {:fill "#757575" :d "M75.991,0C75.996,0,76,0.004,76,0.009V75.99c0,0.006-0.004,0.01-0.009,0.01H0.009 C0.004,76,0,75.996,0,75.99V0.009C0,0.004,0.004,0,0.009,0H75.991 M74,2H2v72h72V2L74,2z"}]
      [:polygon {:fill "#840084" :points "10.363,9.001 10.363,21.783 44.834,21.78 10.018,65.999 30.464,65.999 65.291,21.78 65.982,9.001"}]]
   8 [[:polygon {:fill "#B9B9B9" :points "75.001,75.001 1,75.001 1,1 75.001,1"}]
      [:path {:fill "#757575" :d "M75.991,0C75.996,0,76,0.004,76,0.009V75.99c0,0.006-0.004,0.01-0.009,0.01H0.009 C0.004,76,0,75.996,0,75.99V0.009C0,0.004,0.004,0,0.009,0H75.991 M74,2H2v72h72V2L74,2z"}]
      [:path {:fill "#757575" :d "M66.856,31.087c0,2.377-0.729,4.581-1.977,6.413c1.247,1.826,1.977,4.036,1.977,6.412V54.6 c0,6.29-5.104,11.4-11.4,11.4H20.544c-6.29,0-11.4-5.11-11.4-11.4V43.912c0-2.376,0.729-4.586,1.982-6.412 c-1.252-1.832-1.982-4.036-1.982-6.413V20.4c0-6.295,5.11-11.4,11.4-11.4h34.912c6.296,0,11.4,5.104,11.4,11.4V31.087z M51.182,22.538c0-1.575-1.275-2.85-2.851-2.85H27.669c-1.57,0-2.851,1.275-2.851,2.85v6.413c0,1.569,1.28,2.85,2.851,2.85h20.662 c1.575,0,2.851-1.28,2.851-2.85V22.538z M51.182,46.05c0-1.575-1.275-2.85-2.851-2.85H27.669c-1.57,0-2.851,1.274-2.851,2.85v6.413 c0,1.569,1.28,2.85,2.851,2.85h20.662c1.575,0,2.851-1.28,2.851-2.85V46.05z"}]]})

(defn block-n [board row col n]
  (let [parent-svg-elem [:svg {:width "22px" :height "22px" :viewBox "0 0 76 76" :enable-background "new 0 0 76 76"
                               :on-context-menu (fn [e] (.preventDefault e))}]]
    (apply conj parent-svg-elem (get block-n-svg-elems n))))

(defn smiley-face []
  [:svg {:width "88px" :height "88px" :viewBox "0 0 511.9 511.9"}
   [:path {:fill "#FFD469" :d "M256,35.3C134.1,35.3,35.3,134.1,35.3,256c0,3.3,0.1,6.6,0.2,9.9c-12.8,1.7-22.8,12.6-22.8,25.9c0,14.5,11.7,26.2,26.2,26.2h5.2C71,409.6,155.6,476.6,256,476.6c121.8,0,220.6-98.8,220.6-220.6C476.6,134.1,377.8,35.3,256,35.3z"}]
   [:path {:fill "#2B3B47" :d "M365.8,179.5c13.4,0,24.2,10.8,24.2,24.2V258c0,13.4-10.8,3-24.2,3l0,0c-13.4,0-24.2,10.3-24.2-3v-54.3C341.6,190.3,352.5,179.5,365.8,179.5L365.8,179.5z"}]
   [:path {:fill "#F2A74E" :d "M343.8,271.4c-2.8,0-5.5-1.5-6.8-4.2c-1.9-3.8-0.3-8.3,3.4-10.2c16.2-8,34.7-8.3,50.7-0.7c3.8,1.8,5.4,6.3,3.6,10.1c-1.8,3.8-6.3,5.4-10.1,3.6c-11.8-5.6-25.4-5.4-37.4,0.5C346.1,271.1,345,271.4,343.8,271.4z"}]
   [:path {:fill "#2B3B47" :d "M146.2,179.5c13.4,0,24.2,10.8,24.2,24.2V258c0,13.4-10.8,3-24.2,3l0,0c-13.4,0-24.2,10.3-24.2-3v-54.3C122,190.3,132.9,179.5,146.2,179.5L146.2,179.5z"}]
   [:path {:fill "#F2A74E" :d "M124.2,271.4c-2.8,0-5.5-1.5-6.8-4.2c-1.9-3.8-0.3-8.3,3.4-10.2c16.2-8,34.7-8.3,50.7-0.7c3.8,1.8,5.4,6.3,3.6,10.1c-1.8,3.8-6.3,5.4-10.1,3.6c-11.8-5.6-25.4-5.4-37.4,0.5C126.5,271.1,125.3,271.4,124.2,271.4z"}]
   [:path {:fill "#2B3B47" :d "M405.4,319.4c-5.1-0.5-10.2-0.8-15.2-1.1c-1.4,0.4-5.2,1.3-11,2.4c-7.4,1-17.9,3.6-30.5,4.8c-12.6,1.1-27.4,3.4-43.1,4.1c-15.8,1.1-32.6,1.3-49.5,1.5c-16.8-0.3-33.7-0.5-49.5-1.6c-15.8-0.8-30.5-3-43.1-4.1c-12.6-1.2-23.2-3.7-30.5-4.8c-5.8-1.1-9.7-2-11-2.4c-5.2,0.3-10.3,0.7-15.5,1.1c-0.6,0-1.2,0.3-1.8,0.7c-1.7,1.2-2.1,3.7-0.9,5.5l2.7,4.1c6.2,9.4,12.4,17.5,18.7,24.3c6.2,7,12.4,13,18.7,18.6c12.4,11,24.9,19.1,37.4,25.9c3.4,1.8,6.8,3.1,10.2,4.6c19-13.3,41,4.7,64.5,4.7c23.6,0,45.6-17.9,64.6-4.5c3.4-1.5,6.7-2.8,10.1-4.5c12.4-6.9,24.9-14.9,37.4-26c6.2-5.6,12.4-11.6,18.7-18.7c6.2-6.8,12.5-15,18.7-24.4l2.7-4.2c0.3-0.6,0.6-1.2,0.6-1.9C409,321.5,407.4,319.6,405.4,319.4z"}]
   [:path {:fill "#FFFFFF" :d "M163.5,325.4c12.6,1.1,27.4,3.3,43.1,4.1c15.8,1.1,32.6,1.3,49.5,1.6c16.8-0.3,33.7-0.4,49.5-1.5c15.8-0.8,30.5-3,43.1-4.1c12.6-1.2,23.2-3.7,30.5-4.8c5.8-1.1,9.6-2,11-2.4c-7.4-0.5-14.8-0.9-22.1-1.4l-37.4-1.4c-24.9-0.8-49.8-0.8-74.7-1.2c-24.9,0.4-49.8,0.4-74.7,1.2l-37.4,1.4c-7.3,0.5-14.6,0.9-21.9,1.4c1.3,0.3,5.2,1.3,11,2.4C140.3,321.7,150.9,324.2,163.5,325.4z"}]
   [:path {:fill "#FF473E" :d "M320.4,403.3c0.1,0,0.1,0,0.2-0.1c-19-13.3-41.1-9-64.5-6.1l-0.1,0l-0.1,0c-23.4-2.9-45.6-7.2-64.5,6.1c0.1,0,0.1,0.1,0.2,0.1c-0.1,0-0.1,0.1-0.2,0.1c21.2,9.4,42.3,13.9,63.5,14c0.3,0,0.7,0.1,1,0.1l0.1,0l0.1,0c0.3,0,0.7-0.1,1-0.1c21.2-0.1,42.3-4.6,63.5-14C320.5,403.3,320.5,403.3,320.4,403.3z"}]
   [:path {:fill "#FFB636" :d "M476.3,265.8c0.1-3.3,0.2-6.6,0.2-9.9c0-58.9-23.1-112.5-60.8-152c21.3,34.5,33.6,75.2,33.6,118.8c0,125.2-101.5,226.7-226.7,226.7c-43.6,0-84.2-12.3-118.8-33.6c39.6,37.7,93.1,60.8,152,60.8c100.3,0,185-67,211.7-158.7h5.2c14.5,0,26.2-11.7,26.2-26.2C499.1,278.4,489.2,267.5,476.3,265.8z"}]])

(defn sad-face [board]
  [:svg {:width "88px" :height "88px" :viewBox "0 0 511.9 511.9"
         :on-click #(evt/process-event :new-game board)}
   [:path {:fill "#00B89C" :d "M255.9,35C134,35,35.2,133.8,35.2,255.6c0,2.8,0.1,5.6,0.2,8.5c-12.8,1.7-22.8,12.6-22.8,25.9c0,14.5,11.7,26.2,26.2,26.2h4.8C70,408.6,155,476.3,255.9,476.3c121.8,0,220.6-98.8,220.6-220.6C476.5,133.8,377.7,35,255.9,35z"}]
   [:path {:fill "#009E83" :d "M476.3,264.1c0.1-2.8,0.2-5.6,0.2-8.5c0-58.9-23.1-112.4-60.8-152c21.3,34.5,33.6,75.2,33.6,118.8c0,125.2-101.5,226.6-226.7,226.6c-43.6,0-84.2-12.3-118.8-33.6c39.6,37.7,93.1,60.9,152,60.9c100.8,0,185.8-67.7,212.1-160.1h4.8c14.5,0,26.2-11.7,26.2-26.2C499,276.6,489.1,265.8,476.3,264.1z"}]
   [:path {:fill "#009E83" :d "M408.9,157.4c-2.1,0-4.2-0.9-5.8-2.6c-13.2-15-31.6-19.2-48-11c-3.8,1.9-8.4,0.3-10.3-3.5c-1.9-3.8-0.3-8.4,3.5-10.3c22.9-11.4,48.3-5.8,66.4,14.6c2.8,3.2,2.5,8-0.7,10.9C412.6,156.7,410.7,157.4,408.9,157.4z"}]
   [:path {:fill "#009E83" :d "M108.3,157.4c-1.8,0-3.6-0.6-5.1-1.9c-3.2-2.8-3.5-7.7-0.7-10.9c18.1-20.4,43.5-26,66.4-14.6c3.8,1.9,5.4,6.5,3.5,10.3c-1.9,3.8-6.5,5.4-10.3,3.5c-16.4-8.1-34.8-3.9-48,11C112.5,156.5,110.4,157.4,108.3,157.4z"}]
   [:path {:fill "#2B3B47" :d "M180.2,192.2c-5.1-5.1-13.3-5.1-18.4,0l-18.1,18.1l-18.1-18.1c-5.1-5.1-13.3-5.1-18.4,0c-5.1,5.1-5.1,13.3,0,18.4l18.1,18.1l-18.1,18.1c-5.1,5.1-5.1,13.3,0,18.4c2.5,2.5,5.9,3.8,9.2,3.8s6.7-1.3,9.2-3.8l18.1-18.1l18.1,18.1c2.5,2.5,5.9,3.8,9.2,3.8s6.7-1.3,9.2-3.8c5.1-5.1,5.1-13.3,0-18.4l-18.1-18.1l18.1-18.1C185.2,205.6,185.2,197.3,180.2,192.2z"}]
   [:path {:fill "#2B3B47" :d "M392,228.8l18.1-18.1c5.1-5.1,5.1-13.3,0-18.4c-5.1-5.1-13.3-5.1-18.4,0l-18.1,18.1l-18.1-18.1c-5.1-5.1-13.3-5.1-18.4,0c-5.1,5.1-5.1,13.3,0,18.4l18.1,18.1L337,246.9c-5.1,5.1-5.1,13.3,0,18.4c2.5,2.5,5.9,3.8,9.2,3.8s6.7-1.3,9.2-3.8l18.1-18.1l18.1,18.1c2.5,2.5,5.9,3.8,9.2,3.8c3.3,0,6.7-1.3,9.2-3.8c5.1-5.1,5.1-13.3,0-18.4L392,228.8z"}]
   [:path {:fill "#FFB636" :d "M350.2,73.7c14.2,3.6,25.3,14.7,28.9,28.9c0.3,1.4,2.1,1.4,2.4,0c3.6-14.2,14.7-25.3,28.9-28.9c1.4-0.3,1.4-2.1,0-2.4c-14.2-3.6-25.3-14.7-28.9-28.9c-0.3-1.4-2.1-1.4-2.4,0c-3.6,14.2-14.7,25.3-28.9,28.9C348.8,71.6,348.8,73.4,350.2,73.7z"}]
   [:path {:fill "#FFB636" :d "M73.4,48.6c11.3,2.9,20.3,11.8,23.1,23.1c0.3,1.1,1.7,1.1,2,0c2.9-11.3,11.8-20.3,23.1-23.1c1.1-0.3,1.1-1.7,0-2c-11.3-2.9-20.3-11.8-23.1-23.1c-0.3-1.1-1.7-1.1-2,0c-2.9,11.3-11.8,20.3-23.1,23.1C72.3,46.9,72.3,48.3,73.4,48.6z"}]
   [:path {:fill "#FFB636" :d "M184.8,79.9c14.2,3.6,25.3,14.7,28.9,28.9c0.3,1.4,2.1,1.4,2.4,0c3.6-14.2,14.7-25.3,28.9-28.9c1.4-0.3,1.4-2.1,0-2.4c-14.2-3.6-25.3-14.7-28.9-28.9c-0.3-1.4-2.1-1.4-2.4,0c-3.6,14.2-14.7,25.3-28.9,28.9C183.4,77.8,183.4,79.6,184.8,79.9z"}]
   [:path {:fill "#2B3B47" :d "M303,348.3c0,0-1.1,1.3-3,3.5c-0.5,0.5-1,1.1-1.7,1.8c-1,0.7-2.1,1.5-3.3,2.3c-1.3,0.8-2.6,1.6-4,2.5c-1.4,0.8-2.8,1.2-4.4,1.8c-1.5,0.5-3.1,1.3-4.9,1.7c-1.7,0.4-3.6,0.6-5.4,0.9c-3.8,0.7-7.8,0.4-12,0.1c-4.1-0.8-8.5-1.4-12.6-3.5c-2.1-0.9-4.2-1.7-6-3.1l-2.9-1.9l-1.4-1l-0.7-0.5l-0.4-0.2l-0.2-0.1l-0.1-0.1l0,0c0.5,0.4-1.1-0.8,1,0.8c-2.4-1.7-6-4.5-6.6-4.2c-0.8-0.6-2.9-1.3-4.7-2c-0.9-0.3-1.9-0.3-2.8-0.6c-0.9-0.3-1.9-0.5-2.8-0.5c-1.8-0.1-3.6-0.4-5.3-0.2c-1.7,0-3.3,0.2-4.7,0.5c-1.4,0.2-2.8,0.4-3.7,0.8c-2.1,0.6-3.5,0.9-3.5,0.9l-2.9,0.6c-4.9,1-9.7-2.2-10.7-7.1c-0.6-2.8,0.3-5.6,1.9-7.6c0,0,1.3-1.6,4-4.2c1.3-1.4,3.1-2.8,5.3-4.1c1.1-0.7,2.3-1.4,3.6-2.2c1.3-0.7,2.8-1.2,4.3-1.8c1.5-0.5,3.1-1.3,4.9-1.7c1.7-0.4,3.6-0.6,5.4-0.9c3.8-0.7,7.8-0.4,12-0.1c4.1,0.9,8.3,1.3,13.2,3.8c5.6,2.4,7,4,10,5.8l3.5,2.5c0.7,0.7,1.7,1,2.5,1.4c1.7,1,3.5,1.6,5.3,2.3c0.9,0.3,1.9,0.3,2.8,0.6c0.9,0.3,1.9,0.5,2.8,0.5c1.8,0.1,3.6,0.4,5.3,0.2c0.8,0,1.6,0,2.4-0.1c0.6-0.1,1.2-0.1,1.8-0.1c0.6,0,1.2-0.1,1.7,0c0.7-0.2,1.5-0.6,2.2-0.8c2.7-1,4.4-1.6,4.4-1.6c5.1-1.6,10.4,1.3,12,6.3C305.6,342.6,304.9,345.9,303,348.3z"}]
   [:path {:fill "#009E83" :d "M254.9,403.6c-8.5,0-16.9-1.9-24.8-5.6c-4.1-1.9-5.8-6.8-3.9-10.9c1.9-4.1,6.8-5.8,10.9-3.9c11.7,5.5,25.1,5.3,36.9-0.5c4-2,8.9-0.4,11,3.7c2,4,0.4,8.9-3.7,11C272.9,401.5,263.9,403.6,254.9,403.6z"}]])

(defn flags [num]
  [:h1 num])

(defn grid [board]
  (let [cols (count (first @board))]
    [:div
     [:div (if (game/mine-revealed? @board) [sad-face board] [smiley-face])]
     [:div {:style {:width        (str (* 22 cols) "px")
                    :border-style "ridge"
                    :border-width "12px"
                    :border-color "#B9B9B9"
                    :line-height  "0em"}}
      (doall (map (fn [[row col]]
                    (let [block (cond
                                  (= (get-in @board [row col :marked?]) :flag) [flag board row col]
                                  (= (get-in @board [row col :marked?]) :question-mark) [question-mark board row col]
                                  (not (get-in @board [row col :revealed?])) [unopened board row col]
                                  (get-in @board [row col :mine?]) [bomb board row col]
                                  :else [block-n board row col (get-in @board [row col :adjacent-mine-cnt])])]
                      (with-meta block {:key [row col]})))
                  (game/board-coords @board)))]]))