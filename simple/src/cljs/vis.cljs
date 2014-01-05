(ns simple.vis
  (:require [simple.utils :refer [jsObj]]
            [strokes :refer [d3]]
            [clojure.browser.dom :as dom]           
            [goog.debug.DivConsole :as goog-dc]
            [goog.events :as goog-events]
            [goog.ui.ComboBox :as combo-box]))

;;;;;;;;;;;;;;;;;;;;;;

(def L js/L)
(def metadata (.-metadata js/data))
(def records (.-records js/data))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(def info (-> d3
              (.select "#info")))


(def canvas (-> info
                (.append "svg")
                (.style "position" "absolute")
                (.attr (clj->js {:width  900
                                 :height 900
                                 :x 10
                                 :y 40}))))

(def tooltip (-> canvas
                 (.append "text")
                 (.attr "width" 100)
                 (.attr "x" 10)
                 (.attr "y" 30)))

(def title (-> canvas
               (.append "text")
               (.attr "width" 100)
               (.attr "x" 200)
               (.attr "y" 40)))

(def plot (-> canvas
              (.append "svg")
              (.style "position" "absolute")
              (.attr "height" 400)
              (.attr "width" 500)
              (.attr "x" 30)
              (.attr "y" 100)))

(def plot-y-axes-gs (vec
                     (for [i [0 1]]
                       (.append plot "g"))))

(defn update-info [d]
  (let [p (.-plotting d)
        xscale (-> (.-scale d3)
                   (.linear)
                   (.domain (clj->js [0 1]))
                   (.range (clj->js [50
                                     (- (.attr plot "width") 300)])))
        yscales (vec
                 (for [i [0 1]]
                   (-> (.-scale d3)
                       (.linear)
                       (.domain (aget (.-domains p) i))
                       (.range (clj->js [(- (.attr plot "height") 20)
                                         20])))))
        update-line (fn [l]
                      (this-as this
                               (-> d3
                                   (.select this)
                                   (.attr "x1" (xscale 0))
                                   (.attr "y1" ((yscales 0) (aget (.-ys l) 0)))
                                   (.attr "x2" (xscale 1))
                                   (.attr "y2" ((yscales 1) (aget (.-ys l) 1)))
                                   (.attr "stroke-width" 3)
                                   (.attr "stroke" (.-color l)))))

        update-text (fn [l]
                      (this-as this
                               (-> d3
                                   (.select this)
                                   (.attr "x" (+ (xscale 1) 60))
                                   (.attr "y" ((yscales 1) (aget (.-ys l) 1)))
                                   (.attr "fill" (.-color l))
                                   (.text (fn [l] (str (.-name l)
                                                      ":\t"
                                                      (aget (.-ys l) 0)
                                                      " ->\t"
                                                      (aget (.-ys l) 1)))))))
        
        ;; y-axes (vec
        ;;         (for [i [0 1]]
        ;;           (-> (.-svg d3)
        ;;               (.axis)
        ;;               (.scale (yscales i))
        ;;               (.orient "right")
        ;;               (.ticks 5))))
        ]
    
    (-> plot
        (.selectAll "line")
        (.data (.-lines p))
        (.each update-line)
        (.enter)
        (.append "line")
        (.each update-line))

    (-> plot
        (.selectAll "text")
        (.data (.-lines p))
        (.each update-text)
        (.enter)
        (.append "text")
        (.each update-text))

    ;; (doseq [i [0]]
    ;;   (-> (plot-y-axes-gs i)
    ;;       ;;(.attr "transform" "translate(100,0)")
    ;;       (.call (y-axes i))))
    ))

;;  update-info (aget records


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def tile-url
  "http://{s}.tile.cloudmade.com/BC9A493B41014CAABB98F0471D759707/22677/256/{z}/{x}/{y}.png"
  ;;"http://{s}.tile.cloudmade.com/BC9A493B41014CAABB98F0471D759707/997/256/{z}/{x}/{y}.png"
  )


;;(js/alert (.stringify js/JSON (.-colorkeys metadata)))

(def show-map
  (let [mappy (-> L (.map "map") 
                  (.setView (clj->js [32 34.7]) 12))]
    (fn []
      (let [;; geojsonMarkerOptions {:radius 5,
            ;;                       :color "#000",
            ;;                       :fillColor "#965",
            ;;                       :weight 1,
            ;;                       :opacity 1,
            ;;                       :fillOpacity 0.8}

            gjPolygons (clj->js
                        (for [d (filter #(aget % "polygon")
                                        records)]
                          {:type "Feature",
                           :properties {:d d}
                           :geometry {:type "Polygon",
                                      :coordinates [(aget d "polygon")]}}))

            ;;_ (js/alert (.stringify js/JSON gjPolygons))
            ;;_ (js/alert (.stringify js/JSON (.-colorkeys metadata)))
            
            layer-by-colors-key
            (clj->js (into (sorted-map)
                           (for [color-key (.-colorkeys metadata)]
                             {color-key
                              (-> L
                                  (.geoJson gjPolygons
                                            (clj->js
                                             {:style (fn [feature]
                                                       (clj->js {:weight (if (> 6 (.-chisqval (.-d (.-properties feature))))
                                                                           1
                                                                           6)
                                                                 :color (aget (.-color
                                                                               (.-plotting
                                                                                (.-d
                                                                                 (.-properties feature))))
                                                                              color-key)
                                        ;:weight 2
                                                                 :fillOpacity 0.6}))
                                              :onEachFeature
                                              (fn [feature layer]
                                                (let [d (.-d (.-properties feature))
                                                      p (.-plotting d)
                                                      desc (str (.-desc d)
                                                                " "
                                                                (.-lr13str p))
                                                      color (.-color p)]
                                                  (.on layer
                                                       (clj->js
                                                        {:mouseover (fn []
                                                                      (do (-> title
                                                                              (.text desc)
                                                                              (.attr "fill" color))
                                                                          (update-info d)))
                                                         :mouseout (fn []
                                                                     (-> title
                                                                         (.text "")))
                                                         :click (fn []
                                                                  (do
                                                                    (-> title
                                                                        (.text desc)
                                                                        (.attr "fill" color))
                                                                    (update-info d)))}))))
                                              }))
                                  ;;(.addTo mappy)
                                  )})))
            ]
        
        (-> L (.tileLayer tile-url {
                                    :maxZoom 18
                                    :attribution "Map data &copy ; <a href=\"http://openstreetmap.org\">OpenStreetMap</a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA</a>, Imagery © <a href=\"http://cloudmade.com\">CloudMade</a>"})
            (.addTo mappy))

        ;; var cmAttr = 'Map data &copy; 2011 OpenStreetMap contributors, Imagery &copy; 2011 CloudMade',
        ;; cmUrl = 'http://{s}.tile.cloudmade.com/BC9A493B41014CAABB98F0471D759707/{styleId}/256/{z}/{x}/{y}.png';

        ;; var minimal   = L.tileLayer(cmUrl, {styleId: 22677, attribution: cmAttr}),
        ;; midnight  = L.tileLayer(cmUrl, {styleId: 999,   attribution: cmAttr}),
        ;; motorways = L.tileLayer(cmUrl, {styleId: 46561, attribution: cmAttr});

        ;; var map = L.map('map', {
        ;;                         center: [39.73, -104.99],
        ;;                         zoom: 10,
        ;;                         layers: [minimal, motorways, cities]
        ;;                         });

        ;; var baseLayers = {
        ;;                   "Minimal": minimal,
        ;;                   "Night View": midnight
        ;;                   };

        ;; var overlays = {
        ;;                 "Motorways": motorways,
        ;;                 "Cities": cities
        ;;                 };
        
        ;;(js/alert (.stringify js/JSON layer-by-colors-key))
        
        (-> (.-control L)
            (.layers layer-by-colors-key)
            (.addTo mappy))
        
        ;;L.control.layers(layer-by-colors-key).addTo(mappy);
        ))))

(show-map)

;; (let [el (dom/get-element "colorkeycombo")
;;       cb (goog.ui.ComboBox.)]
;;   (doto cb
;;     (.setUseDropdownArrow true)
;;     (.setDefaultText "Select colorkey...")
;;     (.addItem (goog.ui.ComboBoxItem. "label"))
;;     (.addItem (goog.ui.ComboBoxItem. "smooth-all"))
;;     (.addItem (goog.ui.ComboBoxItem. "smooth-stayed"))
;;     (.addItem (goog.ui.ComboBoxItem. "smooth-moved"))
;;     (.render el))
;;   (.listen goog.events
;;            cb "change"
;;            (fn [e]
;;              (show-map
;;               (.getValue (.-target e)))
;;              ;; (. goog.dom (setTextContent (dom/get-element "v") (.. e target (getValue))))
;;              )
;;            ;; (fn [e] (do (js/alert (.. e target (getValue)))
;;            ;;            ;; (show-map
;;            ;;            ;;  (.. e target (getValue)))
;;            ;;            ))
;;            ))

                                        ;(show-map "label")
