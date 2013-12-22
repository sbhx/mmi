(ns simple.vis
  (:require [simple.utils :refer [jsObj]]
            [strokes :refer [d3]]))

;;;;;;;;;;;;;;;;;;;;;;

(def L js/L)
(def data js/data)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(def info (-> d3
              (.select "#info")))


(def canvas (-> info
                (.append "svg")
                (.style "position" "absolute")
                (.attr (clj->js {:width  600
                                 :height 600
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
                (.attr "x" 30)
                (.attr "y" 60)))


(defn show-info [d]
  (let [p (.-plotting d)
        xscale (-> (.-scale d3)
                   (.linear)
                   (.domain (clj->js [0 1]))
                   (.range (clj->js [150
                                     (- (.attr plot "width") 150)])))
        yscale0 (-> (.-scale d3)
                   (.linear)
                   (.domain (aget (.-domains p) 0))
                   (.range (clj->js [(- (.attr plot "width") 20)
                                     20])))
        yscale1 (-> (.-scale d3)
                    (.linear)
                    (.domain (aget (.-domains p) 1))
                    (.range (clj->js [(- (.attr plot "width") 20)
                                      20])))
        update-line (fn [l]
                      (this-as this
                               (-> d3
                                   (.select this)
                                   (.attr "x1" (xscale 0))
                                   (.attr "y1" (yscale0 (aget (.-ys l) 0)))
                                   (.attr "x2" (xscale 1))
                                   (.attr "y2" (yscale1 (aget (.-ys l) 1)))
                                   (.attr "stroke-width" 3)
                                   (.attr "stroke" (.-color l)))))]
    (-> title
        (.text "title"))
    (-> tooltip
        (.text "tooltip"))
    (-> plot
        (.selectAll "line")
        (.data (.-lines p))
        (.each update-line)
        (.enter)
        (.append "line")
        (.each update-line))))

(show-info (aget data 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def tile-url
  "http://{s}.tile.cloudmade.com/BC9A493B41014CAABB98F0471D759707/997/256/{z}/{x}/{y}.png")

(let [mappy (-> L (.map "map") 
                (.setView (clj->js [32 34.8]) 9))
      geojsonMarkerOptions {:radius 4,
                            :color "#000",
                            :fillColor "#965",
                            :weight 1,
                            :opacity 1,
                            :fillOpacity 0.8}
      ;; _ (js/alert (js/JSON.stringify
      ;;              (clj->js
      ;;               (for [d (take 10 data)]
      ;;                 [(.-plotting d)]))))
      gjPoints (clj->js
                (for [d data]
                  {:type "Feature"
                   :properties {:name (.-desc d)
                                :color (.-color (.-plotting d))}
                   :geometry {:type "Point"
                              :coordinates [(aget d "mean-x")
                                            (aget d "mean-y")]}}))]
 
 (-> L
     (.geoJson gjPoints
               (clj->js
                {:pointToLayer
                 (fn [feature latlng]
                   (-> L
                       (.circleMarker latlng
                                      (clj->js
                                       (assoc geojsonMarkerOptions
                                         :fillColor 
                                         (.-color
                                          (.-properties feature)))
                                         ))))}))
     (.addTo mappy))

 (-> L (.tileLayer tile-url {
                             :maxZoom 18
                             :attribution "Map data &copy; <a href=\"http://openstreetmap.org\">OpenStreetMap</a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA</a>, Imagery © <a href=\"http://cloudmade.com\">CloudMade</a>"})
     (.addTo mappy))

  
  (let [popup (-> L .popup)]
    (.on mappy "click" (fn [{:keys [latlng]} e]
    (-> popup (.setLatLng latlng)
              (.setContent (str "You clicked the map at " latlng))
              (.openOn mappy))))))
