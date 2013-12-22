(ns simple.vis
  (:require [simple.data]
            [simple.utils :refer [jsObj]]
            [strokes :refer [d3]]
            ;; [blade ;refer [L]]
            ;; [goog.events :as events]
            ;; [goog.dom]
            ))

;; ; rect: data ↦ width, index ↦ y
;; ; adapted from mike bostocks slide presentation

;; ;;;; shared data across the 3 examples

;; (def m [50 40 50 40])
;; (def w (- 960 (m 1) (m 3)))
;; (def h (- 500  (m 0) (m 2)))

;; (def data simple.data/data)

;; ;;;;;; a world with strokes!
;; (strokes/bootstrap)

;; ; x is a fn: data ↦ width
;; (def x (-> d3 .-scale (.linear)
;;   (.domain [0 (apply max data)])
;;   (.range [0 w])))

;; ; y is a fn: index ↦ y
;; (def y (-> d3 .-scale (.ordinal)
;;   (.domain (vec (range (count data))))
;;   (.rangeRoundBands [0 h] 0.2)))

;; (def svg2 (-> d3 (.select "#static") (.append "svg")
;;     (.attr {:width  (+ w (m 1) (m 3))
;;             :height (+ h (m 0) (m 2))})
;;   (.append "g")
;;     (.attr {:transform (str "translate(" (m 3) "," (m 0) ")")})))

;; ; Data ↦ Element
;; (def bar2 (-> svg2 (.selectAll "g.bar")
;;     (.data data)
;;   (.enter) (.append "g")
;;     (.attr {:class "bar"
;;             :transform #(str "translate(0," (y %2) ")")})))

;; ; Data Attributes ↦ Element Attributes
;; (-> bar2 (.append "rect")
;;     (.attr {:width   #(x %)
;;             :height  (.rangeBand y)})
;;     (.style {:fill   "indianred"
;;              :stroke-opacity 0}))

;; ; Data Attributes ↦ Element Attributes
;; (-> bar2 (.append "text")
;;     (.attr {:x  x
;;             :y  (/ (.rangeBand y) 2)
;;             :dx -6
;;             :dy ".35em"
;;             :text-anchor "end"})
;;     (.style "fill" "white")
;;     (.text identity))

;;;;;;;;;;;;;;;;;;;;

;; (def gmap (google.maps.Map. (-> d3
;;                                (.select "#gmap")
;;                                (.node))
;;                            {:zoom 11
;;                             :center (google.maps.LatLng. 32 35)
;;                             :mapTypeId google.maps.MapTypeId/ROADMAP}))
;; (def marker-data
;;   [{:x 35 :y 32}
;;    {:x 35.1 :y 32}])
;; (def overlay (google.maps.OverlayView.))
;; ;; (aset overlay
;; ;;       "onAdd"
;; ;;       (fn []
;; ;;         nil
;; ;;         ;; (let [layer (-> d3
;; ;;         ;;                 (.select (-> overlay
;; ;;         ;;                              (.getPanes)
;; ;;         ;;                              (,overlayMouseTarget))))
;; ;;         ;;       update-marker (fn [d]
;; ;;         ;;                       (this-as this
;; ;;         ;;                                (let [proj (.fromLatLngToDivPixel
;; ;;         ;;                                            projection
;; ;;         ;;                                            (google.mals.LatLng. (:y d)
;; ;;         ;;                                                                 (:x d)))]
;; ;;         ;;                                  (-> d3
;; ;;         ;;                                      (.select this)
;; ;;         ;;                                      (.attr "fill" "red")
;; ;;         ;;                                      (.style "left" (:x proj))
;; ;;         ;;                                      (.style "top" (:y proj))))))]
          
;; ;;         ;;   (aset overlay
;; ;;         ;;         "draw"
;; ;;         ;;         (fn []
;; ;;         ;;           (let [projection (.getProjection overlay)
;; ;;         ;;                 marker (-> layer
;; ;;         ;;                            (.selectAll "svg")
;; ;;         ;;                            (.data marker-data)
;; ;;         ;;                            (.each update-marker)
;; ;;         ;;                            (.enter)
;; ;;         ;;                            (.append "svg")
;; ;;         ;;                            (.each update-marker))]
;; ;;         ;;             (-> marker
;; ;;         ;;                 (.append "circle")
;; ;;         ;;                 (.attr "r" 8))))))
;; ;;         ))
;; (.setMap overlay gmap)


;;;;;;;;;;;;;;;;;;;;;;

;; See https://github.com/sritchie/contour.git .

;; (def overlay-defaults
;;   "default overlay options; we're assuming that the tilesize is
;; standard, etc."
;;   {:minZ 3
;;    :maxZ 10
;;    :tileSize (google.maps.Size. 256 256)})

;; (defn mk-overlay
;;   "Returns a Google Maps overlay with the supplied name,
;; url-generating function and opacity."
;;   [name-str url-func opacity]
;;   (let [opts (clj->js
;;               (merge overlay-defaults
;;                      {:name name-str
;;                       :opacity opacity
;;                       :getTileUrl url-func
;;                       }))]
;;     (google.maps.ImageMapType. opts)))

;; (def map-opts
;;   "Default initial map options."
;;   {:zoom 10
;;    :mapTypeId google.maps.MapTypeId.ROADMAP
;;    :center (google.maps.LatLng. 32 34.8)
;;    :styles [{:stylers [{:visibility "on"}
;;                        {:lightness 50}]}]
;;    })

;; (defn init-map
;;   [element overlays]
;;   (let [options (clj->js map-opts)
;;         map (google.maps.Map. element options)
;;         types (.-overlayMapTypes map)
;;         ]
;;     (doseq [layer overlays]
;;       (.push types layer))
;;     map))

;; (def *map*
;;   "Dynamic variable holding our map element, set to an initial value
;; of nil. We don't really need to bind this to anything, but it helps
;; to have a reference to it from the callback for later coding."
;;   nil)

;; (def *overlay*
;;   "Dynamic variable holding our overlay."
;;   nil)

;; (defn map-load []
;;   (letfn [
;;           (tile-url [coord zoom]
;;             (str (.-URL js/document)
;;                  zoom "/" (.-x coord) "/" (.-y coord) ".png"))
;;           ]
;;     (set! *overlay* (mk-overlay "overlay1" tile-url 0.6))
;;     (set! *map* (init-map
;;                  (goog.dom/getElement "map_canvas")
;;                  [*overlay*]))
;;     ))

;; (events/listen js/window "load" map-load)

;; (def L (this-as ct (aget ct "L")))

(def L js/L)

;; (def lmap
;;   (-> L
;;       (.map "map")
;;       (.setView (clj->js [32 35]) 13)
;;       ))

;;(blade/bootstrap)

(def tile-url
  "http://{s}.tile.cloudmade.com/BC9A493B41014CAABB98F0471D759707/997/256/{z}/{x}/{y}.png")

(let [mappy (-> L (.map "map") 
                (.setView (clj->js [32 34.8]) 11)
                )
      ]

  (-> L (.tileLayer tile-url {
            :maxZoom 18
            :attribution "Map data &copy; <a href=\"http://openstreetmap.org\">OpenStreetMap</a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA</a>, Imagery © <a href=\"http://cloudmade.com\">CloudMade</a>"})
        (.addTo mappy))

  (-> L (.marker (clj->js [32 34.8])) 
        (.addTo mappy)
        ;; (.bindPopup "<b>Hello world!</b><br />I am a popup.")
        ;; (.openPopup)
        )

  ;; (-> L (.circle (clj->js [34.0286, -118.5486]) 1000 {
  ;;                                                     :color "red"
  ;;                                                     :fillColor "#f03"
  ;;                                                     :fillOpacity "0.5"})
  ;;     (.addTo mappy)
  ;;     (.bindPopup "I am a circle."))

  ;; (-> L (.polygon (clj->js [[33.979, -118.48]
  ;;                           [33.973, -118.46]
  ;;                           [33.98, -118.447]]))
  ;;     (.addTo mappy)
  ;;     (.bindPopup "I am a <del>polygon</del> triangle"))

  (let [popup (-> L .popup)]
    (.on mappy "click" (fn [{:keys [latlng]} e]
    (-> popup (.setLatLng latlng)
              (.setContent (str "You clicked the map at " latlng))
              (.openOn mappy)))))
  )
