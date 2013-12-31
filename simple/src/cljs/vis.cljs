(ns simple.vis
  (:require [simple.utils :refer [jsObj]]
            [strokes :refer [d3]]
            [clojure.browser.dom :as dom]           
            [goog.debug.DivConsole :as goog-dc]
            [goog.events :as goog-events]
            [goog.ui.ComboBox :as combo-box]))

;;;;;;;;;;;;;;;;;;;;;;

(def L js/L)
(def data js/data)



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

(update-info (aget data 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def tile-url
  "http://{s}.tile.cloudmade.com/BC9A493B41014CAABB98F0471D759707/22677/256/{z}/{x}/{y}.png"
  ;;"http://{s}.tile.cloudmade.com/BC9A493B41014CAABB98F0471D759707/997/256/{z}/{x}/{y}.png"
  )


(def polygon1
  [[34.83930039690187 32.12498458670912] [34.839325770586925 32.1239644139855] [34.839331467301015 32.12373563397999] [34.83935084155226 32.123733220727175] [34.83935415795748 32.12373280647766] [34.83937672567512 32.123729991276015] [34.83954011675081 32.12370972298897] [34.83984651105227 32.1236717387495] [34.83985263412659 32.12364929642622] [34.83985962263 32.1236266815652] [34.839963213812396 32.12329232779698] [34.84010977552333 32.12302320154724] [34.84024898955025 32.12246999373329] [34.84024910022874 32.122467205683] [34.840267899878974 32.12200383573056] [34.840193002642906 32.121464519972456] [34.84007300492403 32.12081227306013] [34.84004490577097 32.12081633510561] [34.84000217257262 32.12082261994806] [34.839790942507165 32.12088567633089] [34.83970050465033 32.12092954754633] [34.8395140694567 32.1210199647023] [34.83937157568377 32.12108908916177] [34.83915691022109 32.1212141667039] [34.83912073452882 32.121173488956295] [34.83903668533779 32.12110525332605] [34.83903396865804 32.121103050455645] [34.83884061169577 32.120946060607736] [34.838504771252225 32.120795753992795] [34.83849160245334 32.120791890587306] [34.83830843526763 32.120738283430086] [34.83809900734896 32.12069808722585] [34.83791341298059 32.12068176870125] [34.83781084392662 32.1206804831589] [34.837745256902856 32.12067912467818] [34.837544170680005 32.120697746349144] [34.83737684645856 32.12072656944629] [34.837070968925914 32.12081873972644] [34.83665862592904 32.12019378219871] [34.83666203144875 32.12018853624197] [34.8368262418852 32.119935540183576] [34.83690914851779 32.11979167130215] [34.83695737395294 32.119707860243466] [34.837031955046875 32.11918440758285] [34.83691882871303 32.118978238527866] [34.83691687815642 32.118974679977974] [34.83657041344179 32.11872721449331] [34.836515613453784 32.1187072014557] [34.835667243423536 32.11839723904836] [34.835408052116264 32.1184763805378] [34.83517922486679 32.11851926988806] [34.83514085060413 32.1185215962629] [34.83512027983932 32.118522981077085] [34.83499532778774 32.118531089489] [34.83480395722784 32.11852998817785] [34.83415800026103 32.11850276024341] [34.833894839828965 32.11849126060304] [34.833767413282004 32.1184824090786] [34.83364420270544 32.11847808037205] [34.833393422260805 32.118467252771964] [34.83199263926864 32.118407313216274] [34.83163592713329 32.11839219374687] [34.83156705689861 32.118388804057055] [34.83144861652658 32.11838298850234] [34.8314260511351 32.11838184156557] [34.83092049595767 32.118356821570856] [34.83037485628435 32.11833995761872] [34.83034921179384 32.11834086652475] [34.830092850154415 32.118350306155335] [34.83012351559023 32.11888813924157] [34.83013308621256 32.11908330932623] [34.830142814914126 32.11922059397512] [34.830173197994746 32.119433680826035] [34.830195070439416 32.11949976201372] [34.83027287360564 32.11993537212821] [34.83046059977477 32.120284562049356] [34.83046835572336 32.12028471100468] [34.83121977824064 32.120299432705885] [34.831190584202844 32.12033757909548] [34.83104324604929 32.120609389195664] [34.83100249990437 32.12075364714696] [34.83097484569317 32.12093609139859] [34.83096333980815 32.12105374078848] [34.8308596112198 32.12211708916832] [34.83077881854339 32.122752344754254] [34.830775337513614 32.1227796605886] [34.83074693477216 32.12319592521151] [34.8307367798814 32.12334487252284] [34.83073061252713 32.12339842057471] [34.83069257787832 32.123656419916095] [34.83069040465036 32.12367118479894] [34.83068855570265 32.12368335161263] [34.83068135426622 32.123969740094374] [34.83067597641835 32.1240136451199] [34.83066293381546 32.124119664167935] [34.83073153381807 32.124410291106784] [34.83073293398876 32.124416191210074] [34.83085929595704 32.12469187107634] [34.83089207105401 32.12468700972361] [34.83111388206964 32.12465439357335] [34.831434310976384 32.12463359981195] [34.831612326263254 32.124638550066805] [34.83163947767986 32.12464016126648] [34.831794221759175 32.12464936067931] [34.83189916267066 32.12466887769855] [34.832000805262915 32.12466349713097] [34.83210857636211 32.12463847944015] [34.83224261084253 32.124641661975645] [34.832519407759605 32.124667274712245] [34.832838358859476 32.12467272035866] [34.833031802610435 32.12468681490444] [34.833427664464885 32.12470014638177] [34.83405755595598 32.12466845516475] [34.834057373502 32.12466222676441] [34.83406900488942 32.12466794045674] [34.8342795699467 32.1247710968225] [34.834568492867824 32.124890248804924] [34.83511409152053 32.12500456538003] [34.835232978326644 32.12503485163254] [34.83528092070589 32.12505131545611] [34.835420264082245 32.1251147621752] [34.83546412347128 32.125134729482575] [34.835597415112 32.12521681279456] [34.8357216801526 32.12525424369491] [34.83579671535945 32.12543770571421] [34.83580371878044 32.125460719953246] [34.8358342540516 32.125458914920095] [34.83706356368551 32.12538773056563] [34.83817154167434 32.12532348386573] [34.83827320137478 32.12531764202589] [34.83917203093174 32.125265419311695] [34.83929360902691 32.125258465609924] [34.83929412380163 32.12523510172113] [34.83929436744359 32.125228169483655] [34.83929474690477 32.12521165888598] [34.83930039690187 32.12498458670912]])

(def show-map
  (let [mappy (-> L (.map "map") 
                  (.setView (clj->js [32 34.7]) 12))]
    (fn [colorkey]
      (let [
            geojsonMarkerOptions {:radius 5,
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
                        (let [p (.-plotting d)]
                          {:type "Feature"
                           :properties {:d d}
                           :geometry {:type "Point"
                                      :coordinates [(aget d "mean-x")
                                                    (aget d "mean-y")]}})))
            gjPolygons (clj->js
                        (for [d (filter #(aget % "polygon")
                                        data)]
                          {:type "Feature",
                           :properties {:d d}
                           :geometry {:type "Polygon",
                                      :coordinates [(aget d "polygon")]}}))]
        (-> L
            (.geoJson gjPolygons
                      (clj->js
                       {:style (fn [feature]
                                 (clj->js {:color (aget (.-color
                                                         (.-plotting
                                                          (.-d
                                                           (.-properties feature))))
                                                        colorkey)
                                           :weight 2
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
                                                (update-info d)))}))))}))
            (.addTo mappy))
        ;; (-> L
        ;;     (.geoJson gjPoints
        ;;               (clj->js
        ;;                {:pointToLayer
        ;;                 (fn [feature latlng]
        ;;                   (-> L
        ;;                       (.circleMarker latlng
        ;;                                      (clj->js
        ;;                                       (assoc geojsonMarkerOptions
        ;;                                         :fillColor 
        ;;                                         (.-color
        ;;                                          (.-plotting
        ;;                                           (.-d
        ;;                                            (.-properties feature)))))))))
        ;;                 :onEachFeature
        ;;                 (fn [feature layer]
        ;;                   (let [d (.-d (.-properties feature))
        ;;                         p (.-plotting d)
        ;;                         desc (str (.-desc d)
        ;;                                   " "
        ;;                                   (.-lr13str p))
        ;;                         color (.-color p)]
        ;;                     (.on layer
        ;;                          (clj->js
        ;;                           {:mouseover (fn []
        ;;                                         (do (-> title
        ;;                                                 (.text desc)
        ;;                                                 (.attr "fill" color))
        ;;                                             (update-info d)))
        ;;                            :mouseout (fn []
        ;;                                        (-> title
        ;;                                            (.text "")))
        ;;                            :click (fn []
        ;;                                     (do (-> title
        ;;                                             (.text desc)
        ;;                                             (.attr "fill" color))
        ;;                                         (update-info d)))}))))}))
        ;;     (.addTo mappy))
        (-> L (.tileLayer tile-url {
                                    :maxZoom 18
                                    :attribution "Map data &copy ; <a href=\"http://openstreetmap.org\">OpenStreetMap</a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA</a>, Imagery © <a href=\"http://cloudmade.com\">CloudMade</a>"})
            (.addTo mappy))
        (let [popup (-> L .popup)]
          (.on mappy "click" (fn [{:keys [latlng]} e]
                               (-> popup (.setLatLng latlng)
                                   (.setContent (str "You clicked the map at " latlng))
                                   (.openOn mappy)))))))))


(defn ^:export setup []
  (let [el (dom/get-element "colorkeycombo")
        cb (goog.ui.ComboBox.)]
    (doto cb
      (.setUseDropdownArrow true)
      (.setDefaultText "Select colorkey...")
      (.addItem (goog.ui.ComboBoxItem. "label"))
      (.addItem (goog.ui.ComboBoxItem. "smooth"))
      (.render el))
    (.listen goog.events
             cb "change"
             (fn [e]
               (show-map
                (.getValue (.-target e)))
               ;; (. goog.dom (setTextContent (dom/get-element "v") (.. e target (getValue))))
               )
             ;; (fn [e] (do (js/alert (.. e target (getValue)))
             ;;            ;; (show-map
             ;;            ;;  (.. e target (getValue)))
             ;;            ))
             )))

;(show-map "label")

