(ns mmi-proj.analyse
  (:require quil.core
            quil.helpers.drawing
            quil.helpers.seqs
            )
  (:require [clojure.string :as string])
  (:require clojure.pprint)
  (:require clojure.inspector)
  (:require [clj-time.core :as t]))
(apply require clojure.main/repl-requires) 
(use '(incanter core stats charts io zoo))
(require 'clojure.core.matrix)
(require 'clojure.core.matrix.operators)
(use '[clojure.java.shell :only [sh]])
(use 'clojure.pprint)
(import [java.net URL])
(require 'clojure.data.csv)
(require 'clojure.reflect)
(import '[org.jfree.chart ChartPanel JFreeChart])
(import '[javax.swing JComponent JLabel JPanel])
(require 'nuroko.gui.visual)
(use '[clojure.algo.generic.functor :only [fmap]])
(import 'java.lang.Math)
(require '[seesaw.core :as s])
(require '[seesaw.font :as sf])

(require '[cemerick.pomegranate :as p])
(defn add-classpath-with-println [jar]
  (do
    (println (str "adding " jar))
    (p/add-classpath jar)))


(add-classpath-with-println "lib/org.csstudio.swt.widgets_2.2.0.201308141544.jar")
(add-classpath-with-println "lib/org.csstudio.swt.xygraph_2.2.0.201308141544.jar")
(add-classpath-with-println "lib/org.eclipse.core.commands_3.6.0.I20110111-0800.jar")
(add-classpath-with-println "lib/org.eclipse.draw2d_3.7.2.v20111017-2020.jar")
(add-classpath-with-println "lib/org.eclipse.equinox.common_3.6.0.v20110523.jar")
(add-classpath-with-println "lib/org.eclipse.jface_3.7.0.I20110522-1430.jar")
(add-classpath-with-println "lib/org.eclipse.swt_3.7.1.v3738a.jar")
(add-classpath-with-println "lib/org.eclipse.swt.gtk.linux.x86_64_3.7.1.v3738a.jar")
(add-classpath-with-println "lib/org.eclipse.swt.win32.win32.x86_64_3.100.0.v4233d.jar")
(add-classpath-with-println "lib/SWT_XYGraph_Examples_linux_x86_64.jar")

(import 'org.csstudio.swt.xygraph.dataprovider.CircularBufferDataProvider)
(import 'org.csstudio.swt.xygraph.figures.ToolbarArmedXYGraph)
(import 'org.csstudio.swt.xygraph.figures.Trace)        
(import 'org.csstudio.swt.xygraph.figures.Trace$PointStyle)
(import 'org.csstudio.swt.xygraph.figures.XYGraph)
(import 'org.eclipse.draw2d.LightweightSystem)
(import 'org.eclipse.swt.widgets.Display)
(import 'org.eclipse.swt.widgets.Shell)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://swt-xy-graph.googlecode.com/git/PureJava/org.csstudio.swt.xygraph/examples/SimpleToolbarArmedXYGraphExample.java
(def shell (doto (Shell.)
             (.setSize 600 400)
             (.open)))
;;use LightweightSystem to create the bridge between SWT and draw2D
(def lws (LightweightSystem. shell))
;;create a new XY Graph.
(def xy-graph (doto (XYGraph.)
                (.setTitle "Simple Toolbar Armed XYGraph Example")))
(def toolbar-armed-xy-graph (ToolbarArmedXYGraph. xy-graph))
;;set it as the content of LightwightSystem
(.setContents lws toolbar-armed-xy-graph)
(.setShowMajorGrid (.primaryXAxis xy-graph)
                   true)
(.setShowMajorGrid (.primaryYAxis xy-graph)
                                     true)
;;create a trace data provider, which will provide the data to the trace.
(def trace-data-provider (doto (CircularBufferDataProvider. false)
                           (.setBufferSize 100)
                           (.setCurrentXDataArray (double-array [10, 23, 34, 45, 56, 78, 88, 99]))
                           (.setCurrentYDataArray (double-array [11, 44, 55, 45, 88, 98, 52, 23]))))
;;create the trace and set trace property
(def the-trace (doto (Trace. "Trace1-XY Plot" 
                             (.primaryXAxis xy-graph)
                             (.primaryYAxis xy-graph)
                             trace-data-provider)
                 (.setPointStyle org.csstudio.swt.xygraph.figures.Trace$PointStyle/XCROSS)))
;;add the trace to xyGraph
(.addTrace xy-graph the-trace)
(def the-display (Display/getDefault))	   
(while (not (.isDisposed shell))
  (when (not (.readAndDispatch the-display))
    (.sleep the-display)))




;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://gist.github.com/daveray/1441520
(s/native!)

(def frames
  (atom {}))

(defn sdisplay [frame-key content]
  (let [frame (do (let [existing-frame (@frames frame-key)]
                    (if existing-frame
                      (do
                        (println "found existing frame")
                        existing-frame)     ;
                      ;; else
                      (do (let [new-frame (s/frame)]
                            (println "adding new frame")
                            (swap! frames assoc frame-key new-frame)
                            new-frame)))))]
    (when (not (.isShowing frame))
      (-> frame s/pack! s/show!))
    (s/config! frame
               :content (s/border-panel
                         :north (doto (s/label (str "   " frame-key))
                                  (s/config! :background "#aabbcc"
                                             :foreground "#334422"
                                             :font (sf/font :name :monospaced
                                                            :style #{:bold}
                                                            :size 10)))
                         :center content))))




(defn show-chart [chart]
  (sdisplay 1 (ChartPanel. chart)))


;; (sdisplay "frame-1" (s/horizontal-panel
;;                      :items [(ChartPanel. (xy-plot (range 99) (map  #(Math/sin %)
;;                                                                     (range 99))))
;;                              (ChartPanel. (scatter-plot (range 99) (range 99)))]
;;                                ))


;;;;



(def neve-sharet-filename
  "/home/we/workspace/data/NeveSharet.csv")

(defn convert-Asaf-date-string-to-year-month-day [Asaf-date-string]
  (->> Asaf-date-string
       (#(clojure.string/split % #" "))
       first
       (#(clojure.string/split % #"-"))
       (map #(Integer/parseInt %))))

(def d (let [dataset-from-file (read-dataset
                                 neve-sharet-filename
                                 :header true)
             ymds (map convert-Asaf-date-string-to-year-month-day
                       ($ :deal_date dataset-from-file))
             year (map first ymds)
             date (map #(clj-time.coerce/to-long (apply t/date-time %)) 
                                          ymds)
             ]
         (conj-cols dataset-from-file
                    (dataset [:price-per-net-area :log-price-per-net-area]
                             (map #(if-let [net-area (:net_area %)]
                                     (let [ppna (float (/ (:estimate_price %)
                                                          net-area))]
                                       [ppna (Math/log ppna)]))
                                  (:rows dataset-from-file)))
                    (dataset [:price-per-room :log-price-per-room]
                             (map #(let [ppr (float ( / (:estimate_price %)
                                                        (:rooms_number %)))]
                                     [ppr (Math/log ppr)])
                                  (:rows dataset-from-file)))
                    (dataset [:year] year)
                    (dataset [:year-month-in-years] (map #(float (+ (* 1/12 (second %))
                                                                     (first %)))
                                                    ymds))
                    (dataset [:date] date)
                    (dataset [:date-in-years] (map #(float (+ 1970
                                                               (/ % (* 365 24 3600000))))
                                                   date))
                    (dataset [:rooms-category] (map #(if (<= % 2.5)
                                                       "-2.5"
                                                       (if (<= % 3.5)
                                                         "3-3.5"
                                                         (if (<= % 4.5)
                                                           "4-4.5"
                                                           "5-")))
                                                    ($ :rooms_number dataset-from-file))))))



(sh "libreoffice" neve-sharet-filename)


(let [d1 d;;($where {:net_area {:gt 0}} d)
      ]
  (sdisplay
   1
   (s/vertical-panel
    :items [(ChartPanel. (box-plot
                          :rooms_number
                          :data (transform-col d1 :rooms_number float)
                          :group-by :year
                          ))
            (ChartPanel. (xy-plot
                          :year-month-in-years
                          :price-per-room
                          :data ($rollup median
                                         :price-per-room
                                         [:year-month-in-years]
                                         d1)
                          ))
            (ChartPanel. (scatter-plot
                          :date-in-years
                          :log-price-per-room
                          :group-by :rooms-category
                          :data d1))
            (ChartPanel. (time-series-plot
                          :date
                          :log-price-per-room
                          :group-by :rooms-category
                          :data d1))])))





(let [d1 d;;($where {:net_area {:gt 0}} d)
      ]
  (sdisplay
   1
   (s/vertical-panel
    :items [(ChartPanel. (box-plot
                          :rooms_number
                          :data (transform-col d1 :rooms_number float)
                          :group-by :year
                          ))
            (ChartPanel. (xy-plot
                          :year-month-in-years
                          :id
                          :data ($rollup count
                                         :id
                                         [:year-month-in-years]
                                         d1)
                          ))
            (ChartPanel. (scatter-plot
                          :date-in-years
                          :log-price-per-room
                          :group-by :rooms-category
                          :data d1))
            ])))








(defn freqs-as-rows [x]
  (map #(hash-map :val (first %) :count (second %))
       (sort-by (comp  - second) (frequencies x))))

($where {:count {:gt 10}}
        (to-dataset
         (freqs-as-rows
          (:rows
           ($ [:guch :helka :building_year :year] ($where {:year {:gt 2009}} d))))))


($where {:helka 36} d)





(show-chart
 (time-series-plot
  :date
  :log-price-per-net-area
  :data ($where {:net_area {:gt 0}} d)))


          (show-chart 
           (scatter-plot
            :date
            :price-per-net-area
            :data ($ [:date :price-per-net-area] d)
            ;;:group-by ($ :guch d)
            )))



(show-chart
 (scatter-plot
  :year
  :estimate_price
  :data ($rollup median
                 :estimate_price
                 [:year ;;:guch
                  ]
                 ($where {:net_area {:gt 0}} d))
  ;;:group-by ($ :guch d)
  ))



(show-chart
 (scatter-plot
  :year
  :estimate_price
  :data ($rollup count
                 :estimate_price
                 [:year]
                 d)))



(pprint (frequencies
         (:rows ($ [:rooms_number :year] d))))

($rollup median :rooms_number [:year] d)

(pprint
 (apply conj
        (map
         (fn [colname] {colname (count (distinct ($ colname d)))})
         (col-names d)
         )))

(pprint (filter #(> (second %) 20) (frequencies (:rows ($ [:guch :helka] ($where {:year {:gt 2009}
                                                                                  :building_year {:gt 2009}} d))))))

(map :rooms_number)






