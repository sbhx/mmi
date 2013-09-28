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
(require '[clojure.data.csv :as csv])
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://gist.github.com/daveray/1441520
;; (s/native!)

;; (def frames
;;   (atom {}))

;; (defn sdisplay [frame-key content]
;;   (let [frame (do (let [existing-frame (@frames frame-key)]
;;                     (if existing-frame
;;                       (do
;;                         (println "found existing frame")
;;                         existing-frame)     ;
;;                       ;; else
;;                       (do (let [new-frame (s/frame)]
;;                             (println "adding new frame")
;;                             (swap! frames assoc frame-key new-frame)
;;                             new-frame)))))]
;;     (when (not (.isShowing frame))
;;       (-> frame s/pack! s/show!))
;;     (s/config! frame
;;                :content (s/border-panel
;;                          :north (doto (s/label (str "   " frame-key))
;;                                   (s/config! :background "#aabbcc"
;;                                              :foreground "#334422"
;;                                              :font (sf/font :name :monospaced
;;                                                             :style #{:bold}
;;                                                             :size 10)))
;;                          :center content))))




;; (defn show-chart [chart]
;;   (sdisplay 1 (ChartPanel. chart)))



;; https://gist.github.com/daveray/1441520
(s/native!)

(def frames
  (atom {}))

(defn sdisplay [frame-key content controls]
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
                         :north (s/horizontal-panel :items
                                                    (concat [(doto (s/label (str "  frame: " frame-key "   "))
                                                               (s/config! :background "#ddeeff"
                                                                          :foreground "#555555"
                                                                          :font (sf/font :name "ARIAL"
                                                                                         :style #{:bold}
                                                                                         :size 21)))]
                                                            (if controls
                                                              [controls]
                                                              ;;else
                                                              [])))
                         :center content))))

;; (sdisplay "frame-1"
;;           (s/horizontal-panel
;;            :items [(ChartPanel. (xy-plot (range 99) (map  #(Math/sin %)
;;                                                           (range 99))))
;;                    (ChartPanel. (scatter-plot (range 99) (range 99)))])
;;           nil)

(defn new-chart-panel-with-controls [chart]
  (let [b (s/button :text "Auto Zoom")
        chart-panel (doto (ChartPanel. chart)
                      (.add b))]
    (s/listen b :action
            (fn [e] (.restoreAutoBounds chart-panel)))
    chart-panel))

(defn new-panel-with-chart-and-controls [chart]
  (let [b (s/button :text "Auto Zoom")
        chart-panel (ChartPanel. chart)]
    (s/listen b :action
              (fn [e] (.restoreAutoBounds chart-panel)))
    (s/horizontal-panel :items
                      [chart-panel
                       b])))

(defn new-chart-panel-and-controls [chart]
  (let [b (s/button :text "Auto Zoom")
        chart-panel (ChartPanel. chart)]
    (s/listen b :action
              (fn [e] (.restoreAutoBounds chart-panel)))
    {:chart-panel chart-panel
     :controls b}))


(defn show-chart [chart]
  (let [chart-panel-and-controls (new-chart-panel-and-controls chart)]
    (sdisplay 1
              (:chart-panel chart-panel-and-controls)
              (:controls chart-panel-and-controls))))

(defn show-charts [charts]
  (sdisplay 1
            (s/vertical-panel :items
                              ;;(map new-panel-with-chart-and-controls
                              ;;charts)
                              (map #(ChartPanel. %)
                                   charts))
            nil))

(defn scatter-plot-wrt-index [x]
  (scatter-plot (range (count x))
                x))

(defn show-plot [x]
  (show-chart (scatter-plot-wrt-index x)))












;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn leave-only-nil-and-values-of-set [vals-set]
  (fn [x]
    (if x (if (vals-set x)
            x
            ;; else
            :other))))

;; (= 
;;  (map (leave-only-nil-and-values-of-set #{4 "A"})
;;       [3 nil 3 4 "a" 1 "A" nil "A" 132])
;;  [:other nil :other 4 :other :other "A" nil "A" :other])


(defn freqs-as-rows [x]
  (map #(hash-map :val (first %) :count (second %))
       (sort-by (comp  - second) (frequencies x))))


(defn and-func [x y]
  (and x y))

(defn filter-all-nonnil [adataset]
  (to-dataset
   (filter #(reduce and-func
                    (vals %))
           (:rows adataset))))

(defn filter-full [adataset]
  (to-dataset
   (filter #(= (count %)
               (ncol adataset))
           (:rows adataset))))



(defn transform-col-and-rename
" Apply function f & args to the specified column of dataset, replace the column
  with the resulting new values, and rename it to new-column."
  [dataset column new-column f & args]
  (->> (map #(apply update-in % [column] f args) (:rows dataset))
    vec
    (assoc dataset :rows)
    (#(col-names % (replace
                    {column new-column}
                    (:column-names dataset))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def puf-filename
  "/home/we/workspace/PUF 2008/H20081171Data.csv")

(defn read-csv-seq [filename]
  (let [file-reader (clojure.java.io/reader filename)
        column-names (->> (.readLine file-reader)
                          (#(clojure.string/split % #","))
                          (map keyword))
        ;; Note that the side effect of the last let-element
        ;; is that the file-reader has progressed to its 2nd
        ;; line.
        rows-vals (csv/read-csv file-reader)]
    {:column-names column-names
     :rows-vals rows-vals}))
 

(defn read-csv-dataset [filename column-names nrows]
  (let [cols-and-rows (read-csv-seq filename)
        column-indices (map #(.indexOf (:column-names
                                        cols-and-rows) %)
                            column-names)
        restricted-rows-vals (map (fn [row-vals]
                                    (map (partial nth row-vals)
                                         column-indices))
                                  (if nrows
                                    (take nrows
                                          (:rows-vals
                                           cols-and-rows))
                                    ;; else
                                    (:rows-vals
                                           cols-and-rows)))
        dataset-rows (map (fn [row-vals]
                            (apply hash-map
                                   (interleave column-names row-vals)))
                          restricted-rows-vals)]
    (dataset
     column-names
     dataset-rows)))


(def d 
  (read-csv-dataset puf-filename
                    [:SmlYishuvPUF
                     :SmlEzorStatistiKtvtMegurimPUF
                     :TkufatSiyumBniyatDiraMchvPUF
                     :Hchns2008MbMchvPUF
                     :KtvtLifney5ShanaMetropolinPUF
                     :KtvtLifney5ShanaMachozMchvPUF]
                    nil))

(def clean-d (transform-col
              (transform-col
               ($where {:Hchns2008MbMchvPUF {:$ne ""}} d)
               :Hchns2008MbMchvPUF #(Integer/parseInt %))
              :KtvtLifney5ShanaMetropolinPUF #(> (Integer/parseInt %) 12)))

(dim d)
(dim clean-d)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(->> ($where {:SmlYishuvPUF "5000"} d)
     ($ [:KtvtLifney5ShanaMachozMchvPUF :KtvtLifney5ShanaMetropolinPUF])
     :rows
     freqs-as-rows
     (map (fn [row] (conj (select-keys row [:count])
                         (:val row))))
     to-dataset
     ($group-by :KtvtLifney5ShanaMetropolinPUF))


(->> ($where {:SmlYishuvPUF "5000"} d)
     ($ [:KtvtLifney5ShanaMetropolinPUF :TkufatSiyumBniyatDiraMchvPUF])
     :rows
     freqs-as-rows
     (map (fn [row] (conj (select-keys row [:count])
                         (:val row))))
     to-dataset
     ($group-by :KtvtLifney5ShanaMetropolinPUF))

(->> ($where {:SmlYishuvPUF "5000"} d)
     ($ [:KtvtLifney5ShanaMetropolinPUF :TkufatSiyumBniyatDiraMchvPUF])
     :rows
     freqs-as-rows
     (map (fn [row] (conj (select-keys row [:count])
                         (:val row))))
     to-dataset
     ($group-by :TkufatSiyumBniyatDiraMchvPUF))

(->> ($where {:SmlYishuvPUF "8300"} d)
     ($ [:KtvtLifney5ShanaMetropolinPUF :TkufatSiyumBniyatDiraMchvPUF])
     :rows
     freqs-as-rows
     (map (fn [row] (conj (select-keys row [:count])
                         (:val row))))
     to-dataset
     ($group-by :TkufatSiyumBniyatDiraMchvPUF))

(->> ($where {:SmlYishuvPUF "8300"} d)
     ($ [:KtvtLifney5ShanaMetropolinPUF :TkufatSiyumBniyatDiraMchvPUF])
     :rows
     freqs-as-rows
     (map (fn [row] (conj (select-keys row [:count])
                         (:val row))))
     to-dataset
     ($group-by :TkufatSiyumBniyatDiraMchvPUF))


(->> ($where {:SmlYishuvPUF "7900"} d)
     ($ [:KtvtLifney5ShanaMetropolinPUF :TkufatSiyumBniyatDiraMchvPUF])
     :rows
     freqs-as-rows
     (map (fn [row] (conj (select-keys row [:count])
                         (:val row))))
     to-dataset
     ($group-by :TkufatSiyumBniyatDiraMchvPUF))


(->> ($where {:SmlYishuvPUF "3000"} d)
     ($ [:KtvtLifney5ShanaMachozMchvPUF :TkufatSiyumBniyatDiraMchvPUF])
     :rows
     freqs-as-rows
     (map (fn [row] (conj (select-keys row [:count])
                         (:val row))))
     to-dataset
     ($group-by :TkufatSiyumBniyatDiraMchvPUF))



(->> ($where {:TkufatSiyumBniyatDiraMchvPUF "9"} d)
     ($ [:KtvtLifney5ShanaMachozMchvPUF :SmlYishuvPUF])
     :rows
     freqs-as-rows
     (map (fn [row] (conj (select-keys row [:count])
                         (:val row))))
     to-dataset
     ($group-by :SmlYishuvPUF)
     )





(->> (transform-col-and-rename clean-d
                               :TkufatSiyumBniyatDiraMchvPUF
                               :new-apt
                               #(= % "9"))
     ($ [:KtvtLifney5ShanaMachozMchvPUF :SmlYishuvPUF])
     :rows
     freqs-as-rows
     (take 20)
     ;;(filter #(< 50 (:count %)))
     (map (fn [row] (conj (select-keys row [:count])
                         (:val row))))
     to-dataset
     (#(map (fn [row]
              (let [incomes ($ :Hchns2008MbMchvPUF
                               ($where (dissoc row :count)
                                       clean-d))]
                (conj row
                      {:median-income (median incomes)
                       :mean-income (mean incomes)})))
            (:rows %)))
     to-dataset
     ($group-by :SmlYishuvPUF)
     )


















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


      (float (/ (nrow ($where {:Hchns2008MbMchvPUF {:$lt 12}} clean-d))
                (nrow clean-d))) 

(->>
 ($where {:SmlEzorStatistiKtvtMegurimPUF "235"} d)
 ($where {:Hchns2008MbMchvPUF {:$ne ""}})
 ($ [:KtvtLifney5ShanaMetropolinPUF
     :Hchns2008MbMchvPUF
     :TkufatSiyumBniyatDiraMchvPUF])
 (#(transform-col-and-rename %
                             :Hchns2008MbMchvPUF :Hchns2008MbMchvPUF-gt12
                             (fn [x] (> (Integer/parseInt x) 12))))
 (#(transform-col-and-rename %
                             :KtvtLifney5ShanaMetropolinPUF :KtvtLifney5ShanaMetropolinPUF-gt12
                             (fn [x] (> (Integer/parseInt x) 12))))
 (#(transform-col-and-rename %
                             :TkufatSiyumBniyatDiraMchvPUF :TkufatSiyumBniyatDiraMchvPUF-gt7
                             (fn [x] (> (Integer/parseInt x) 7))))
 :rows
 freqs-as-rows
 (map (fn [row] (conj (select-keys row [:count])
                     (:val row))))
 to-dataset)


(def dataset-by-sa
  ($group-by [:SmlYishuvPUF :SmlEzorStatistiKtvtMegurimPUF] d))


(def distrib-by-sa
  (fmap (fn [adataset]
          (let [clean-dataset ($where {:Hchns2008MbMchvPUF {:$ne ""}} adataset)
                freqs (->> (conj-cols (dataset [:new] (map (fn [x] (> (Integer/parseInt x) 12))
                                                                 ($ :KtvtLifney5ShanaMetropolinPUF clean-dataset)))
                                            (dataset [:wealthy] (map (fn [x] (> (Integer/parseInt x) 12))
                                                                     ($ :Hchns2008MbMchvPUF clean-dataset))))
                                 :rows
                                 frequencies)
                distrib (fmap #(float (/ % (nrow clean-dataset)))
                              freqs)
                mean-hchns (->> ($ :Hchns2008MbMchvPUF clean-dataset)
                                  (map #(Integer/parseInt %))
                                  mean)
                ]
            (conj distrib
                  {:mean-hchns mean-hchns})))
        dataset-by-sa))




(->>
 (map #(apply conj %) distrib-by-sa)
 to-dataset
 filter-full
 :rows
 (map (fn [row]
        (conj row
              {:new-part (+ (row {:wealthy true, :new true})
                            (row {:wealthy false, :new true}))})))
 to-dataset
 (#(scatter-plot :new-part :mean-hchns
                 :data %
                 :group-by :SmlYishuvPUF))
 show-chart
 )

(defn obj-to-keyword [obj]
  (keyword (clojure.string/replace (str obj)
                                   #"[,|{|}|:| ]"
                                   "-")))
(obj-to-keyword {:a 3 :b 4})


(->> (map #(apply conj %) distrib-by-sa)
     to-dataset
     filter-full
     (#(sel % :except-cols [:SmlYishuvPUF :SmlEzorStatistiKtvtMegurimPUF]))
     (#(col-names % (map obj-to-keyword (:column-names %))))
     scatter-plot-matrix
     show-chart )


(def cond-means-by-sa
  (fmap (fn [adataset]
          (let [clean-dataset clean-d
                rollup-mean (:rows ($rollup mean :Hchns2008MbMchvPUF [:KtvtLifney5ShanaMetropolinPUF]
                                            clean-dataset))
                cond-means {:mean-hchns-new (first (map :Hchns2008MbMchvPUF
                                                        (filter :KtvtLifney5ShanaMetropolinPUF rollup-mean)))
                            :mean-hchns-old (first (map :Hchns2008MbMchvPUF
                                                        (filter (complement :KtvtLifney5ShanaMetropolinPUF) rollup-mean)))}]
            cond-means))
          dataset-by-sa))



(->> (map #(apply conj %) cond-means-by-sa)
     to-dataset
     filter-all-nonnil
     (#(transform-col % :SmlYishuvPUF (fn [x] (= x "9000"))))
     (#(scatter-plot :mean-hchns-old :mean-hchns-new
                      :data %
                      :group-by :SmlYishuvPUF))
     show-chart
     )




(->> (map #(apply conj %) cond-means-by-sa)
     to-dataset
     filter-all-nonnil
     :rows
     (map (fn [row]
             (conj row
                   {:quot (/ (:mean-hchns-new row)
                             (:mean-hchns-old row))
                    :diff (- (:mean-hchns-new row)
                             (:mean-hchns-old row))})))
     to-dataset
     ($order [:quot] :asc)) 






(def cond-means-by-sa
  (fmap (fn [adataset]
          (let [clean-dataset (transform-col
                               (transform-col
                                ($where {:Hchns2008MbMchvPUF {:$ne ""}} adataset)
                                :Hchns2008MbMchvPUF #(Integer/parseInt %))
                               :KtvtLifney5ShanaMetropolinPUF #(> (Integer/parseInt %) 12))
                rollup-mean (:rows ($rollup mean :Hchns2008MbMchvPUF [:KtvtLifney5ShanaMetropolinPUF]
                                            clean-dataset))
                cond-means {:mean-hchns-new (first (map :Hchns2008MbMchvPUF
                                                        (filter :KtvtLifney5ShanaMetropolinPUF rollup-mean)))
                            :mean-hchns-old (first (map :Hchns2008MbMchvPUF
                                                        (filter (complement :KtvtLifney5ShanaMetropolinPUF) rollup-mean)))}]
            cond-means))
          dataset-by-sa))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





