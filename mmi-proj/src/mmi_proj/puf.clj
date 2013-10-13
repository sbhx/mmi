(ns mmi-proj.puf
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
;;(require '[clojure.data.csv :as csv])
(require 'clojure-csv.core)
(require 'clojure.reflect)
(import '[org.jfree.chart ChartPanel JFreeChart])
(import '[javax.swing JComponent JLabel JPanel])
(require 'nuroko.gui.visual)
(use '[clojure.algo.generic.functor :only [fmap]])
(import 'java.lang.Math)
(require '[seesaw.core :as s])
(require '[seesaw.font :as sf])
(require '[clojure.core.reducers :as r])

(require '[cemerick.pomegranate :as p])
(defn add-classpath-with-println [jar]
  (do
    (println (str "adding " jar))
    (p/add-classpath jar)))




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

(defn round3 [x]
  (float (/ (Math/round (* x
                           1000))
            1000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def puf-filename
  "/home/we/workspace/PUF 2008/H20081171Data.csv")

(defn read-cols-and-rows [filename & {:keys [seq-transformer]
                                      :or {seq-transformer identity}}]
  (let [file-reader (clojure.java.io/reader filename)
        column-names (->> (.readLine file-reader)
                          (#(clojure.string/split % #","))
                          (map keyword))
        ;; Note that the side effect of the last let-element
        ;; is that the file-reader has progressed to its 2nd
        ;; line.
        lines (seq-transformer (line-seq file-reader))
        rows-vals (map (comp first clojure-csv.core/parse-csv)
                       lines)
        ;;rows-vals (csv/read-csv file-reader)
        rows (map (fn [row-vals]
                    (apply hash-map
                           (interleave column-names row-vals)))
                  (seq-transformer rows-vals))]
    {:column-names column-names
     :rows rows}))


(defn transform-cols-and-rows
  [new-columns-fns cols-and-rows]
  {:column-names (keys new-columns-fns)
   :rows (map
          (fn [row]
            (apply hash-map
                   (apply concat
                          (map (fn [column-name]
                                 [column-name ((column-name new-columns-fns)
                                               row)])
                               (keys new-columns-fns)))))
          (:rows cols-and-rows))})

(defn take-cols-and-rows
  [nrows cols-and-rows]
  {:column-names (:column-names cols-and-rows)
   :rows (take nrows (:rows cols-and-rows))})

(defn sample-from-cols-and-rows
  [size cols-and-rows]
  {:column-names (:column-names cols-and-rows)
   :rows (sample (:rows cols-and-rows)
                 :size size)})

(defn cols-and-rows-to-dataset
  [cols-and-rows]
  (dataset (:column-names cols-and-rows)
           (:rows cols-and-rows)))

;; (defn read-csv-dataset [cols-and-rowvals column-names]
;;   (let [column-indices (map #(.indexOf (:column-names
;;                                         cols-and-rowvals) %)
;;                             column-names)
;;         restricted-rowvals (map (fn [row-vals]
;;                                   (map (partial nth row-vals)
;;                                        column-indices))
;;                                 (:rowvals
;;                                  cols-and-rowvals))
;;         dataset-rows (map (fn [row-vals]
;;                             (apply hash-map
;;                                    (interleave column-names row-vals)))
;;                           restricted-rowvals)]
;;     (dataset
;;      column-names
;;      dataset-rows)))



(defn put-Latin-around-Hebrew [s]
  (if (re-matches #"(?s).*[אבגדהוזחטיכלמנסעפצקרשת].*" s)
    (str "o " s " o")
    s))


(def from-yishuv-code-to-name
  (-> "/home/we/workspace/data/yishuv-name-code.csv"
      (read-dataset :header true)
      (transform-col :name put-Latin-around-Hebrew)
      :rows
      (#(mapcat vals %))
      (#(apply hash-map %))
      ))


;; http://www.thebusby.com/2012/07/tips-tricks-with-clojure-reducers.html
(defn fold-into-vec [coll]
  "Provided a reducer, concatenate into a vector.
Note: same as (into [] coll), but parallel."
  (r/fold (r/monoid into vector) conj coll))


(defn obj-to-keyword [obj]
  (keyword (clojure.string/replace (str obj)
                                   #"[,|{|}|:| ]"
                                   "-")))


(def codebook-map (let [codebook-dataset (read-dataset
                                          "/home/we/workspace/PUF 2008/my-processing/codebook.csv"
                                          :header true)
                        variable-name-column-name (nth (col-names codebook-dataset)
                                                       1)]
                    (apply hash-map
                           (apply concat
                                  (map (fn [row]
                                         [(keyword (variable-name-column-name row))
                                          row])
                                       (:rows codebook-dataset))))))

(defn cb [variable-keyword]
  (do (doseq [[k v] (reverse (variable-keyword codebook-map))]
        (println "___________________")
        (println k)
        (println v))
      (println "_________________________________________________________")))


(defn identity-map [aseq] 
  (apply hash-map (interleave aseq aseq)))



(def relevant-puf-columns [:DiraNosefetAchrPUF
                     :EretzLeidaZkPUF
                     :Hchns2008BrutoSachirPUF
                     :Hchns2008MbMchvPUF
                     :KayamDudShemeshPUF
                     :KayamInternetPUF
                     :KayamMachshevPUF
                     :KayamMazganPUF
                     :KayamMediachKelimPUF
                     :KayamMeyabeshKvisaPUF
                     :KayamMicrogalPUF
                     :KayamTvPUF
                     :KayamVideoDvdPUF
                     :KtvtLifney5ShanaMachozMchvPUF
                     :KtvtLifney5ShanaMetropolinPUF
                     :MspChadarimPUF
                     :MspChdshAvdShana2008PUF
                     :MspShaotAvdShavuaPUF
                     :MspSherutimPUF
                     :MspShnotLimudZkPUF
                     :NefashotMeshekBayitPUF
                     :OleShnot90MchvPUF
                     :RchvPUF
                     :RovaKtvtMegurimPUF
                     :SmlAnafKalkaliPUF
                     :SmlEzorStatistiKtvtMegurimPUF
                     :SmlMishlachYadPUF
                     :SmlYishuvPUF
                     :TatRovaKtvtMegurimPUF
                     :TelephonNayadPUF
                     :TelephonPUF
                     :TkufatSiyumBniyatDiraMchvPUF
                     :TzfifutDiurPUF
                     :TzuratAchzakatDira2008MchvPUF ])

(defn get-all-relevant-cols-and-rows []
  (->> puf-filename
       read-cols-and-rows
       (transform-cols-and-rows (identity-map relevant-puf-columns))))

(defn get-all-relevant-cols-and-some-rows [seq-transformer]
  (transform-cols-and-rows (identity-map relevant-puf-columns)
                           (read-cols-and-rows puf-filename
                                               :seq-transformer seq-transformer)))

(def pre-d
  (read-csv-dataset puf-filename
                    
                    nil))

(map (fn [col-name]
       (do (cb col-name)
           (println (frequencies ($ col-name pre-d)))
           (println "#########################################################")))
     (col-names pre-d))



(def d (transform-col
        (transform-col
         ($where {:Hchns2008MbMchvPUF {:$ne ""}} pre-d)
         :Hchns2008MbMchvPUF #(Integer/parseInt %))
        :KtvtLifney5ShanaMetropolinPUF #(> (Integer/parseInt %) 12)))

(dim pre-d)
(dim d)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PCA


:DiraNosefetAchrPUF =1
:Hchns2008BrutoSachirPUF
:KayamDudShemeshPUF
:KayamInternetPUF
:KayamMachshevPUF
:KayamMazganPUF
:KayamMediachKelimPUF
:KayamMeyabeshKvisaPUF
:KayamMicrogalPUF
:KayamTvPUF
:KayamVideoDvdPUF
:MspChadarimPUF
:MspChdshAvdShana2008PUF
:MspShaotAvdShavuaPUF
:MspSherutimPUF
:MspShnotLimudZkPUF
:NefashotMeshekBayitPUF
:RchvPUF
:SmlAnafKalk
:SmlMishlachYadPUF
:TelephonNayadPUF
:TelephonPUF
:TkufatSiyumBniyatDiraMchvPUF
:TzfifutDiurPUF
:TzuratAchzakatDira2008MchvPUF








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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












;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

























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



(def comparison
  (let [;;;;
        transformed-clean-d
        (transform-col-and-rename clean-d
                                  :TkufatSiyumBniyatDiraMchvPUF
                                  :new-apt
                                  #(= % "9"))
        ;;;;
        combinations
        (->> transformed-clean-d
             ($ [:KtvtLifney5ShanaMachozMchvPUF :SmlYishuvPUF :new-apt])
             :rows
             freqs-as-rows
             (filter #(< 50 (:count %)))
             (map (fn [row] (conj (select-keys row [:count])
                                  (:val row))))
             )
        ;;;;
        combinations-with-measures
        (into [] (r/map (fn [row]
                          (let [incomes ($ :Hchns2008MbMchvPUF
                                           ($where (dissoc row :count)
                                                   transformed-clean-d))]
                            (conj row
                                  {:median-income (median incomes)
                                   :mean-income (round3 (mean incomes))})))
                        (r/filter
                         ;;identity
                         #(.endsWith (:SmlYishuvPUF %) "00")
                         combinations)))]
    ($order
     [:SmlYishuvPUF :new-apt] :asc
     (to-dataset combinations-with-measures))))
;;($group-by [:SmlYishuvPUF :new-apt])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defn comparison [sml-yishuv]
  (let [;;;;
        transformed-clean-d
        (transform-col-and-rename
         (transform-col-and-rename
          ($where {:SmlYishuvPUF sml-yishuv} clean-d)
          :TkufatSiyumBniyatDiraMchvPUF
          :new-apt
          #(if (= % "9")
             "yes"
             "-"))
         :SmlYishuvPUF
         :yishuv-name
         #(from-yishuv-code-to-name (Integer/parseInt %))
         )
        ;;;;
        combinations
        (->> transformed-clean-d
             ($ [:KtvtLifney5ShanaMachozMchvPUF
                 :yishuv-name
                 :RovaKtvtMegurimPUF
                 :new-apt])
             :rows
             freqs-as-rows
             ;;(filter #(< 50 (:count %)))
             (map (fn [row] (conj (select-keys row [:count])
                                  (:val row))))
             )
        ;;;;
        combinations-with-measures
        (fold-into-vec (r/map (fn [row]
                                (let [incomes (flatten [($ :Hchns2008MbMchvPUF
                                                           ($where (dissoc row :count)
                                                                   transformed-clean-d))])]
                                  (println (count incomes))
                                  (conj row
                                        {:median-income (median incomes)
                                         :mean-income (round3 (mean incomes))})))
                              combinations))]
    ($order
     [:SmlYishuvPUF :RovaKtvtMegurimPUF :new-apt] :asc
     (to-dataset combinations-with-measures))))


;;($group-by [:SmlYishuvPUF :new-apt])

(->> d
     ($where {:RovaKtvtMegurimPUF "2"
              :SmlYishuvPUF "70"
              :TkufatSiyumBniyatDiraMchvPUF "9"})
     )



(->> clean-d
     ($where {:RovaKtvtMegurimPUF "2"
              :SmlYishuvPUF "70"
              ;;:TkufatSiyumBniyatDiraMchvPUF "9"
              })
     ($ [:Hchns2008MbMchvPUF :EretzLeidaZkPUF :TkufatSiyumBniyatDiraMchvPUF])
     ((fn [d1] (transform-col-and-rename
                d1
                :TkufatSiyumBniyatDiraMchvPUF
                :new-apt
                #(if (= % "9")
                   "yes"
                   "-"))))
     ((fn [d1] (transform-col-and-rename
                d1
                :Hchns2008MbMchvPUF
                :hcns-grp
                #(int (/ % 7)))))
     :rows
     freqs-as-rows
     to-dataset
     )


(save comparison "comparison.csv")














;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;       (float (/ (nrow ($where {:Hchns2008MbMchvPUF {:$lt 12}} clean-d))
;;                 (nrow clean-d)))

;; (->>
;;  ($where {:SmlEzorStatistiKtvtMegurimPUF "235"} d)
;;  ($where {:Hchns2008MbMchvPUF {:$ne ""}})
;;  ($ [:KtvtLifney5ShanaMetropolinPUF
;;      :Hchns2008MbMchvPUF
;;      :TkufatSiyumBniyatDiraMchvPUF])
;;  (#(transform-col-and-rename %
;;                              :Hchns2008MbMchvPUF :Hchns2008MbMchvPUF-gt12
;;                              (fn [x] (> (Integer/parseInt x) 12))))
;;  (#(transform-col-and-rename %
;;                              :KtvtLifney5ShanaMetropolinPUF :KtvtLifney5ShanaMetropolinPUF-gt12
;;                              (fn [x] (> (Integer/parseInt x) 12))))
;;  (#(transform-col-and-rename %
;;                              :TkufatSiyumBniyatDiraMchvPUF :TkufatSiyumBniyatDiraMchvPUF-gt7
;;                              (fn [x] (> (Integer/parseInt x) 7))))
;;  :rows
;;  freqs-as-rows
;;  (map (fn [row] (conj (select-keys row [:count])
;;                      (:val row))))
;;  to-dataset)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
