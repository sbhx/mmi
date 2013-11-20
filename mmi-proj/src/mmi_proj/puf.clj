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
(require '[clojure.data.csv])
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

(defn round [k]
  (let [p (pow 10 k)]
    (fn [x]
      (float (/ (Math/round (* x
                               p))
                p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def puf-filename
  "/home/we/workspace/PUF 2008/H20081171Data.csv")


(defn read-cols-and-rows [filename & {:keys [seq-transformer delimiter]
                                      :or {seq-transformer identity
                                           delimiter ","}}]
  (let [delimiter-pattern (re-pattern delimiter)
        file-reader (clojure.java.io/reader filename)
        column-names (->> (.readLine file-reader)
                          (#(clojure.string/split % delimiter-pattern))
                          (map keyword))
        ;; Note that the side effect of the last let-element
        ;; is that the file-reader has progressed to its 2nd
        ;; line.
        lines (seq-transformer (line-seq file-reader))
        rows-vals (map #(clojure.string/split % delimiter-pattern)
                       lines)
        ;;rows-vals (csv/read-csv file-reader)
        rows (map (fn [row-vals]
                    (apply hash-map
                           (interleave column-names row-vals)))
                  rows-vals
                  ;;(seq-transformer rows-vals)
                  )]
    (println ["reading" filename "with delimiter-pattern" (re-pattern delimiter)])
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
                           ;; problematic? :MspChdshAvdShana2008PUF
                           ;; problematic? :MspShaotAvdShavuaPUF
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

(defn freqs [col-name & {:keys [sample-size]
                         :or {sample-size 100}}]
  (future (->> (read-cols-and-rows puf-filename
                                    :seq-transformer #(sample % :size sample-size))
                (transform-cols-and-rows (identity-map [col-name]))
                :rows
                (map (comp first vals))
                frequencies
                (into (sorted-map))
                (#(pprint {col-name %})))))


(defn specific-val-to-1-others-to-0 [specific-val]
  #(if (= specific-val %)
     1 0))

(defn specific-vals-to-1-others-to-0 [specific-vals-set]
  #(if (specific-vals-set %)
     1 0))

(defn threshold-to-nil [threshold]
  #(if (<= threshold %)
     nil %))

(defn parse-int-or-nil [string]
  (try (Integer/parseInt string)
                   (catch NumberFormatException e
                          (do 
                            ;; (println (str "warning: NumberFormatException "
                            ;;               (.getMessage e)
                            nil))))

(def regular-int-or-nil (comp (threshold-to-nil 90)
                              parse-int-or-nil))


(def standard-column-fns
  (conj (identity-map [:EretzLeidaZkPUF
                       :KtvtLifney5ShanaMachozMchvPUF
                       :KtvtLifney5ShanaMetropolinPUF
                       :OleShnot90MchvPUF
                       :RovaKtvtMegurimPUF
                       :TatRovaKtvtMegurimPUF
                       :SmlAnafKalkaliPUF ;; TODO: transform this
                       :SmlEzorStatistiKtvtMegurimPUF
                       :SmlMishlachYadPUF ;; TODO: transform this
                       :SmlYishuvPUF
                       :TkufatSiyumBniyatDiraMchvPUF
                       ]
                      )
        {:Hchns2008BrutoSachirPUF-int (comp parse-int-or-nil :Hchns2008BrutoSachirPUF)
         :Hchns2008MbMchvPUF-int (comp parse-int-or-nil :Hchns2008MbMchvPUF)
         :DiraNosefetAchrPUF=1 (comp (specific-val-to-1-others-to-0 "1")
                                     :DiraNosefetAchrPUF)
         :KayamMachshevPUF (comp (specific-val-to-1-others-to-0 "1")
                                 :KayamMachshevPUF)
         :KayamMazganPUF (comp (specific-val-to-1-others-to-0 "1")
                               :KayamMazganPUF)
         :KayamMediachKelimPUF (comp (specific-val-to-1-others-to-0 "1")
                                     :KayamMediachKelimPUF)
         :KayamMeyabeshKvisaPUF (comp (specific-val-to-1-others-to-0 "1")
                                      :KayamMeyabeshKvisaPUF)
         :KayamMicrogalPUF (comp (specific-val-to-1-others-to-0 "1")
                                 :KayamMicrogalPUF)
         :KayamTvPUF (comp (specific-val-to-1-others-to-0 "1")
                           :KayamTvPUF)
         :KayamVideoDvdPUF (comp (specific-val-to-1-others-to-0 "1")
                                 :KayamVideoDvdPUF)
         :MspChadarimPUF-reg (comp regular-int-or-nil
                                   :MspChadarimPUF)
         :MspSherutimPUF-reg (comp regular-int-or-nil
                                   :MspSherutimPUF)
         :MspShnotLimudZkPUF-reg (comp regular-int-or-nil
                                       :MspShnotLimudZkPUF)
         ;; so we will filter out ages 0-14!
         :NefashotMeshekBayitPUF-reg (comp regular-int-or-nil
                                           :MspShnotLimudZkPUF)
         :RchvPUF-reg (comp regular-int-or-nil
                            :RchvPUF)
         :TelephonNayadPUF-reg (comp regular-int-or-nil
                                     :TelephonNayadPUF)
         :TelephonPUF-reg (comp regular-int-or-nil
                                :TelephonPUF)
         :new-apt (comp (specific-val-to-1-others-to-0 "9")
                        :TkufatSiyumBniyatDiraMchvPUF)
         ;; Note redundancy of information with
         ;; the column :TkufatSiyumBniyatDiraMchvPUF
         :TzfifutDiurPUF-reg (comp regular-int-or-nil
                                   :TzfifutDiurPUF)
         :apt-owned-by-family (comp (specific-vals-to-1-others-to-0 #{"1" "4"})
                                    :TzuratAchzakatDira2008MchvPUF)}))


(def pca-columns [:Hchns2008MbMchvPUF-int
                  ;; too many missing values? :Hchns2008BrutoSachirPUF-int
                  :MspShnotLimudZkPUF-reg
                  :RchvPUF-reg
                  :DiraNosefetAchrPUF=1
                  ;; :MspSherutimPUF-reg
                  :KayamMachshevPUF
                  ;; :MspChadarimPUF-reg
                  :TelephonPUF-reg
                  :KayamTvPUF
                  :KayamMicrogalPUF
                  :KayamMediachKelimPUF
                  :TelephonNayadPUF-reg
                  ;; :TzfifutDiurPUF-reg
                  :KayamVideoDvdPUF
                  :KayamMeyabeshKvisaPUF
                  :KayamMazganPUF
                  :apt-owned-by-family
                  ;; relevant? :NefashotMeshekBayitPUF-reg
                  ;; :SmlAnafKalkaliPUF ;; TODO: transform this
                  ;; :SmlMishlachYadPUF ;; TODO: transform this
                  ])
;; NOTE: Avoiding home-related vars to avoid interaction with :new-apt.


(def col-names-to-avoid [:Hchns2008BrutoSachirPUF-int
                       :MspSherutimPUF-reg
                       :MspChadarimPUF-reg
                       :TzfifutDiurPUF-reg
                       :NefashotMeshekBayitPUF-reg
                       :SmlAnafKalkaliPUF
                       :SmlMishlachYadPUF])

;; TODO: Remove duplicate versions of this fn in different files.
(defn filter-all-nonnil [adataset]
  (to-dataset
   (filter #(reduce and-func
                    (vals %))
           (:rows adataset))))


(def comp1-filename
  "/home/we/workspace/data/pca/comp1.clj")

(comment (def pca-result
           (let [data-for-pca (->> (read-cols-and-rows puf-filename
                                                       :seq-transformer #(sample % :size 10000)
                                                       ;;(partial take 1000)
                                                       )
                                   (transform-cols-and-rows
                                    (select-keys standard-column-fns pca-columns))
                                   cols-and-rows-to-dataset
                                   filter-all-nonnil
                                   )
                 pca (principal-components (to-matrix data-for-pca))]
             {:pca pca
              :col-names (col-names data-for-pca)}))
         (let [comp1 (apply hash-map (interleave (:col-names pca-result)
                                                 ($ 0 (:rotation (:pca pca-result)))))]
            (spit
             comp1-filename
             (with-out-str
               (pprint comp1)))))

(def comp1
  (load-file comp1-filename))


(defn replace-columns-by-linear-combination
  [coeff-map new-col-name cols-and-rows]
  (let [coeff-vals (vals coeff-map)
        coeff-col-names (keys coeff-map)]
    {:column-names (:column-names cols-and-rows)
     :rows (for [row (:rows cols-and-rows)]
             (let [relevant-vals (map row coeff-col-names)
                   lincomb (if (every? identity relevant-vals)
                             (apply
                              +
                              (map *
                                   relevant-vals
                                   coeff-vals)))]
               (assoc
                   (apply dissoc row (keys coeff-map))
                 new-col-name lincomb)))}))



(defn order [values]
  (map second
       (sort-by first
                (map vector
                     values
                     (range (count values))))))

(defn uniformize [values]
  (map /
       (map inc
            (order (order values)))
       (repeat (inc (count values)))))

(comment
  (let [x (repeatedly 9 rand)]
    (= (order x)
       (order (uniformize x)))))




;;;;;;;;;;;;;;;;;;;;;



(def coords-filename "/home/we/workspace/data/coords.csv")

(comment
  (let [coords (->> (read-cols-and-rows "/home/we/workspace/data/salesDetails1.tsv"
                                        :delimiter "\t")
                    (transform-cols-and-rows {:x #(Double/parseDouble (:x %))
                                              :y #(Double/parseDouble (:y %))
                                              :cityCode :cityCode
                                              :statAreaCode (comp
                                                             #(if (= % "null")
                                                                nil
                                                                %)
                                                             :statAreaCode)})
                    cols-and-rows-to-dataset
                    ($group-by [:cityCode :statAreaCode])
                    (map (fn [case-and-data]
                           (conj (first case-and-data)
                                 {:mean-x (mean ($ :x (second case-and-data)))
                                  :mean-y (mean ($ :y (second case-and-data)))})))
                    to-dataset)]
    (save coords coords-filename)
    (println ["wrote coords of dim" (dim coords)
              "to" coords-filename])))

(def coords (read-dataset coords-filename
                          :header true))

(comment
  (sdisplay 1
            (ChartPanel.
             (scatter-plot :mean-x
                           :mean-y
                           :data coords
                           :group-by :cityCode))
            nil))

(def coords-map
  (apply conj
         (for [row (:rows coords)]
           {{:SmlYishuvPUF (:cityCode row)
              :SmlEzorStatistiKtvtMegurimPUF (:statAreaCode row)}
            (select-keys row [:mean-x
                              :mean-y])})))


;;;;;;;;;;;;;;;;;;;;;


(def pre-d-filename "/home/we/workspace/data/pre-d.csv")

(comment
  (time
   (let [pre-d 
         (->> (read-cols-and-rows puf-filename
                                  ;;:seq-transformer #(take 100000 %)
                                  )
              (transform-cols-and-rows (apply dissoc standard-column-fns
                                              col-names-to-avoid))
              (replace-columns-by-linear-combination comp1 :comp1)
              cols-and-rows-to-dataset
              filter-all-nonnil)]
     (println (dim pre-d))
     (save pre-d pre-d-filename))))


(def d
  (time (let [pre-d (read-dataset pre-d-filename
                                  :header true)
              pre-d-1 (conj-cols pre-d
                               (dataset [:mean-x :mean-y]
                                        (map coords-map
                                             (:rows
                                              ($ [:SmlYishuvPUF
                                                  :SmlEzorStatistiKtvtMegurimPUF]
                                                 pre-d)))))
              pre-d-2 (add-column :ucomp1
                                  (uniformize ($ :comp1 pre-d-1))
                                  (add-column :yishuv-name
                                              (map from-yishuv-code-to-name 
                                                   ($ :SmlYishuvPUF pre-d-1))
                                              pre-d-1))]
          (println (dim pre-d-2))
          pre-d-2)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def comparison
  (memoize
   (fn [subd cutoff]
     (let [ ;;;;
           combinations (->> subd
                             ($ [:KtvtLifney5ShanaMachozMchvPUF
                                 :yishuv-name
                                 :RovaKtvtMegurimPUF
                                 :new-apt])
                             :rows
                             freqs-as-rows
                             (filter #(< cutoff
                                         (:count %)))
                             (map (fn [row] (conj (select-keys row [:count])
                                                 (:val row)))))
;;;;
           combinations-with-measures
           (fold-into-vec (r/map (fn [row]
                                   (let [ucomp1s (flatten [($ :ucomp1
                                                              ($where (dissoc row :count)
                                                                      subd))])
                                         n (count ucomp1s)]
                                     (println n)
                                     (conj row
                                           {:n n
                                            :median-ucomp1 ((round 4) (median ucomp1s))
                                            :mean-ucomp1 ((round 4) (mean ucomp1s))})))
                                 (vec combinations)))]
       ($order
        [:SmlYishuvPUF :RovaKtvtMegurimPUF :new-apt] :asc
        (to-dataset combinations-with-measures))))))

(comment
  (let [cutoff 50
        c (comparison d cutoff)
        filename (str "/home/we/workspace/data/c" cutoff ".csv")]
    (save c filename)
    (println ["wrote" filename])
    (print (dim c))))

(def c50 (read-dataset "/home/we/workspace/data/c50.csv"
                       :header true))


;;;;

(def stat-areas
  (distinct
   ($ [:SmlYishuvPUF :SmlEzorStatistiKtvtMegurimPUF]
      d)))

(defn mean-ucomp1-by-stat-area [subd]
  ($rollup mean
           :ucomp1
           [:SmlYishuvPUF :SmlEzorStatistiKtvtMegurimPUF]
           subd))

(defn mean-ucomp1-by-coords [subd]
  (to-dataset
   (map (fn [row]
          (into row
                (coords-map (select-keys row
                                         [:SmlYishuvPUF
                                          :SmlEzorStatistiKtvtMegurimPUF]))))
        (:rows (mean-ucomp1-by-stat-area subd)))))


(defn mean-ucomp1s-by-stat-area [subd]
  (to-dataset
   (for [[k v] ($group-by 
                [:SmlYishuvPUF :SmlEzorStatistiKtvtMegurimPUF]
                subd)]
     (conj k {:mean1 (mean
                      ($ :ucomp1 ($where {:KtvtLifney5ShanaMachozMchvPUF {:$lt 4}} subd)))
              :mean2 (mean
                      ($ :ucomp1 ($where {:KtvtLifney5ShanaMachozMchvPUF {:$gt 3 :$lt 98}} subd)))}))))

(defn mean-ucomp1s-by-coords [subd]
  (to-dataset
   (map (fn [row]
          (into row
                (coords-map (select-keys row
                                         [:SmlYishuvPUF
                                          :SmlEzorStatistiKtvtMegurimPUF]))))
        (:rows (mean-ucomp1s-by-stat-area subd)))))


(defn plot [subd]
  (let [chart (scatter-plot [] [])
        rows (vec (filter
                   (fn [row] (every? #(not (or (nil? %)
                                              (Double/isNaN %))) (vals row)))
                   (:rows
                    (mean-ucomp1s-by-coords subd))))
        uu (vec
            (uniformize (map -
                             (map :mean1 rows)
                             (map :mean2 rows))))] ;; NOTE THIS!
    (doseq [row rows]
      (add-points chart
                  [(:mean-x row)]
                  [(:mean-y row)]))
    (doseq [i (range (count rows))]
      (let [uui (double (uu i))]
        (.setSeriesPaint
         (.getRenderer (.getPlot chart) (inc i))
         0
         (java.awt.Color. (float uui)
                          (float (- 1 uui))
                          (float (- 1 uui))))))
    ;; (let [city ($where {:SmlYishuvPUF 5000} mean-ucomp1-by-coords)]
    ;;   (add-lines chart
    ;;              ($ :mean-x city)
    ;;              ($ :mean-y city))
    ;;   (.setSeriesPaint
    ;;    (.getRenderer (.getPlot chart) (inc (count rows)))
    ;;      0
    ;;      (java.awt.Color. 1 1 1)))
    chart))


;; (let [n 999
;;       chart (scatter-plot [] [])
;;       uu (vec
;;           (uniformize (range n)))]
;;   (doseq [i (range n)]
;;     (add-points chart
;;                 [i]
;;                 [i]))
;;   (doseq [i (range n)]
;;     (let [uui (double (uu i))]
;;       (println uui)
;;       (.setSeriesPaint
;;        (.getRenderer (.getPlot chart) i)
;;        0
;;        (java.awt.Color. (float uui)
;;                         (float (- 1 uui))
;;                         (float (- 1 uui))))))
;;   (view chart))



(def d-from-here ($where
                  {:KtvtLifney5ShanaMachozMchvPUF {:$lt 4}} d))

(def d-from-there ($where
                   {:KtvtLifney5ShanaMachozMchvPUF {:$gt 3 :$lt 98}} d))

(def d-new-apt ($where {:new-apt 1} d))

(def d-old-apt ($where {:new-apt 0} d))

(println (map dim [d
                   d-from-here d-from-there
                   d-new-apt d-old-apt]))

(sdisplay 1
          (s/horizontal-panel
           :items (map #(ChartPanel. (plot-uucomp1 %))
                       [;;d-new-apt d-old-apt
                        d-from-here d-from-there
                        ]))
          nil)
