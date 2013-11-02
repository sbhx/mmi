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

;;;;;;;;;;;;;;;;;;;;;

(time
 (def d
   (->> (read-cols-and-rows puf-filename
                            :seq-transformer #(sample % :size 10000))
        (transform-cols-and-rows (apply dissoc standard-column-fns
                                        col-names-to-avoid))
        (replace-columns-by-linear-combination comp1 :comp1)
        cols-and-rows-to-dataset
        filter-all-nonnil)))






