(comment
  (do
    (do
      (require 'mmi-proj.clean20131204 :reload-all)
      (in-ns 'mmi-proj.clean20131204))
    (defn reload []
      (require 'mmi-proj.clean20131204 :reload-all))))

(ns mmi-proj.clean20131204
  (:import java.lang.Math)
  (:import [java.net URL])
  (:import [javax.swing JComponent JLabel JPanel])
  (:import [org.jfree.chart ChartPanel JFreeChart])
  (:require [clj-time.core :as t])
  (:require clojure.core.matrix)
  (:require clojure.core.matrix.operators)
  (:require clojure-csv.core)
  (:require [clojure.data.csv])
  (:require clojure.inspector)
  (:require clojure.pprint)
  (:require clojure.reflect)
  (:require [clojure.string :as string])
  (:require nuroko.gui.visual)
  (:require quil.core quil.helpers.drawing quil.helpers.seqs)
  (:require [seesaw.core :as s])
  (:require [seesaw.font :as sf])
  (:require [clojure.data.json :as json])
  (:require [clojure.core.reducers :as r])
  (:use [clojure.algo.generic.functor :only [fmap]])
  (:use [clojure.java.shell :only [sh]])
  (:use clojure.pprint)
  (:use (incanter core stats charts io zoo som))
  (:use clj-utils.misc)
  (:use clj-utils.visual)
  (:use clj-utils.cols-and-rows)
  (:use clj-utils.cache)
  (:use clj-utils.io)
  (:use [c2.core :only (unify)])
  (:use hiccup.core)
  (:use clojure.stacktrace)
  (:use [clj-ml data clusterers])
  (:require [clatrix.core :as clx])
  (:require [clj-liblinear.core :as liblinear])
  (:require [clojure.data.generators :as gen]))

(apply require clojure.main/repl-requires)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def puf-filename
  "/home/we/workspace/PUF 2008/H20081171Data.csv")


(def map-from-yishuv-code-to-name
  (-> "/home/we/workspace/data/yishuv-name-code.csv"
      (read-dataset :header true)
      (transform-col :name put-Latin-around-Hebrew)
      :rows
      (#(mapcat vals %))
      (#(apply hash-map %))
      ))

(defn from-yishuv-code-to-name
  [yishuv-code]
  (if-let [yishuv-name (map-from-yishuv-code-to-name yishuv-code)]
    yishuv-name
    (str yishuv-code)))


(def get-cities
  (memoize 
   (fn [cityCode]
     (->> puf-filename
          read-cols-and-rows
          :rows
          (map :SmlYishuvPUF)
          distinct))))

(def get-stat-areas-of-city
  (memoize
   (fn [cityCode]
     (->> puf-filename
          read-cols-and-rows
          (filter-cols-and-rows #(= (str cityCode) (:SmlYishuvPUF %)))
          :rows
          (map :SmlEzorStatistiKtvtMegurimPUF)
          distinct
          (map (comp (nil-to-val :other)
                     parse-int-or-nil))))))


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
                           :TzuratAchzakatDira2008MchvPUF
                           :YabeshetMotzaByAvMchlkMchvPUF])


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


(def regular-int-or-nil (comp (threshold-to-nil 90)
                              parse-int-or-nil))


(def standard-column-fns
  (conj (identity-map [:EretzLeidaZkPUF
                       :YabeshetMotzaByAvMchlkMchvPUF
                       :RovaKtvtMegurimPUF
                       :TatRovaKtvtMegurimPUF
                       :SmlAnafKalkaliPUF ;; TODO: transform this
                       :SmlMishlachYadPUF ;; TODO: transform this
                       :TkufatSiyumBniyatDiraMchvPUF
                       ]
                      )
        {:Muslim (comp (specific-val-to-1-others-to-0 2)
                       parse-int-or-nil
                       :DatZkPUF)
         :Jewish (comp (specific-val-to-1-others-to-0 1)
                       parse-int-or-nil
                       :DatZkPUF)
         :Christian (comp (specific-val-to-1-others-to-0 3)
                       parse-int-or-nil
                       :DatZkPUF)
         :Druze (comp (specific-val-to-1-others-to-0 5)
                       parse-int-or-nil
                       :DatZkPUF)
         :aliyah (comp (specific-val-to-1-others-to-0 1)
                       parse-int-or-nil
                       :OleShnot90MchvPUF)
         :mizrach (comp (specific-vals-to-1-others-to-0 #{1 2})
                         parse-int-or-nil
                         :YabeshetMotzaByAvMchlkMchvPUF)
         :ashkenaz (comp (specific-vals-to-1-others-to-0 #{3 5})
                         parse-int-or-nil
                         :YabeshetMotzaByAvMchlkMchvPUF)
         :KtvtLifney5ShanaMachozMchvPUF (comp parse-int-or-nil :KtvtLifney5ShanaMachozMchvPUF)
         :KtvtLifney5ShanaMetropolinPUF (comp parse-int-or-nil :KtvtLifney5ShanaMetropolinPUF)
         :cityCode (comp parse-int-or-nil :SmlYishuvPUF)
         :statAreaCode (comp parse-int-or-nil :SmlEzorStatistiKtvtMegurimPUF)
         :Hchns2008BrutoSachirPUF-int (comp parse-int-or-nil :Hchns2008BrutoSachirPUF)
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
                         ;; :TzfifutDiurPUF-reg
                         ;; :NefashotMeshekBayitPUF-reg
                         :SmlAnafKalkaliPUF
                         :SmlMishlachYadPUF
                         ])


(defn compute-pca-components [_]
  (let [data-for-pca
        (->> (read-cols-and-rows puf-filename
                                 :seq-transformer #(sample % :size 10000))
             (transform-cols-and-rows
              (select-keys standard-column-fns pca-columns))
             cols-and-rows-to-dataset
             filter-all-nonnil)
        data-matrix (to-matrix data-for-pca)
        centered-matrix (apply bind-columns
                         (for [i (range (ncol data-matrix))]
                          (clx/-
                           ($ i data-matrix)
                           (mean (seq ($ i data-matrix))))))
        pca
        (principal-components centered-matrix)
        svd (clx/svd centered-matrix)
        svded-matrix (clx/* centered-matrix
                          (:right svd))
        components
        (vec (for [i (range (ncol data-for-pca))]
               (apply hash-map (interleave
                                (col-names data-for-pca)
                                ($ i (:rotation pca))))))
        data-for-pca-with-components
        (-> data-for-pca
            dataset-to-cols-and-rows
            ((apply comp
                    (for [i (range (count components))]
                      #(add-linear-combination-column (components i)
                                                      (keyword (str "comp" i))
                                                      %))))
            cols-and-rows-to-dataset)
        ]
    (with-data data-for-pca-with-components
      (correlation ($ :comp0)
                   ($ :comp1)))))

(def get-pca-components
  (memoize
   (fn []
     (compute-and-save-or-load
      compute-pca-components
      (fn [_] "/home/we/workspace/data/pca/components.clj")
      load-file
      pr-str-to-file
      nil
      :components))))


;;;;;;;;;;;;;;;;;;;;


(defn compute-coords []
  (->> (read-cols-and-rows "/home/we/workspace/data/salesDetails1.tsv"
                           :delimiter "\t")
       (transform-cols-and-rows {:x #(Double/parseDouble (:x %))
                                 :y #(Double/parseDouble (:y %))
                                 :cityCode (comp parse-int-or-nil
                                                 :cityCode)
                                 :statAreaCode (comp
                                                parse-int-or-nil
                                                ;; #(if (= % "null")
                                                ;;    nil
                                                ;;    %)
                                                :statAreaCode)})
       cols-and-rows-to-dataset
       ($group-by [:cityCode :statAreaCode])
       (map (fn [place-and-data]
              (conj (first place-and-data)
                    {:mean-x (mean ($ :x (second place-and-data)))
                     :mean-y (mean ($ :y (second place-and-data)))})))
       to-dataset
       filter-all-nonnil
       ($where {:mean-x {:$gt 0}
                :mean-y {:$gt 0}})))


(def get-coords
  ;; broken!!
  (memoize
   (fn []
     (compute-and-save-or-load
      (fn [_] (compute-coords))
      (fn [_] "/home/we/workspace/data/coords.csv")
      (fn [filename] (read-dataset filename
                                   :header true))
      (fn [filename object]
        (save object filename))
      nil
      :coords))))

(defn write-coords-as-json
  []
  (let [coords (get-coords)
        coords-with-desc (col-names
                          ($ [:mean-x :mean-y :statAreaCode :cityCode :desc]
                             (add-column
                              :desc
                              (map place-desc-of-row
                                   (:rows coords))
                              coords))
                          [:x :y :statAreaCode :cityCode :desc])]
    (spit "../client/interactive_map/coords.json"
          (json/write-str
           (:rows coords-with-desc)))))

(comment
  (sdisplay 1
            (ChartPanel.
             (scatter-plot :mean-x
                           :mean-y
                           :data (get-coords)
                           :group-by :cityCode))
            nil))

(defn get-coords-map []
  (apply conj
         (for [row (:rows (get-coords))]
           {{:cityCode (:cityCode row)
             :statAreaCode (:statAreaCode row)}
            (select-keys row [:mean-x
                              :mean-y])})))


(defn add-coords-to-place-dataset [place-dataset]
  (to-dataset
   (map (fn [row]
          (into row
                ((get-coords-map) (select-keys row
                                               [:cityCode
                                                :statAreaCode]))))
        (:rows place-dataset))))


(defn place-desc
  [cityCode statAreaCode]
  (str (clojure.string/replace
        (from-yishuv-code-to-name cityCode)
        #"o" "")
       "-"
       statAreaCode))

(defn place-desc-of-row
  [row]
  (place-desc (:cityCode row)
              (:statAreaCode row)))


(defn probability-to-color
  [prob]
  (let [prob256 (int (* 256 prob))]
    (format "#%06X"
            (+ (- 256 prob256)
               (* 256 256 prob256)))))


(defn compute-puf-with-components [components]
  (->> (read-cols-and-rows puf-filename)
       (transform-cols-and-rows (apply dissoc standard-column-fns
                                       col-names-to-avoid))
       ((apply comp
               (for [i (range (count components))]
                 #(add-linear-combination-column (components i)
                                                 (keyword (str "comp" i))
                                                 %))))
       (remove-columns (keys (components 0)))
       cols-and-rows-to-dataset
       ;;(add-coords-to-place-dataset)
       filter-all-nonnil))


(def get-puf-with-components
  (memoize
   (fn [components]
     (compute-and-save-or-load
      compute-puf-with-components
      (fn [_] (str "/home/we/workspace/data/puf-with-components-"
                   (hash _)
                   ".csv"))
      (fn [filename] (read-dataset filename
                                   :header true))
      (fn [filename object]
        (save object filename))
      components
      :puf-with-components))))

(comment
  (time (dim (get-puf-with-components (get-pca-components)))))


(defn compute-measures-by-place
  [input]
  (to-dataset
   (for [[place-map subset] ($group-by
                             [:cityCode :statAreaCode]
                             ((:dataset-with-places-fn input)))]
     (let [cond-rows-map
           (fmap
            (fn [row-condition]
              (filter row-condition
                      (:rows subset)))
            (:condition-map input))]
       (into place-map
             (for [[cond-name cond-rows] cond-rows-map
                     col-name (:columns-to-summarize input)
                   [summarizing-fn-name summarizing-fn] (:summarizing-fns-map input)]
               [(concat-keywords col-name
                                 summarizing-fn-name
                                 :given
                                 cond-name)
                (summarizing-fn
                 (map col-name cond-rows))]
               ))))))

(def get-measures-by-place
  (memoize
   (fn [input]
     (compute-and-save-or-load
      compute-measures-by-place
      (fn [input] (str "/home/we/workspace/data/measures-by-place-"
                       (hash [(keys (:condition-map input))
                              (:columns-to-summarize input)
                              (keys (:summarizing-fns-map input))])
                       ".csv"))
      (fn [filename] (read-dataset filename
                                   :header true))
      (fn [filename object]
        (save object filename))
      input
      :measures-by-place))))


(def get-standard-measures-by-place
  (memoize
   (fn []
     (let [input {:dataset-with-places-fn (fn []
                                            (get-puf-with-components (get-pca-components)))
                  :condition-map {:all (fn [row] true)
                                  :stayed (fn [row]
                                            (= 1 (:KtvtLifney5ShanaMachozMchvPUF row)))
                                  :moved (fn [row]
                                           (not= 1 (:KtvtLifney5ShanaMachozMchvPUF row)))}
                  :columns-to-summarize [:comp0 :comp1 :comp2 :comp3
                                         :new-apt
                                         :Muslim :Jewish :Christian :Druze
                                         :aliyah
                                         :mizrach
                                         :ashkenaz
                                         :TzfifutDiurPUF-reg
                                         :NefashotMeshekBayitPUF-reg
                                         ]
                  :summarizing-fns-map {:count count
                                        :mean (careful-mean 15)}}]
       (get-measures-by-place input)))))


(def get-standard-measures-by-coords
  (memoize
   (fn []
     (add-coords-to-place-dataset
      (get-standard-measures-by-place)))))

(comment
  (time (dim (get-standard-measures-by-coords))))


(defn compute-sales-summary-by-place-map
  [years]
  (let [;;;;
        summary-by-place-by-year
        (->> (read-cols-and-rows "/home/we/workspace/data/salesDetails1.tsv"
                                 :delimiter "\t")
             (transform-cols-and-rows {:cityCode (comp parse-int-or-nil
                                                       :cityCode)
                                       :statAreaCode (comp parse-int-or-nil
                                                           ;; nil happens,
                                                           ;; for example,
                                                           ;; when "null"
                                                           ;; appears in data.
                                                           :statAreaCode)
                                       :pricePerMeter (comp parse-int-or-nil
                                                            :priceByMeter)
                                       :year #(parse-int-or-nil
                                               (first (clojure.string/split (:saleDay %) #"-")))})
             (filter-cols-and-rows #((into #{} years)
                                     (:year %)))
             cols-and-rows-to-dataset
             ($group-by [:year])
             (map (fn [year-and-data]
                    [(first year-and-data)
                     (->> year-and-data
                          second
                          ($group-by [:cityCode :statAreaCode])
                          (map (fn [place-and-data]
                                 [(first place-and-data)
                                  {:medppm (median
                                            (to-seq ($ :pricePerMeter
                                                       (second place-and-data))))
                                   :n (nrow (second place-and-data))}]))
                          (apply concat)
                          (apply hash-map))]))
             (apply concat)
             (apply hash-map))
          ;;;;
        unifprice-by-place-by-year
        (fmap (fn [summary-by-place]
                (apply hash-map
                       (apply concat
                              (map vector
                                   (keys summary-by-place)
                                   (map double (uniformize (map :medppm
                                                                (vals summary-by-place))))))))
              summary-by-place-by-year)
          ;;;;
        places
        (distinct (apply concat
                         (map (fn [year-and-unifprice-by-place]
                                (map first (second year-and-unifprice-by-place)))
                              unifprice-by-place-by-year)))
          ;;;;
        summary-by-place
        (apply hash-map
               (apply concat
                      (for [place places]
                        [place (into {}
                                     (for [year years]
                                       {(keyword (str "unifprice" year))
                                        (get-in unifprice-by-place-by-year
                                                [{:year year} place])
                                        (keyword (str "n" year))
                                        (get-in summary-by-place-by-year
                                                [{:year year} place :n])}))])))
        ]
    summary-by-place))


(defn compute-sales-summary-by-place [years]
  (to-dataset (map #(apply conj %)
                   (compute-sales-summary-by-place-map years))))


(def get-sales-summary-by-place
  (memoize
   (fn [years]
     (compute-and-save-or-load
      compute-sales-summary-by-place
      (fn [_] (str
              (concat-with-delimiter
               "-"
               (cons
                "/home/we/workspace/data/sales-summary-by-place"
                years))
              ".csv"))
      (fn [filename] (read-dataset filename
                                  :header true))
      (fn [filename object]
        (save object filename))
      years
      :sales-summary-by-place))))


(defn compute-sales-summary-by-coords [years]
  (add-coords-to-place-dataset
   (compute-sales-summary-by-place years)))


(def get-sales-summary-by-coords
  (memoize
   (fn [years]
     (compute-and-save-or-load
      compute-sales-summary-by-coords
      (fn [_] (str
              (concat-with-delimiter
               "-"
               (cons
                "/home/we/workspace/data/sales-summary-by-coords"
                years))
              ".csv"))
      (fn [filename] (read-dataset filename
                                  :header true))
      (fn [filename object]
        (save object filename))
      years
      :sales-summary-by-coords))))


(defn compute-join-by-coords []
  (let [measures-by-place (get-standard-measures-by-place)
        summary-by-place (get-sales-summary-by-place (range 2006 2011))
        join-by-place ($join [[:cityCode :statAreaCode]
                              [:cityCode :statAreaCode]]
                             summary-by-place
                             measures-by-place)
        join-by-coords (sort-colnames
                        (add-coords-to-place-dataset join-by-place))]
    join-by-coords))


(def get-join-by-coords
  (memoize
   (fn []
     (compute-and-save-or-load
      (fn [_] (compute-join-by-coords))
      (fn [_] "/home/we/workspace/data/join-by-coords.csv")
      (fn [filename] (read-dataset filename
                                  :header true))
      (fn [filename object]
        (save object filename))
      nil
      :join-by-coords))))

(comment
  (time (dim (get-join-by-coords))))


(defn add-changes-to-join-by-coords []
  (let [join-by-coords (filter-complete-rows
                        (filter-all-nonnil-and-nonNaN
                         (get-join-by-coords)))]
    (apply conj-cols
           (cons join-by-coords
                 (concat
                  (for [[from-year to-year] [[2006 2007] [2007 2008] [2008 2009] [2009 2010]
                                             [2006 2010]]]
                    (dataset
                     [(keyword (concat-with-delimiter
                                "-"
                                ["logit-unifprice-change"
                                 to-year
                                 from-year]))]
                     (map logits-difference
                          ($ (keyword (str "unifprice" to-year)) join-by-coords)
                          ($ (keyword (str "unifprice" from-year)) join-by-coords))))
                  (for [varname [:comp0 :comp1 :comp2 :comp3
                                 :NefashotMeshekBayitPUF-reg
                                 :TzfifutDiurPUF-reg
                                 :new-apt
                                 :Muslim :Christian :Jewish
                                 :ashkenaz :mizrach :aliyah]]
                    (dataset
                     [(keyword (str
                                (name varname) "-change"))]
                     (map -
                          ($ (keyword (str (name varname) "-mean-given-all")) join-by-coords)
                          ($ (keyword (str (name varname) "-mean-given-stayed")) join-by-coords)))))))))


(comment
  (let [data (->> (add-changes-to-join-by-coords)
                  ($ [:cityCode :statAreaCode
                      :NefashotMeshekBayitPUF-reg-change
                      ;;:TzfifutDiurPUF-reg-change
                      :new-apt-change
                      ;; :Muslim-change :Christian-change 
                      ;;:Jewish-change
                      :ashkenaz-change
                      ;;:mizrach-change
                      ;; :aliyah-change
                      :comp0-mean-given-all
                      :comp1-mean-given-all
                      :comp0-change
                      :comp1-change
                      :mean-x
                      :mean-y
                      ]))
        data (add-column
              :yishuv-name (map (comp from-yishuv-code-to-name
                                      (nil-to-val "other")
                                      (leave-only-nil-and-values-of-set #{3000 4000 5000 7000 70 6100 1031 2800}))
                                ($ :cityCode data))
              (add-column :desc (map place-desc-of-row
                                     (:rows data))
                          data))
        data ($ (filter (complement #{:cityCode :statAreaCode})
                        (col-names data))
                data)
        ;; data (->> data
        ;;           :rows
        ;;           (map (fn [row]
        ;;                  (into {}
        ;;                        (for [[k v] row]
        ;;                          (if (number? v)
        ;;                            {k (abs v)}
        ;;                            {k v})))))
        ;;           to-dataset)
        ]
    (save data "../client/my-scatter/scatter.csv")
    (->> data
         :rows
         (map #(fmap signum %))
         freqs-as-rows
         (map #(into (:val %) {:-count (:count %)}))
         (map #(into (sorted-map) %))
         (filter #(<= 10 (:-count %)))
         print-table
         ))
  (sdisplay 1
            (s/vertical-panel
             :items
             (for [varname [:NefashotMeshekBayitPUF-reg-change
                            :TzfifutDiurPUF-reg-change
                            :new-apt-change
                            :Muslim-change :Christian-change :Jewish-change
                            :ashkenaz-change :mizrach-change :aliyah-change]]
               (org.jfree.chart.ChartPanel.
                (histogram (filter (complement zero?)
                                   ($ varname
                                      (add-changes-to-join-by-coords)))
                           :title (name varname)
                           :nbins 100))))
            nil))




(defn compute-sample
  [input]
  (let [n-elements-to-sample (sum (vals (:group-size-by-name input)))
        _ (assert (<= n-elements-to-sample
                      (count (:coll input)))
                  "enough elements to sample")
        samples (vec
                 (binding [gen/*rnd* (java.util.Random. ((nil-to-val 1)
                                                         (:seed input)))]
                   (take n-elements-to-sample ; TODO: Is there a more
                                        ; efficient way?
                         (gen/shuffle (:coll input)))))
        sizes-cumsum (cumulative-sum (vals (:group-size-by-name input)))
        samples-by-name (zipmap
                         (keys (:group-size-by-name input))
                         (map (comp vec
                                    #(map samples %)
                                    range)
                              (cons 0 sizes-cumsum)
                              sizes-cumsum))]
    samples-by-name))


(comment
  (compute-sample
   {:group-size-by-name {:a 5 :b 3}
    :coll (range 99)
    :seed 1}))


(defn train-liblinear-model
  [input]
  (let [adataset ((:dataset-fn input))
        samples-by-name (compute-sample {:coll (range (nrow adataset))
                                         :group-size-by-name {:training (:training-size input)
                                                              :evaluation (- (nrow adataset)
                                                                             (:training-size input))}})]
    (println {:sample-sizes
              (fmap count samples-by-name)})
    (liblinear/train (map
                      #(assoc % :one 1)
                      (:rows
                       ($ (:training samples-by-name) (:feature-columns input) adataset)))
                     ($ (:training samples-by-name) (:response-column input) adataset)
                     :algorithm (:liblinear-algorithm input)
                     :c (:liblinear-c input))))


(defn get-liblinear-model-coeff-map [model]
  (let [w (-> model :liblinear-model (#(.getFeatureWeights %)) vec)]
    (apply hash-map
           (apply concat
                  (filter
                   (comp not zero? second)
                   (fmap (comp w dec)
                         (:dimensions model)))))))

;; (defn get-liblinear-model-labels [model]
;;   (vec (.getLabels (:liblinear-model model))))


(defn predict-by-linear-model
  [input]
  (let [adataset ((:dataset-fn input))
        samples-by-name (compute-sample {:coll (range (nrow adataset))
                                         :group-size-by-name {:training (:training-size input)
                                                              :evaluation (- (nrow adataset)
                                                                             (:training-size input))}})
        model ((:model-fn input))
        coeff-map (get-liblinear-model-coeff-map model)]
    (fmap
     (fn [set-name]
       (to-dataset {:prediction (map
                                 (fn [row]
                                   (sum (for [[col-name coeff] coeff-map]
                                          (* coeff (col-name row)))))
                                 (map #(assoc % :one 1)
                                      (:rows
                                       ($ (set-name samples-by-name) (:feature-columns input) adataset))))
                    :response ($ (set-name samples-by-name) (:response-column input) adataset) }))
     (identity-map (keys samples-by-name)))))



(comment
  (let [input {:dataset-fn (comp filter-all-nonnil-and-nonNaN
                                 get-join-by-coords)
               :training-size 200
               :feature-columns [:comp0-mean-given-stayed
                                 :comp1-mean-given-stayed
                                 :comp2-mean-given-stayed
                                 :comp3-mean-given-stayed]
               :response-column :unifprice2006
               :liblinear-algorithm :l2l2
               :liblinear-c 100000}
        model (train-liblinear-model input)
        _ (println (get-liblinear-model-coeff-map model))
        input (into input
                    {:model-fn (fn [] model)})
        predictions (predict-by-linear-model input)]
    (show-charts 1
                 (for [[set-name prediction-vs-response] predictions]
                   (scatter-plot :prediction :response
                                 :data prediction-vs-response
                                 :title (str (name set-name)
                                             ":"
                                             (format "%.3f"
                                                     (correlation
                                                      ($ :prediction prediction-vs-response)
                                                      ($ :response prediction-vs-response)))))))))


(comment
  (let [input {:dataset-fn (comp filter-all-nonnil-and-nonNaN
                                 add-changes-to-join-by-coords)
               :training-size 800
               :feature-columns [  ;; :comp0-change :comp1-change :comp2-change :comp3-change
                                 :NefashotMeshekBayitPUF-reg-change
                                 :TzfifutDiurPUF-reg-change
                                 :new-apt-change
                                 :Muslim-change :Christian-change :Jewish-change
                                 :ashkenaz-change :mizrach-change :aliyah-change
                                 ]
               :response-column :logit-unifprice-change-2010-2006
               :liblinear-algorithm :l1l2_primal
               :liblinear-c 1}
        model (train-liblinear-model input)
        _ (println (get-liblinear-model-coeff-map model))
        input (into input
                    {:model-fn (fn [] model)})
        predictions (predict-by-linear-model input)]
    (show-charts 1
                 (for [[set-name prediction-vs-response] predictions]
                   (scatter-plot :prediction :response
                                 :data prediction-vs-response
                                 :title (str (name set-name)
                                             ":"
                                             (format "%.3f"
                                                     (correlation
                                                      ($ :prediction prediction-vs-response)
                                                      ($ :response prediction-vs-response)))))))))



(defn incanter-dataset-to-weka-dataset
  [name incanter-dataset]
  (make-dataset name
                (col-names incanter-dataset)
                (map vals (:rows incanter-dataset))))


(defn weka-dataset-to-incanter-dataset
  [weka-dataset]
  (dataset
   (attribute-names weka-dataset)
   (map instance-to-map weka-dataset)))



(defn general-gen-map-data
  [data colname transf color-func]
  (let [rows (vec (filter
                   (fn [row] (every? #(not (or (nil? %)
                                              (Double/isNaN %))) (vals row)))
                   (:rows data)))
        values (vec
                (map transf
                     (map colname rows)))
        markers (for [i (range (count rows))]
                  (let [vali (values i)
                        row (rows i)]
                    {:lon (:mean-x row)
                     :lat (:mean-y row)
                     :label (place-desc-of-row row)
                     :color (color-func vali)}))
        center {:lat (mean (filter identity (map :lat markers)))
                :lon (mean (filter identity (map :lon markers)))}
        zoom 11]
    {:center center
     :zoom zoom
     :markers markers}))



(comment
  (let [data (filter-all-nonnil-and-nonNaN
              (add-changes-to-join-by-coords))
        clusterer (make-clusterer :k-means
                                  :number-clusters 3
                                  :number-iterations 10000)
        weka-dataset (incanter-dataset-to-weka-dataset :data
                                                       ($ [:NefashotMeshekBayitPUF-reg-change
                                                           :TzfifutDiurPUF-reg-change
                                                           :new-apt-change
                                                           :Muslim-change :Christian-change :Jewish-change
                                                           :ashkenaz-change :mizrach-change :aliyah-change]
                                                          data))
        _ (clusterer-build clusterer
                           weka-dataset)
        labels (map (comp parse-int-or-nil name :class instance-to-map)
                    (clusterer-cluster clusterer
                                       weka-dataset))
        labelled-data (conj-cols data {:label labels})]
    (spit "../client/data2.json"
          (json/write-str
           (general-gen-map-data labelled-data
                                 :label
                                 int
                                 ["red" "green" "blue" "cyan" "magenta" "black" "white"])))))



















;; (def get-liblinear-model
;;   (memoize
;;    (fn [input]
;;      (compute-and-save-or-load
;;       train-liblinear-model
;;       (fn [_] (str
;;               (concat-with-delimiter
;;                "-"
;;                (concat
;;                 ["/home/we/workspace/data/liblinear-model"]
;;                 [(:response-column input)]
;;                 (:feature-columns input)
;;                 (apply concat (:liblinear-params input))))
;;               ".csv")))
;;      (fn [filename] (liblinear/load-model filename))
;;       (fn [filename object]
;;         (liblinear/save-model object filename))
;;       input
;;       :liblinear-model)))


;; (comment
;;   (let [data (add-coords-to-place-dataset
;;               (get-sales-summary-by-place (range 2006 2011)))]
;;     (spit "../client/data1.json"
;;           (json/write-str
;;            (general-gen-map-data data :unifprice2006 identity probability-to-color)))
;;     (spit "../client/data2.json"
;;           (json/write-str
;;            (general-gen-map-data data :unifprice2010 identity probability-to-color)))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;

;; (defn sets-to-labels
;;   [sets]
;;   (let [idx (range (apply max
;;                           (map #(apply max %)
;;                                sets)))
;;         sets-vec (vec sets)]
;;     (map second
;;          (sort-by first
;;                   (apply concat
;;                          (for [i (range (count sets-vec))]
;;                            (let [aset (sets-vec i)]
;;                              (for [j aset]
;;                                [(int j) (int i)]))))))))

;; (comment
;;   (let [data (filter-all-nonnil-and-nonNaN
;;               ($ [:unifprice2006 :unifprice2007 :unifprice2008
;;                   :unifprice2009 :unifprice2010
;;                   :cityCode :statAreaCode]
;;                  (get-join-by-coords)))
;;         clusters (som-batch-train
;;                   (to-matrix
;;                    ($ [:unifprice2006 :unifprice2007 :unifprice2008
;;                        :unifprice2009 :unifprice2010]
;;                       data)))
;;         num-clusters (count (:sets clusters))
;;         labels (sets-to-labels
;;                 (vals (:sets clusters)))
;;         labelled-data (add-coords-to-place-dataset
;;                        (conj-cols data {:label labels}))]
;;     (spit "../client/data2.json"
;;           (json/write-str
;;            (general-gen-map-data labelled-data
;;                                  :label
;;                                  int
;;                                  ["red" "green" "blue" "cyan" "magenta" "black" "white"])))))








;; (comment
;;   (let [pre-data (filter-all-nonnil-and-nonNaN
;;                   ($ [
;;                       :prob-a-om :prob-a-os :prob-o-om :prob-o-os
;;                       :mean-ucomp1-om :mean-ucomp1-os
;;                       :unifprice2006 :unifprice2007 :unifprice2008 :unifprice2009 :unifprice2010
;;                       :n2006 :n2007 :n2008 :n2009 :n2010
;;                       :cityCode :statAreaCode
;;                       :mean-x :mean-y]
;;                      (get-join-by-coords)))
;;         data (filter-all-nonnil-and-nonNaN
;;               ($ [:ucomp1-change
;;                   :prob-a-change
;;                   :prob-o-change
;;                   :ucomp1-change
;;                   :unifprice-change-2007-2006
;;                   :unifprice-change-2008-2007
;;                   :unifprice-change-2009-2008
;;                   :unifprice-change-2010-2009
;;                   :n-change-2007-2006
;;                   :n-change-2008-2007
;;                   :n-change-2009-2008
;;                   :n-change-2010-2009
;;                   :cityCode :statAreaCode
;;                   :mean-x :mean-y]
;;                  (conj-cols (to-dataset
;;                              {:prob-a-change (map logits-difference
;;                                                   ($ :prob-a-om pre-data)
;;                                                   ($ :prob-a-os pre-data))
;;                               :prob-o-change (map logits-difference
;;                                                   ($ :prob-o-om pre-data)
;;                                                   ($ :prob-o-os pre-data))
;;                               :ucomp1-change (map logits-difference
;;                                                   ($ :mean-ucomp1-om pre-data)
;;                                                   ($ :mean-ucomp1-os pre-data))
;;                               :unifprice-change-2007-2006 (map logits-difference
;;                                                                ($ :unifprice2007 pre-data)
;;                                                                ($ :unifprice2006 pre-data))
;;                               :unifprice-change-2008-2007 (map logits-difference
;;                                                                ($ :unifprice2008 pre-data)
;;                                                                ($ :unifprice2007 pre-data))
;;                               :unifprice-change-2009-2008 (map logits-difference
;;                                                                ($ :unifprice2009 pre-data)
;;                                                                ($ :unifprice2008 pre-data))
;;                               :unifprice-change-2010-2009 (map logits-difference
;;                                                                ($ :unifprice2010 pre-data)
;;                                                                ($ :unifprice2009 pre-data))
;;                               :n-change-2007-2006 (map log-ratio
;;                                                        ($ :n2007 pre-data)
;;                                                        ($ :n2006 pre-data))
;;                               :n-change-2008-2007 (map log-ratio
;;                                                        ($ :n2008 pre-data)
;;                                                        ($ :n2007 pre-data))
;;                               :n-change-2009-2008 (map log-ratio
;;                                                        ($ :n2009 pre-data)
;;                                                        ($ :n2008 pre-data))
;;                               :n-change-2010-2009 (map log-ratio
;;                                                        ($ :n2010 pre-data)
;;                                                        ($ :n2009 pre-data))})
;;                             pre-data)))
;;         clusters (som-batch-train
;;                   (to-matrix
;;                    ($ [:ucomp1-change
;;                        :prob-a-change
;;                        :prob-o-change
;;                        :unifprice-change-2007-2006
;;                        :unifprice-change-2008-2007
;;                        :unifprice-change-2009-2008
;;                        :unifprice-change-2010-2009
;;                        ;; :n-change-2007-2006
;;                        ;; :n-change-2008-2007
;;                        ;; :n-change-2009-2008
;;                        ;; :n-change-2010-2009
;;                        ]
;;                       data)))
;;         num-clusters (count (:sets clusters))
;;         labels (sets-to-labels
;;                 (vals (:sets clusters)))
;;         labelled-data (conj-cols data {:label labels})]
;;     (spit "../client/data2.json"
;;           (json/write-str
;;            (general-gen-map-data labelled-data
;;                                  :label
;;                                  int
;;                                  ["red" "green" "blue" "cyan" "magenta" "black" "white"])))))




;; (comment
;;   (let [pre-data (filter-all-nonnil-and-nonNaN
;;                   ($ [
;;                       :prob-a-om :prob-a-os :prob-o-om :prob-o-os
;;                       :mean-ucomp1-om :mean-ucomp1-os
;;                       :unifprice2006 :unifprice2007 :unifprice2008 :unifprice2009 :unifprice2010
;;                       :n2006 :n2007 :n2008 :n2009 :n2010
;;                       :cityCode :statAreaCode
;;                       :mean-x :mean-y]
;;                      (get-join-by-coords)))
;;         data (filter-all-nonnil-and-nonNaN
;;               ($ [:ucomp1-change
;;                   :prob-a-change
;;                   :prob-o-change
;;                   :ucomp1-change
;;                   :unifprice-change-2007-2006
;;                   :unifprice-change-2008-2007
;;                   :unifprice-change-2009-2008
;;                   :unifprice-change-2010-2009
;;                   :n-change-2007-2006
;;                   :n-change-2008-2007
;;                   :n-change-2009-2008
;;                   :n-change-2010-2009
;;                   :cityCode :statAreaCode
;;                   :mean-x :mean-y]
;;                  (conj-cols
;;                   (to-dataset
;;                    {:prob-a-change (map logits-difference
;;                                         ($ :prob-a-om pre-data)
;;                                         ($ :prob-a-os pre-data))
;;                     :prob-o-change (map logits-difference
;;                                         ($ :prob-o-om pre-data)
;;                                         ($ :prob-o-os pre-data))
;;                     :ucomp1-change (map logits-difference
;;                                         ($ :mean-ucomp1-om pre-data)
;;                                         ($ :mean-ucomp1-os pre-data))
;;                     :unifprice-change-2007-2006 (map logits-difference
;;                                                      ($ :unifprice2007 pre-data)
;;                                                      ($ :unifprice2006 pre-data))
;;                     :unifprice-change-2008-2007 (map logits-difference
;;                                                      ($ :unifprice2008 pre-data)
;;                                                      ($ :unifprice2007 pre-data))
;;                     :unifprice-change-2009-2008 (map logits-difference
;;                                                      ($ :unifprice2009 pre-data)
;;                                                      ($ :unifprice2008 pre-data))
;;                     :unifprice-change-2010-2009 (map logits-difference
;;                                                      ($ :unifprice2010 pre-data)
;;                                                      ($ :unifprice2009 pre-data))
;;                     :n-change-2007-2006 (map log-ratio
;;                                              ($ :n2007 pre-data)
;;                                              ($ :n2006 pre-data))
;;                     :n-change-2008-2007 (map log-ratio
;;                                              ($ :n2008 pre-data)
;;                                              ($ :n2007 pre-data))
;;                     :n-change-2009-2008 (map log-ratio
;;                                              ($ :n2009 pre-data)
;;                                              ($ :n2008 pre-data))
;;                     :n-change-2010-2009 (map log-ratio
;;                                              ($ :n2010 pre-data)
;;                                              ($ :n2009 pre-data))})
;;                   pre-data)))
;;         clusterer (make-clusterer :k-means
;;                                   :number-clusters 3
;;                                   :number-iterations 10000)
;;         weka-dataset (incanter-dataset-to-weka-dataset :data
;;                                                        ($ [:ucomp1-change
;;                                                            ;; :prob-a-change
;;                                                            ;; :prob-o-change
;;                                                            :unifprice-change-2007-2006
;;                                                            :unifprice-change-2008-2007
;;                                                            :unifprice-change-2009-2008
;;                                                            :unifprice-change-2010-2009
;;                                                            ;; :n-change-2007-2006
;;                                                            ;; :n-change-2008-2007
;;                                                            ;; :n-change-2009-2008
;;                                                            ;; :n-change-2010-2009
;;                                                            ]
;;                                                           data))
;;         _ (clusterer-build clusterer
;;                            weka-dataset)
;;         labels (map (comp parse-int-or-nil name :class instance-to-map)
;;                     (clusterer-cluster clusterer
;;                                        weka-dataset))
;;         labelled-data (conj-cols data {:label labels})]
;;     (spit "../client/data2.json"
;;           (json/write-str
;;            (general-gen-map-data labelled-data
;;                                  :label
;;                                  int
;;                                  ["red" "green" "blue" "cyan" "magenta" "black" "white"])))))


;; (defn random-color
;;   []
;;   (format "#%06X"
;;           (rand-int 16777216)))



;; (comment
;;   (let [pre-data (filter-all-nonnil-and-nonNaN
;;                   ($ [
;;                       :prob-a :prob-a-om :prob-a-os
;;                       :prob-o :prob-o-om :prob-o-os
;;                       :mean-ucomp1-om :mean-ucomp1-os
;;                       :unifprice2006 :unifprice2007 :unifprice2008 :unifprice2009 :unifprice2010
;;                       :n2006 :n2007 :n2008 :n2009 :n2010
;;                       :cityCode :statAreaCode
;;                       :mean-x :mean-y]
;;                      (get-join-by-coords)))
;;         data pre-data
;;         cluster-to-json
;;         (fn [colnames filename]
;;           (let [
;;                 clusterer (make-clusterer :expectation-maximization
;;                                           :number-clusters 10)
;;                 weka-dataset (incanter-dataset-to-weka-dataset
;;                               :data ($ colnames data))
;;                 _ (clusterer-build clusterer
;;                                    weka-dataset)
;;                 labels (map (comp parse-int-or-nil name :class instance-to-map)
;;                             (clusterer-cluster clusterer
;;                                                weka-dataset))
;;                 _ (println (frequencies labels))
;;                 labelled-data (conj-cols data {:label labels})
;;                 num-classes (count (distinct labels))]
;;             (spit filename
;;                   (json/write-str
;;                    (general-gen-map-data labelled-data
;;                                          :label
;;                                          int
;;                                          (vec (repeatedly num-classes random-color))
;;                                          )))))]
;;     (cluster-to-json [:prob-a-os :prob-o-os :mean-ucomp1-os]
;;                      "../client/data1.json")
;;     (cluster-to-json [:prob-a :prob-o :mean-ucomp1]
;;                      "../client/data2.json")))




;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; ;; (comment
;; ;;   (let [pre-d
;; ;;         (->> (read-cols-and-rows puf-filename)
;; ;;              (transform-cols-and-rows (apply dissoc standard-column-fns
;; ;;                                              col-names-to-avoid))
;; ;;              (add-coords-to-place-dataset)
;; ;;              (add-linear-combination-column (extended-components 0) :xcomp0)
;; ;;              (add-linear-combination-column (extended-components 1) :xcomp1)
;; ;;              (add-linear-combination-column (extended-components 2) :xcomp2)
;; ;;              (add-linear-combination-column (extended-components 3) :xcomp3)
;; ;;              (remove-columns (keys (extended-components 0)))
;; ;;              cols-and-rows-to-dataset
;; ;;              filter-all-nonnil)
;; ;;         pre-d-1 (conj-cols pre-d
;; ;;                            (dataset [:mean-x :mean-y]
;; ;;                                     (map
;; ;;                                         ; change nils with {}s
;; ;;                                      #(if % % {})
;; ;;                                      (map coords-map
;; ;;                                           (:rows
;; ;;                                            ($ [:cityCode
;; ;;                                                :statAreaCode]
;; ;;                                               pre-d))))))
;; ;;         pre-d-2 (conj-cols pre-d-1
;; ;;                            {:axcomp0 (map double (adapt-range ($ :xcomp0 pre-d-1)))
;; ;;                             :axcomp1 (map double (adapt-range ($ :xcomp1 pre-d-1)))
;; ;;                             :axcomp2 (map double (adapt-range ($ :xcomp2 pre-d-1)))
;; ;;                             :axcomp3 (map double (adapt-range ($ :xcomp3 pre-d-1)))
;; ;;                             :uxcomp0 (map double (uniformize ($ :xcomp0 pre-d-1)))
;; ;;                             :uxcomp1 (map double (uniformize ($ :xcomp1 pre-d-1)))
;; ;;                             :uxcomp2 (map double (uniformize ($ :xcomp2 pre-d-1)))
;; ;;                             :uxcomp3 (map double (uniformize ($ :xcomp3 pre-d-1)))})
;; ;;         measures-by-place (get-xmeasures-by-place pre-d-2)
;; ;;         measures-by-coords (filter-all-nonnil-and-nonNaN
;; ;;                             (add-coords-to-place-dataset
;; ;;                              measures-by-place))
;; ;;         measures-by-coords-with-info (add-column
;; ;;                                       :yishuv-name (map (comp from-yishuv-code-to-name
;; ;;                                                               (nil-to-val "other")
;; ;;                                                               (leave-only-nil-and-values-of-set #{3000 4000 5000 7000 70 6100 1031 2800}))
;; ;;                                                         ($ :cityCode measures-by-coords))
;; ;;                                       (add-column :desc (map place-desc-of-row
;; ;;                                                              (:rows measures-by-coords))
;; ;;                                                   measures-by-coords))
;; ;;         x-axis #(* 1000 %)
;; ;;         y-axis #(- 1000 (* 1000 %))
;; ;;         relevant-rows
;; ;;         (:rows measures-by-coords-with-info)
;; ;;         ;; (filter #(#{5000} (:cityCode %))
;; ;;         ;;         (:rows measures-by-coords-with-info))
;; ;;         ]
;; ;;     (plot-by-d3
;; ;;      {"h" 2000
;; ;;       "w" 1000
;; ;;       ;; "lines" [;; {"x" (x-axis 0) "y" (y-axis 0)}
;; ;;       ;;          ;; {"x" (x-axis 1) "y" (y-axis 0)}
;; ;;       ;;          ;; {"x" (x-axis 1) "y" (y-axis 1)}
;; ;;       ;;          ;; {"x" (x-axis 0) "y" (y-axis 1)}
;; ;;       ;;          ;; {"x" (x-axis 0) "y" (y-axis 0)}
;; ;;       ;;          ;; {"x" (x-axis 1) "y" (y-axis 1)}
;; ;;       ;;          ]
;; ;;       "lines"
;; ;;       (map
;; ;;        (fn [row]
;; ;;          {"x1" (x-axis (- 1 (:mean-axcomp0-s row)))
;; ;;           "y1" (x-axis (:mean-axcomp2-s row))
;; ;;           "x2" (x-axis (- 1 (:mean-axcomp0-m row)))
;; ;;           "y2" (x-axis (:mean-axcomp2-m row))})
;; ;;        relevant-rows)
;; ;;       "circles"
;; ;;       (mapcat
;; ;;        (fn [row]
;; ;;          (let [color (case (:cityCode row)
;; ;;                        5000 "white"
;; ;;                        4000 "red"
;; ;;                        3000 "yellow"
;; ;;                        7600 "magenta"
;; ;;                        70 "blue"
;; ;;                        "#666666")
;; ;;                attr-and-style { ;; "r" 5
;; ;;                                ;; (/ (sqrt (:n row))
;; ;;                                ;;     2)
;; ;;                                "fill" color
;; ;;                                "stroke" color
;; ;;                                "opacity" 0.5}]
;; ;;            [(into attr-and-style {"r" 1
;; ;;                                   "cx" (x-axis (- 1 (:mean-axcomp0-m row)))
;; ;;                                   "cy" (x-axis (:mean-axcomp2-m row))
;; ;;                                   "text" (str (:desc row) "-M")})
;; ;;             (into attr-and-style {"r" 3
;; ;;                                   "cx" (x-axis (- 1 (:mean-axcomp0-s row)))
;; ;;                                   "cy" (x-axis (:mean-axcomp2-s row))
;; ;;                                   "text" (str (:desc row) "-S")})]))
;; ;;        relevant-rows)})
;; ;;     (save
;; ;;      ($ [:desc
;; ;;          :mean-axcomp0-m
;; ;;          :mean-axcomp0-s
;; ;;          :mean-axcomp1-m
;; ;;          :mean-axcomp1-s
;; ;;          :mean-axcomp2-m
;; ;;          :mean-axcomp2-s
;; ;;          :mean-axcomp3-m
;; ;;          :mean-axcomp3-s
;; ;;          ;; :mean-x
;; ;;          ;; :mean-y
;; ;;          ;; :n
;; ;;          :yishuv-name]
;; ;;         measures-by-coords-with-info)
;; ;;      "../client/my-scatter/scatter.csv")))



;; ;;;;;;;;;;;;;;;;

;; ;; - arrows
;; ;; - stability of pca wrt randomization

(defn plot-by-d3 [d3data]
  (spit "../client/d3data.json"
        (json/write-str d3data)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(comment
  (->> puf-filename
       read-cols-and-rows
       (filter-cols-and-rows #(= "5000" (:SmlYishuvPUF %)))
       (transform-cols-and-rows {:statAreaCode
                                 (comp parse-int-or-nil
                                       :SmlEzorStatistiKtvtMegurimPUF)
                                 :ashkenaz (comp (specific-vals-to-1-others-to-0 #{3 5})
                                                 parse-int-or-nil
                                                 :YabeshetMotzaByAvMchlkMchvPUF)
                                 :stayed (comp #(= 1 %) parse-int-or-nil :KtvtLifney5ShanaMachozMchvPUF)
                                 })
       :rows
       frequencies
       (map (fn [val-and-freq]
              (into (first val-and-freq) {:count (second val-and-freq)})))
       (sort-by :statAreaCode)
       print-table)

  (->> puf-filename
       read-cols-and-rows
       (filter-cols-and-rows #(= "2650" (:SmlYishuvPUF %)))
       (transform-cols-and-rows {;;:statAreaCode
                                 ;; (comp parse-int-or-nil
                                 ;;    :SmlEzorStatistiKtvtMegurimPUF)
                                 :ashkenaz (comp (specific-vals-to-1-others-to-0 #{3 5})
                                                 parse-int-or-nil
                                                 :YabeshetMotzaByAvMchlkMchvPUF)
                                 :KtvtLifney5ShanaMachozMchvPUF (comp parse-int-or-nil :KtvtLifney5ShanaMachozMchvPUF)
                                 })
       :rows
       frequencies
       (map (fn [val-and-freq]
              (into (first val-and-freq) {:count (second val-and-freq)})))
       (sort-by :count)
       print-table))

(def period-descs
  {0 "ביישוב מלידה"
   1 "עד 1947"
   2 "1948-1954"
   3 "1955-1964"
   4 "1965-1974"
   5 "1975-1984"
   6 "1985-1989"
   7 "1990-1994"
   8 "1995-1999"
   9 "2000-2004"
   10 "2005 ויותר"})
   
(defn draw [cityCode]
  (let [origin-by-period
        (->> puf-filename
             read-cols-and-rows
             (filter-cols-and-rows #(= (str cityCode) (:SmlYishuvPUF %)))
             (transform-cols-and-rows { ;;:statAreaCode
                                       ;; (comp parse-int-or-nil
                                       ;;    :SmlEzorStatistiKtvtMegurimPUF)
                                       :origin (comp parse-int-or-nil
                                                     :YabeshetMotzaByAvMchlkMchvPUF)
                                       ;; :mizrach (comp (specific-vals-to-1-others-to-0 #{1 2})
                                       ;;                parse-int-or-nil
                                       ;;                :YabeshetMotzaByAvMchlkMchvPUF)
                                       ;; :ashkenaz (comp (specific-vals-to-1-others-to-0 #{3 5})
                                       ;;                 parse-int-or-nil
                                       ;;                 :YabeshetMotzaByAvMchlkMchvPUF)
                                       :period (comp regular-int-or-nil :ShanaKnisaYishuvPUF)
                                       })
             (filter-cols-and-rows :period)
             (filter-cols-and-rows :origin)
             ($group-by [:period])
             (#(for [[k v] %]
                 (into k
                       (dissoc (frequencies (to-seq ($ :origin v)))
                               nil))))
             (dataset [:period 0 1 2 3 4 5 6 7 8 9 10])
             ($order :period :asc))]
    (show-chart cityCode
                (with-data origin-by-period
                  (add-lines
                   (xy-plot ($ :period)
                            (map + ($ 1) ($ 2))
                            :title (from-yishuv-code-to-name cityCode)
                            )
                   :period 3)))))

(defn draw-by-stat-area [cityCode]
  (let [cols-and-rows-of-this-city (->> puf-filename
                                        read-cols-and-rows
                                        (filter-cols-and-rows #(= (str cityCode) (:SmlYishuvPUF %)))
                                        (transform-cols-and-rows {
                                                                  :statAreaCode
                                                                  (comp (nil-to-val :other)
                                                                        parse-int-or-nil
                                                                        :Kvutza
                                                                        :SmlEzorStatistiKtvtMegurimPUF)
                                                                  :origin (fn [row]
                                                                            (keyword
                                                                             (str
                                                                              (parse-int-or-nil (:YabeshetMotzaByAvMchlkMchvPUF row))
                                                                              (if (= "1" (:OleShnot90MchvPUF row))
                                                                                "o"
                                                                                ""))))
                                                                  :period (comp regular-int-or-nil
                                                                                :ShanaKnisaYishuvPUF)}))
        origin-types (sort
                      (->> cols-and-rows-of-this-city
                           :rows
                           (map :origin)
                           distinct))]
    (for [statAreaCode (filter identity
                               (get-stat-areas-of-city cityCode))]
      (let [origin-by-period
            (->> cols-and-rows-of-this-city
                 (filter-cols-and-rows #(= statAreaCode (:statAreaCode %)))
                 (filter-cols-and-rows :period)
                 (filter-cols-and-rows :origin)
                 ($group-by [:period])
                 (#(for [[k v] %]
                     (into k
                           (dissoc (frequencies (to-seq ($ :origin v)))
                                   nil))))
                 (dataset (cons :period origin-types))
                 ($order :period :asc))
            origin-by-period-with-compound-origins (conj-cols origin-by-period
                                                              (with-data origin-by-period
                                                                {:o (map +
                                                                         (map nil-to-zero ($ :1o))
                                                                         (map nil-to-zero ($ :2o))
                                                                         (map nil-to-zero ($ :3o))
                                                                         (map nil-to-zero ($ :5o)))
                                                                 :a (map nil-to-zero ($ :3))
                                                                 :m (map +
                                                                         (map nil-to-zero ($ :1))
                                                                         (map nil-to-zero ($ :2 )))}))
            desc (place-desc cityCode statAreaCode)]
        (println desc)
        (future
          (sdisplay desc
                    (ChartPanel.
                     (with-data origin-by-period-with-compound-origins
                       (add-lines
                        (add-lines
                         (xy-plot :period :m
                                  :title ""
                                  :x-label ""
                                  :y-label "")
                         :period :a)
                        :period :o)))
                    nil))))))





(defn plot-coordinates []
 (let [coords (get-coords)
       min-x (apply min ($ :mean-x coords))
       min-y (apply min ($ :mean-y coords))
       max-x (apply max ($ :mean-x coords))
       max-y (apply max ($ :mean-y coords))
       x-axis #(* 5000
                  (/ (- % min-x)
                     (- max-x min-x )))
       y-axis #(- 10000
                  (* 10000 (/ (- % min-y)
                             (- max-x min-y ))))]
    (plot-by-d3
     {"h" 10000
      "w" 5000
      "lines" []
      "circles"
      (map
       (fn [row]
         {"r" 3
          "fill" "red"
          "stroke" "red"
          "opacity" 0.5
          "cx" (x-axis (:mean-x row))
          "cy" (y-axis (:mean-y row))
          "text" (place-desc-of-row row)})
       (:rows coords))})))



(comment
  (->> puf-filename
       read-cols-and-rows
       (transform-cols-and-rows
        {:YabeshetMotzaByAvMchlkMchvPUF (comp parse-int-or-nil :YabeshetMotzaByAvMchlkMchvPUF)
         :OleShnot90MchvPUF (comp parse-int-or-nil :OleShnot90MchvPUF)})
       :rows
       freqs-as-rows
       ))
