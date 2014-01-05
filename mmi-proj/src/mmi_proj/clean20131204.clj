(comment
  (do
    (require 'mmi-proj.clean20131204 :reload-all)
    (in-ns 'mmi-proj.clean20131204))
  (defn reload []
    (require 'mmi-proj.clean20131204 :reload-all)))

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
  (:require [clojure.data.generators :as gen])
  ;(:require [clojure.data.xml :as dxml])
  (:require [clojure.xml :as xml]))

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
                           :ShanaKnisaYishuvPUF
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
         :Arab #(if (and (#{2 3 5}
                          (parse-int-or-nil
                           (:DatZkPUF %)))
                         (not (= "1"
                                 (:OleShnot90MchvPUF %))))
                  1 0)
         :aliyah (comp (specific-val-to-1-others-to-0 1)
                       parse-int-or-nil
                       :OleShnot90MchvPUF)
         :mizrach #(if (and (#{1 2}
                             (parse-int-or-nil
                              (:YabeshetMotzaByAvMchlkMchvPUF %)))
                            (not (= "1"
                                    (:OleShnot90MchvPUF %))))
                     1 0)
         :ashkenaz #(if (and (#{3 5}
                              (parse-int-or-nil
                               (:YabeshetMotzaByAvMchlkMchvPUF %)))
                             (not (= "1"
                                     (:OleShnot90MchvPUF %))))
                      1 0)
         :local #(if (and (#{9}
                           (parse-int-or-nil
                            (:YabeshetMotzaByAvMchlkMchvPUF %)))
                          (not (= "1"
                                  (:OleShnot90MchvPUF %))))
                   1 0)
         :income1 (comp
                   #(if %
                      (if (< % 7)
                        1
                        0))
                   parse-int-or-nil
                   :Hchns2008MbMchvPUF)
         :income2 (comp
                   #(if %
                      (if (< 6 % 13)
                        1
                        0))
                   parse-int-or-nil
                   :Hchns2008MbMchvPUF)
         :income3 (comp
                   #(if %
                      (if (< 12 %)
                        1
                        0))
                   parse-int-or-nil
                   :Hchns2008MbMchvPUF)
         :edu012 #(if (#{0 1 2}
                      (parse-int-or-nil
                       (:TeudatLimudZkPUF %)))
                   1
                   0)
         :edu34 #(if (#{3 4}
                      (parse-int-or-nil
                       (:TeudatLimudZkPUF %)))
                   1
                   0)
         :edu567 #(if (#{5 6 7}
                       (parse-int-or-nil
                        (:TeudatLimudZkPUF %)))
                    1
                    0)
         :occ01 #(if (#{0 1}
                      (parse-int-or-nil
                       (:SmlMishlachYadPUF %)))
                   1
                   0)
         :occ2 #(if (#{2}
                     (parse-int-or-nil
                      (:SmlMishlachYadPUF %)))
                  1
                  0)          
         :occ3456 #(if (#{3 4 5 6}
                         (parse-int-or-nil
                          (:SmlMishlachYadPUF %)))
                      1
                      0)
         :period (comp parse-int-or-nil :ShanaKnisaYishuvPUF)
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

(defn add-components [components-coefficients cols-and-rows]
  (->> cols-and-rows
       ((apply comp
               (for [i (range (count components-coefficients))]
                 #(add-linear-combination-column (components-coefficients i)
                                                 (keyword (str "comp" i))
                                                 %))))
       (remove-columns (keys (components-coefficients 0)))
       cols-and-rows-to-dataset
       ;;(add-coords-to-place-dataset)
       filter-all-nonnil))


(defn test-correlations-of-components [dat]
  (correlation
   (to-matrix
    ($ [:comp0 :comp1 :comp2 :comp3 :comp4 :comp5 :comp6]
       dat))))


(def col-names-to-avoid [:Hchns2008BrutoSachirPUF-int
                         :MspSherutimPUF-reg
                         :MspChadarimPUF-reg
                         ;; :TzfifutDiurPUF-reg
                         ;; :NefashotMeshekBayitPUF-reg
                         :SmlAnafKalkaliPUF
                         :SmlMishlachYadPUF
                         ])


(defn center-matrix
  [mat]
  (apply bind-columns
         (for [i (range (ncol mat))]
           (clx/-
            ($ i mat)
            (mean (seq ($ i mat)))))))

(defn center-dataset
  [dat]
  (-> dat
      to-matrix
      center-matrix
      to-dataset
      (col-names (col-names dat))))

(defn compute-data-for-pca
  [n-samples]
  (->> (read-cols-and-rows puf-filename
                           :seq-transformer #(sample % :size n-samples))
       (transform-cols-and-rows
        (select-keys standard-column-fns pca-columns))
       cols-and-rows-to-dataset
       filter-all-nonnil))

(def get-data-for-pca
  (memoize
   (fn [n-samples]
     (compute-and-save-or-load
      compute-data-for-pca
      (fn [n-samples] (str
              "/home/we/workspace/data/data-for-pca-"
              n-samples
              ".csv"))
      (fn [filename] (read-dataset filename
                                  :header true))
      (fn [filename object]
        (save object filename))
      n-samples
      :data-for-pca))))


(defn compute-components-coefficients [_]
  (let [data-for-pca (get-data-for-pca 10000)
        data-matrix (to-matrix data-for-pca)
        centered-matrix (center-matrix data-matrix)
        svd (clx/svd centered-matrix)
        components-coefficients (vec
                                 (for [i (range (ncol data-for-pca))]
                                   (apply hash-map (interleave
                                                    (col-names data-for-pca)
                                                    ($ i (:right svd))))))
        ;; data-for-pca-with-components
        ;; (-> data-for-pca
        ;;     dataset-to-cols-and-rows
        ;;     ((apply comp
        ;;             (for [i (range (count components-coefficients))]
        ;;               #(add-linear-combination-column
        ;;                 (components-coefficients i)
        ;;                 (keyword (str "comp" i))
        ;;                 %))))
        ;;     cols-and-rows-to-dataset)
        ;; components-matrix (to-matrix
        ;;                    ($ [:comp0 :comp1 :comp2 :comp3 :comp4 :comp5 :comp6]
        ;;                       data-for-pca-with-components))
        ]
    ;; (correlation components-matrix)
    ;; (map #(sqrt (variance %))
    ;;          (for [i (range (ncol components-matrix))]
    ;;            ($ i components-matrix)))
    (test-correlations-of-components
                         (add-components
                          components-coefficients
                          (center-dataset (get-data-for-pca 10000))))
    components-coefficients))

(def get-components-coefficients
  (memoize
   (fn []
     (compute-and-save-or-load
      compute-components-coefficients
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

(defn place-desc1-of-row
  [row]
  (place-desc (:cityCode row)
              (.substring (str (:statAreaCode row))
                          0 1)))


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



(defn probability-to-color
  [prob]
  (let [prob256 (int (* 256 prob))]
    (format "#%06X"
            (+ (- 256 prob256)
               (* 256 256 prob256)))))


(defn rgb-to-color
  [r g b]
  (format "#%06X"
          (+ (* 65536 (int (* 256 r)))
             (* 256 (int (* 256 g)))
             (int (* 256 b)))))



(defn compute-puf-with-components [components]
  (->> (read-cols-and-rows puf-filename)
       (transform-cols-and-rows standard-column-fns)
       (add-components components)))


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
  (time (dim (get-puf-with-components (get-components-coefficients))))
  (test-correlations-of-components
   (get-puf-with-components (get-components-coefficients))))

(defn compute-measures-by-place
  [input]
  (to-dataset
   (for [[place-map subset] ($group-by
                             [:cityCode :statAreaCode]
                             ((:dataset-with-places-fn input)))]
     (try
       (let [cond-rows-map
             (fmap
              (fn [row-condition]
                (filter row-condition
                        (:rows subset)))
              (:condition-map input))]
         (into place-map
               (concat
                (for [[cond-name cond-rows] cond-rows-map]
                  [(concat-keywords :num
                                    cond-name)
                   (count cond-rows)])
                (for [[cond-name cond-rows] cond-rows-map
                      col-name (:columns-to-summarize input)
                      [summarizing-fn-name summarizing-fn] (:summarizing-fns-map input)]
                  [(concat-keywords col-name
                                    summarizing-fn-name
                                    :given
                                    cond-name)
                   (try (summarizing-fn
                         (map col-name cond-rows))
                        (catch Exception e
                          nil))]))))
       (catch Exception e
         (do (println place-map)
             nil))))))

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



(def period-descs
  {0 "birth-";"ביישוב מלידה"
   1 "-1947"
   2 "1948-1954"
   3 "1955-1964"
   4 "1965-1974"
   5 "1975-1984"
   6 "1985-1989"
   7 "1990-1994"
   8 "1995-1999"
   9 "2000-2004"
   10 "2005-"})

(def get-standard-measures-by-place
  (memoize
   (fn []
     (let [input {:dataset-with-places-fn (fn []
                                            (->> (read-cols-and-rows puf-filename)
                                                (transform-cols-and-rows (apply dissoc
                                                                                standard-column-fns
                                                                                pca-columns))
                                                ;; (filter-cols-and-rows
                                                ;;  (fn [row] (and (:cityCode row)
                                                ;;                (:statAreaCode row))))
                                                (cols-and-rows-to-dataset)
                                                (filter-all-nonnil)
                                                ))
                  :condition-map (into {:all (fn [row] true)
                                        :stayed (fn [row]
                                                  (= 1 (:KtvtLifney5ShanaMachozMchvPUF row)))
                                        :moved (fn [row]
                                                 (not= 1 (:KtvtLifney5ShanaMachozMchvPUF row)))}
                                       []
                                       ;; (for [p (range 1 11)]
                                       ;;   {(keyword (period-descs p))
                                       ;;    (fn [row]
                                       ;;      (= p (:period row)))})
                                       )
                  :columns-to-summarize [;:comp0 :comp1 :comp2 :comp3
                                         :new-apt
                                         :Muslim :Jewish :Christian :Druze
                                         :Arab :aliyah :mizrach :ashkenaz :local
                                         :income1 :income2 :income3
                                         :edu012 :edu34 :edu567
                                         :occ01 :occ2 :occ3456
                                         :TzfifutDiurPUF-reg
                                         :NefashotMeshekBayitPUF-reg
                                         ]
                  :summarizing-fns-map {:sum sum
                                        :mean mean;(careful-mean 15)
                                        }}]
       (get-measures-by-place input)))))


(def get-standard-measures-by-coords
  (memoize
   (fn []
     (add-coords-to-place-dataset
      (get-standard-measures-by-place)))))

(comment
  (time (dim (get-standard-measures-by-coords)))
  (view (let [d ($where  {:cityCode {:$in #{4000}}} (get-standard-measures-by-place))]
                                 (add-points
                                  (scatter-plot :income1-mean-given-stayed
                                                :income3-mean-given-stayed
                                                :data d
                                                :group-by :cityCode)
                                  ($ :income1-mean-given-moved d)
                                  ($ :income3-mean-given-moved d)))))






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
        stdprice-by-place-by-year
        (fmap (fn [summary-by-place]
                (apply hash-map
                       (apply concat
                              (map vector
                                   (keys summary-by-place)
                                   (map double (standardize (map :medppm
                                                                 (vals summary-by-place))))))))
              summary-by-place-by-year)
        ;; unifprice-by-place-by-year
        ;; (fmap (fn [summary-by-place]
        ;;         (apply hash-map
        ;;                (apply concat
        ;;                       (map vector
        ;;                            (keys summary-by-place)
        ;;                            (map double (uniformize (map :medppm
        ;;                                                         (vals summary-by-place))))))))
        ;;       summary-by-place-by-year)
          ;;;;
        places
        (distinct (apply concat
                         (map (fn [year-and-stdprice-by-place]
                                (map first (second year-and-stdprice-by-place)))
                              stdprice-by-place-by-year)))
          ;;;;
        summary-by-place
        (apply hash-map
               (apply concat
                      (for [place places]
                        [place (into {}
                                     (for [year years]
                                       {(keyword (str "stdprice" year))
                                        (get-in stdprice-by-place-by-year
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



(defn read-polygons-by-place
  []
  (binding [*print-length* nil
            *print-level* nil]
    (to-dataset
     (for [d (:content
              (xml/parse (java.io.File.
                          "/home/we/workspace/data/wsg84Lamas.gml")))
           :when (= :gml:featureMember (:tag d))]
       (let [d1 (first (:content d))
             d2s (:content d1)
             cityCode (->> d1
                           :content
                           (filter #(= :ogr:SEMEL_YISH
                                       (:tag %)))
                           only-one
                           :content
                           first
                           parse-int-or-nil)
             statAreaCode (->> d1
                               :content
                               (filter #(= :ogr:STAT08
                                           (:tag %)))
                               only-one
                               :content
                               first
                               parse-int-or-nil)
             polygon-str (->> d1
                              :content
                              (filter #(= :ogr:geometryProperty
                                          (:tag %)))
                              only-one
                              :content first
                              :content first
                              :content first
                              :content first
                              :content first
                              ((fn [s]
                                 (if (string? s) s))))]
         {:cityCode cityCode
          :statAreaCode statAreaCode
          :polygon-str polygon-str})))))

(defn read-polygon-from-str [polygon-str]
  (if polygon-str
    (vec
     (for [xy-str (clojure.string/split polygon-str #" ")]
       (vec
        (map parse-double-or-nil
             (clojure.string/split xy-str #",")))))))

(comment
  (update-in (first (filter #(= 5000 (:cityCode %))
                            (read-polygons)))
             [:polygon] #(map vec %)))


(defn compute-join-by-coords []
  (binding [*print-length* nil *print-level* nil]
    (let [measures-by-place (get-standard-measures-by-place)
          summary-by-place (get-sales-summary-by-place (range 2006 2011))
          polygons-by-place (read-polygons-by-place)
          join-by-place ($join [[:cityCode :statAreaCode]
                                [:cityCode :statAreaCode]]
                               polygons-by-place
                               measures-by-place
                               ;; ($join [[:cityCode :statAreaCode]
                               ;;         [:cityCode :statAreaCode]]
                               ;;        summary-by-place
                               ;;        measures-by-place)
                               )
          join-by-coords (sort-colnames
                          (add-coords-to-place-dataset join-by-place))]
      join-by-coords)))


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
  (time (dim (get-join-by-coords)))
  (pprint (map place-desc-of-row
               (filter (comp nil? :polygon-str)
                       (:rows x)))))


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
                                ["stdprice-change"
                                 to-year
                                 from-year]))]
                     (map -
                          ($ (keyword (str "stdprice" to-year)) join-by-coords)
                          ($ (keyword (str "stdprice" from-year)) join-by-coords))))
                  (for [varname [:comp0 :comp1 :comp2 :comp3
                                 :NefashotMeshekBayitPUF-reg
                                 :TzfifutDiurPUF-reg
                                 :new-apt
                                 :Muslim :Christian :Jewish
                                 :ashkenaz :mizrach :aliyah
                                 :income1 :income2 :income3]]
                    (dataset
                     [(keyword (str
                                (name varname) "-change"))]
                     (map -
                          ($ (keyword (str (name varname) "-mean-given-moved")) join-by-coords)
                          ($ (keyword (str (name varname) "-mean-given-stayed")) join-by-coords)))))))))


(comment
  (let [data (->> (add-changes-to-join-by-coords)
                  ($ [:cityCode :statAreaCode
                      ;;:NefashotMeshekBayitPUF-reg-change
                      ;;:TzfifutDiurPUF-reg-change
                      ;; :new-apt-change
                      ;; :Muslim-change :Christian-change 
                      ;;:Jewish-change
                      ;;:ashkenaz-change
                      ;;:mizrach-change
                      ;; :aliyah-change
                      ;;:comp0-mean-given-all
                      ;; :comp1-mean-given-all
                      :income1-change
                      :income2-change
                      :income3-change
                      :comp0-change
                      ;; :comp1-change
                      ;; :stdprice-change-2007-2006
                      ;; :stdprice-change-2008-2007
                      ;; :stdprice-change-2009-2008
                      ;; :stdprice-change-2010-2009
                      :stdprice-change-2010-2006
                      ;;:mean-x
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
    ;; (->> data
    ;;      :rows
    ;;      (map #(fmap signum %))
    ;;      freqs-as-rows
    ;;      (map #(into (:val %) {:-count (:count %)}))
    ;;      (map #(into (sorted-map) %))
    ;;      (filter #(<= 10 (:-count %)))
    ;;      print-table
    ;;      )
    )
  ;; (sdisplay 1
  ;;           (s/vertical-panel
  ;;            :items
  ;;            (for [varname [:NefashotMeshekBayitPUF-reg-change
  ;;                           :TzfifutDiurPUF-reg-change
  ;;                           :new-apt-change
  ;;                           :Muslim-change :Christian-change :Jewish-change
  ;;                           :ashkenaz-change :mizrach-change :aliyah-change]]
  ;;              (org.jfree.chart.ChartPanel.
  ;;               (histogram (filter (complement zero?)
  ;;                                  ($ varname
  ;;                                     (add-changes-to-join-by-coords)))
  ;;                          :title (name varname)
  ;;                          :nbins 100))))
  ;;           nil)
  )

(comment
  (->>
   (add-changes-to-join-by-coords)
   col-names
   (map name)
   (filter #(re-matches #".*change.*" %))
   (map keyword)
   sort
   pprint)
  ;;
  (->> (add-changes-to-join-by-coords)
       ($ [:mean-x :mean-y :stdprice2006 :stdprice2007 :stdprice2008 :stdprice2009 :stdprice2010 :cityCode :statAreaCode
           :Arab-mean-given-stayed
           :aliyah-mean-given-stayed
           :ashkenaz-mean-given-stayed
           :mizrach-mean-given-stayed
           :comp0-mean-given-stayed
           :comp0-change
           :new-apt-mean-given-all])
       filter-complete-rows
       ;; (filter-cols-and-rows
       ;;  #(let [c (:cityCode %)]
       ;;     (= (* 1000 (quot c 1000))
       ;;        c)))
       (filter-cols-and-rows #(or (= 3000 (:cityCode %))
                                  (= 5000 (:cityCode %))))
       (transform-cols-and-rows (conj
                                 (identity-map [;:Arab-mean-given-stayed
                                                ;:aliyah-mean-given-stayed
                                                ;:ashkenaz-mean-given-stayed
                                                ;:mizrach-mean-given-stayed
                                                :comp0-mean-given-stayed
                                                :comp0-change
                                                ;;:new-apt-mean-given-all
                                                ;;:mean-x
                                                :mean-y
                                                :stdprice2006
                                                ;;:stdprice2008
                                                ;;:stdprice2010
                                                ])
                                 {:d1 #(- (:stdprice2008 %)
                                          (:stdprice2006 %))
                                  :d2 #(- (:stdprice2010 %)
                                          (:stdprice2008 %))
                                  :yishuv-name (comp from-yishuv-code-to-name
                                                     (nil-to-val "other")
                                                     (leave-only-nil-and-values-of-set #{3000 4000 5000 7000 70 6100 1031 2800})
                                                     :cityCode)
                                  :desc1 place-desc1-of-row
                                  :desc place-desc-of-row}))
       sort-column-names-of-cols-and-rows
       cols-and-rows-to-dataset
       (#(save % "../client/my-scatter/scatter.csv"))))

(comment
  (->> (get-join-by-coords)
       ($ [:stdprice2006 :stdprice2007 :stdprice2008 :stdprice2009 :stdprice2010 :cityCode :statAreaCode])
       filter-complete-rows
       (filter-cols-and-rows
        #(let [c (:cityCode %)]
           (= (* 1000 (quot c 1000))
              c)))
       (transform-cols-and-rows {:d1 #(- (:stdprice2008 %)
                                         (:stdprice2006 %))
                                 :d2 #(- (:stdprice2010 %)
                                         (:stdprice2008 %))
                                 :cityCode :cityCode
                                 :statAreaCode :statAreaCode})
       cols-and-rows-to-dataset
       (scatter-plot :d1 :d2 :group-by :cityCode :data)
       view))


(defn compute-data-with-clustering
  []
  (let [data (filter-all-nonnil-and-nonNaN
              (add-changes-to-join-by-coords))
        clusterer (make-clusterer :k-means
                                  {:number-clusters 8
                                   :number-iterations 10000})
        weka-dataset (incanter-dataset-to-weka-dataset :data
                                                       ($ [:income1-mean-given-stayed
                                                           :income3-mean-given-stayed
                                                           ;; :stdprice-change-2007-2006
                                                           ;; :stdprice-change-2008-2007
                                                           ;; :stdprice-change-2009-2008
                                                           ;; :stdprice-change-2010-2006
                                                           ]
                                                          ;; [:NefashotMeshekBayitPUF-reg-change
                                                          ;;  :TzfifutDiurPUF-reg-change
                                                          ;;  :new-apt-change
                                                          ;;  :Muslim-change :Christian-change :Jewish-change
                                                          ;;  :ashkenaz-change :mizrach-change :aliyah-change]
                                                          data))
        _ (clusterer-build clusterer
                           weka-dataset)
        labels (map (comp parse-int-or-nil name :class instance-to-map)
                    (clusterer-cluster clusterer
                                       weka-dataset))
        labelled-data (conj-cols data {:label labels})]
    (println ["label frequencies" (frequencies labels)])
    ;; (spit "../client/data2.json"
    ;;       (json/write-str
    ;;        (general-gen-map-data labelled-data
    ;;                              :label
    ;;                              int
    ;;                              ["#000" "#F00" "#0F0" "#00F" "#0FF" "#F0F" "#FF0" "#FFF"]
    ;;                              ;["red" "green" "blue" "cyan"
    ;;                              ;"magenta" "black" "white"]
    ;;                              )))
    labelled-data))


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
               :response-column :stdprice2006
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
               :response-column :logit-stdprice-change-2010-2006
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
                                  {:number-clusters 8
                                   :number-iterations 10000})
        weka-dataset (incanter-dataset-to-weka-dataset :data
                                                       ($ [:income1-mean-given-stayed
                                                           :income2-mean-given-stayed
                                                           :income3-mean-given-stayed
                                                           ;; :stdprice-change-2007-2006
                                                           ;; :stdprice-change-2008-2007
                                                           ;; :stdprice-change-2009-2008
                                                           ;; :stdprice-change-2010-2006
                                                           ]
                                                          ;; [:NefashotMeshekBayitPUF-reg-change
                                                          ;;  :TzfifutDiurPUF-reg-change
                                                          ;;  :new-apt-change
                                                          ;;  :Muslim-change :Christian-change :Jewish-change
                                                          ;;  :ashkenaz-change :mizrach-change :aliyah-change]
                                                          data))
        _ (clusterer-build clusterer
                           weka-dataset)
        labels (map (comp parse-int-or-nil name :class instance-to-map)
                    (clusterer-cluster clusterer
                                       weka-dataset))
        labelled-data (conj-cols data {:label labels})]
    (println (frequencies labels))
    (spit "../client/data2.json"
          (json/write-str
           (general-gen-map-data labelled-data
                                 :label
                                 int
                                 ["#000" "#F00" "#0F0" "#00F" "#0FF" "#F0F" "#FF0" "#FFF"]
                                 ;["red" "green" "blue" "cyan"
                                 ;"magenta" "black" "white"]
                                 )))))



















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
;;            (general-gen-map-data data :stdprice2006 identity probability-to-color)))
;;     (spit "../client/data2.json"
;;           (json/write-str
;;            (general-gen-map-data data :stdprice2010 identity probability-to-color)))))


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
;;               ($ [:stdprice2006 :stdprice2007 :stdprice2008
;;                   :stdprice2009 :stdprice2010
;;                   :cityCode :statAreaCode]
;;                  (get-join-by-coords)))
;;         clusters (som-batch-train
;;                   (to-matrix
;;                    ($ [:stdprice2006 :stdprice2007 :stdprice2008
;;                        :stdprice2009 :stdprice2010]
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
;;                       :stdprice2006 :stdprice2007 :stdprice2008 :stdprice2009 :stdprice2010
;;                       :n2006 :n2007 :n2008 :n2009 :n2010
;;                       :cityCode :statAreaCode
;;                       :mean-x :mean-y]
;;                      (get-join-by-coords)))
;;         data (filter-all-nonnil-and-nonNaN
;;               ($ [:ucomp1-change
;;                   :prob-a-change
;;                   :prob-o-change
;;                   :ucomp1-change
;;                   :stdprice-change-2007-2006
;;                   :stdprice-change-2008-2007
;;                   :stdprice-change-2009-2008
;;                   :stdprice-change-2010-2009
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
;;                               :stdprice-change-2007-2006 (map logits-difference
;;                                                                ($ :stdprice2007 pre-data)
;;                                                                ($ :stdprice2006 pre-data))
;;                               :stdprice-change-2008-2007 (map logits-difference
;;                                                                ($ :stdprice2008 pre-data)
;;                                                                ($ :stdprice2007 pre-data))
;;                               :stdprice-change-2009-2008 (map logits-difference
;;                                                                ($ :stdprice2009 pre-data)
;;                                                                ($ :stdprice2008 pre-data))
;;                               :stdprice-change-2010-2009 (map logits-difference
;;                                                                ($ :stdprice2010 pre-data)
;;                                                                ($ :stdprice2009 pre-data))
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
;;                        :stdprice-change-2007-2006
;;                        :stdprice-change-2008-2007
;;                        :stdprice-change-2009-2008
;;                        :stdprice-change-2010-2009
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
;;                       :stdprice2006 :stdprice2007 :stdprice2008 :stdprice2009 :stdprice2010
;;                       :n2006 :n2007 :n2008 :n2009 :n2010
;;                       :cityCode :statAreaCode
;;                       :mean-x :mean-y]
;;                      (get-join-by-coords)))
;;         data (filter-all-nonnil-and-nonNaN
;;               ($ [:ucomp1-change
;;                   :prob-a-change
;;                   :prob-o-change
;;                   :ucomp1-change
;;                   :stdprice-change-2007-2006
;;                   :stdprice-change-2008-2007
;;                   :stdprice-change-2009-2008
;;                   :stdprice-change-2010-2009
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
;;                     :stdprice-change-2007-2006 (map logits-difference
;;                                                      ($ :stdprice2007 pre-data)
;;                                                      ($ :stdprice2006 pre-data))
;;                     :stdprice-change-2008-2007 (map logits-difference
;;                                                      ($ :stdprice2008 pre-data)
;;                                                      ($ :stdprice2007 pre-data))
;;                     :stdprice-change-2009-2008 (map logits-difference
;;                                                      ($ :stdprice2009 pre-data)
;;                                                      ($ :stdprice2008 pre-data))
;;                     :stdprice-change-2010-2009 (map logits-difference
;;                                                      ($ :stdprice2010 pre-data)
;;                                                      ($ :stdprice2009 pre-data))
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
;;                                                            :stdprice-change-2007-2006
;;                                                            :stdprice-change-2008-2007
;;                                                            :stdprice-change-2009-2008
;;                                                            :stdprice-change-2010-2009
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
;;                       :stdprice2006 :stdprice2007 :stdprice2008 :stdprice2009 :stdprice2010
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




(comment
  (->> puf-filename
       read-cols-and-rows
       :rows
       (filter #(and (= (:SmlYishuvPUF %) "2650")
                     (= (:SmlEzorStatistiKtvtMegurimPUF %) "13")))
       count
       ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(def period-keys
  (map #(keyword (str "p" %))
       (range 1 11)))


(defn compute-measures-for-json
  []
  (let [cond-keys1 [:stayed :moved]
        cond-keys2 (map (comp keyword period-descs)
                        (range 1 11))
        cond-keys (concat cond-keys1 ;cond-keys2
                          )
        group-keys [:edu012 :edu34 :edu567]
        ;;[:occ3456 :occ01 :occ2]
        gk1 (first group-keys)
        gk2 (second group-keys)
        gk3 (last group-keys)
        ;;[:income1 :income2 :income3]
        data (filter-cols-and-rows
              (fn [row] (and (:mean-x row)
                            (:mean-y row)))
              (filter-all-nonnil       ;-and-nonNaN
               (compute-join-by-coords))   ;(compute-data-with-clustering)
              )
        extended-data-rows
        (for [row (:rows data)]
          (let [sums
                (into {}
                      (for [cond-key cond-keys]
                           {cond-key
                            (into {}
                                  (for [group-key group-keys]
                                    {group-key
                                     ((concat-keywords group-key
                                                       :sum-given
                                                       cond-key) row)}))}))
                ;; {:stayed
                ;;  {;; :income1 (:income1-sum-given-stayed row)
                ;;   ;; :income2 (:income2-sum-given-stayed row)
                ;;   ;; :income3 (:income3-sum-given-stayed row)
                ;;   :ashkenaz (:ashkenaz-sum-given-stayed row)
                ;;   :mizrach (:mizrach-sum-given-stayed row)
                ;;   :aliyah (:aliyah-sum-given-stayed row)
                ;;   ;; :Arab (:Arab-sum-given-stayed row)
                ;;   ;; :local (:local-sum-given-stayed row)
                ;;   }
                ;;  :moved
                ;;  {;; :income1 (:income1-sum-given-moved row)
                ;;   ;; :income2 (:income2-sum-given-moved row)
                ;;   ;; :income3 (:income3-sum-given-moved row)
                ;;   :ashkenaz (:ashkenaz-sum-given-moved row)
                ;;   :mizrach (:mizrach-sum-given-moved row)
                ;;   :aliyah (:aliyah-sum-given-moved row)
                ;;   ;; :Arab (:Arab-sum-given-moved row)
                ;;   ;; :local (:local-sum-given-moved row)
                ;;   }}
                contingency-tables sums
                ;; {:stayed (into (:stayed sums)
                ;;                {
                ;;                 ;; :other (- (:num-stayed row)
                ;;                 ;;           (sum (vals (:stayed sums))))
                ;;                 })
                ;;  :moved (into (:moved sums)
                ;;               {
                ;;                ;; :other (- (:num-moved row)
                ;;                ;;           (sum (vals (:moved sums))))
                ;;                })}
                counts
                {:stayed (into (:stayed contingency-tables)
                               {:total (:num-stayed row)
                                :0 0})
                 :moved (into (:moved contingency-tables)
                               {:total (:num-moved row)
                                :0 0})}
                groups (sort (keys (:stayed contingency-tables)))
                lr1r3r (try (log
                             (/ (/ (gk1 (:moved contingency-tables))
                                 (gk1 (:stayed contingency-tables)))
                              (/ (gk3 (:moved contingency-tables))
                                 (gk3 (:stayed contingency-tables)))))
                          (catch Exception e Double/NaN))
                lr1r2r (try (log
                             (/ (/ (gk1 (:moved contingency-tables))
                                   (gk1 (:stayed contingency-tables)))
                                (/ (gk2 (:moved contingency-tables))
                                   (gk2 (:stayed contingency-tables)))))
                            (catch Exception e Double/NaN))
                chisq-val (chi-square-comparison
                           (for [g groups] ((:stayed contingency-tables) g))
                           (for [g groups] ((:moved contingency-tables) g)))]
            (into row
                  {:polygon (read-polygon-from-str
                             (:polygon-str row))
                   :desc
                   (place-desc-of-row row)
                   :chisqval
                   (if (Double/isNaN chisq-val)
                     -1
                     chisq-val)
                   :lr1r2r lr1r2r
                   :plotting {
                              :lr1r2rstr (format "%04f" lr1r2r)
                              :lr1r3rstr (format "%04f" lr1r3r)
                              :color
                              (into {:chisq (if (< chisq-val 6)
                                              "#c0a"
                                              (if (neg? lr1r3r)
                                                "#ac0"
                                                (if (pos? lr1r3r)
                                                  "#0ac"
                                                  "#c0a")))
                                     :lr1lr3r (probability-to-color
                                               (* 0.5
                                                  (+ 1
                                                     (java.lang.Math/tanh lr1r3r))))}
                                    (concat
                                     ;; (for [cond-key cond-keys2]
                                     ;;   {(concat-keywords :origin cond-key)
                                     ;;    (rgb-to-color
                                     ;;     ((concat-keywords :mizrach-mean-given cond-key) row)
                                     ;;     0 ;;((concat-keywords :aliyah-mean-given cond-key) row)
                                     ;;     ((concat-keywords :ashkenaz-mean-given cond-key) row))})
                                     (for [cond-key cond-keys]
                                       {(concat-keywords :edu cond-key)
                                        (rgb-to-color
                                         ((concat-keywords gk1 :mean-given cond-key) row)
                                         0 ;((concat-keywords gk2 :mean-given cond-key) row)
                                         ((concat-keywords gk3 :mean-given cond-key) row)
                                         )})
                                     ;; (for [cond-key cond-keys]
                                     ;;   {(concat-keywords :occ cond-key)
                                     ;;    (rgb-to-color
                                     ;;     ((concat-keywords gk1 :mean-given cond-key) row)
                                     ;;     ((concat-keywords gk2 :mean-given cond-key) row)
                                     ;;     0)})
                                     ;; (for [cond-key cond-keys]
                                     ;;   {(concat-keywords :income cond-key)
                                     ;;    (rgb-to-color
                                     ;;     ((concat-keywords :income1-mean-given cond-key) row)
                                     ;;     0
                                     ;;     ((concat-keywords :income2-mean-given cond-key) row))})
                                     ))
                              ;; { ;; :label (["#000" "#900" "#090"
                              ;;  ;;          "#009" "#099" "#909"
                              ;;  ;;          "#990" "#999"]
                              ;;  ;;           (int (:label row)))
                              ;;  }
                              ;; (rgb-to-color
                              ;;  (:income1-mean-given-all row)
                              ;;  (:income2-mean-given-all row)
                              ;;  (:income3-mean-given-all row))
                              ;; (probability-to-color
                              ;;  (java.lang.Math/tanh lr1r3r))
                              ;; (if (< 10 chisq-val)
                              ;;   "#0b0"
                              ;;   "#404")
                              ;; (probability-to-color ((round 1)
                              ;;                        (/ (+ 1
                              ;;                              (:mizrach-sum-given-moved row))
                              ;;                           (+ 2
                              ;;                              (:mizrach-sum-given-moved row)
                              ;;                              (:ashkenaz-sum-given-moved row)))))
                              :domains
                              [[0 (:num-stayed row)]
                               [0 (:num-moved row)]]
                              :lines
                              (for [g (concat groups [:0 :total])]
                                {:ys [((:stayed counts) g)
                                      ((:moved counts) g)]
                                 :color (case g
                                          :income1 "red"
                                          :income2 "green"
                                          :income3 "blue"
                                          :edu012 "red"
                                          :edu34 "green"
                                          :edu567 "blue"
                                          :occ3456 "red"
                                          :occ01 "green"
                                          :occ2 "blue"
                                          :mizrach "red"
                                          :ashkenaz "blue"
                                          :aliyah "green"
                                          :Arab "orange"
                                          :total "grey"
                                          :0 "grey"
                                          "black")
                                 :name g})}})))
        ]
    {:metadata {:colorkeys (-> extended-data-rows
                               first :plotting :color keys sort)}
     :records (map #(select-keys % (into [:polygon
                                          :mean-x :mean-y
                                          :statAreaCode :cityCode
                                          :num-moved
                                          ;; :aliyah-sum-given-moved
                                          ;; :mizrach-sum-given-moved
                                          ;; :ashkenaz-sum-given-moved
                                          :num-stayed
                                          ;; :aliyah-sum-given-stayed
                                          ;; :mizrach-sum-given-stayed
                                          ;; :ashkenaz-sum-given-stayed
                                          :desc
                                          :chisqval
                                          :lr1r2rstr
                                          :lr1r3rstr
                                          :plotting]
                                         (apply concat
                                                (for [cond-key cond-keys]
                                                  (map (fn [group-key] (concat-keywords group-key :mean-given cond-key))
                                                       group-keys)))))
                   extended-data-rows)}))


(defn write-measures-as-cljs
  []
  (let [filename "../simple/src/cljs/data.cljs"]
    (spit filename
          (str
           "(ns simple.data)\n"
           "(def data '"
           (with-out-str (binding [*print-length* nil
                             *print-level* nil]
                           (pprint (compute-measures-for-json)))
             ")\n")))
    (println ["wrote" filename])))

(defn write-measures-as-json
  []
  (let [filename "../simple/src/cljs/data.json"
        ;; "../client/interactive_map/data.json"
        ]
    (spit filename
          (clojure.string/replace
           (json/write-str
            (compute-measures-for-json))
           "},{"
           "},\n{"))
    (println ["wrote" filename])))


(defn write-measures-as-js
  []
  (let [filename "../simple/public/out/data.js"]
    (spit filename
          (str
           "var data = "
           (clojure.string/replace
            (json/write-str
             (compute-measures-for-json))
            "},{"
            "},\n{")
           ";"))
    (println ["wrote" filename])))

(comment
  (write-measures-as-js))


(comment
  (->> (compute-measures-for-json)
       (map (fn [row]
              (let [lines (:lines (:plotting row))
                    ys-map (into {}
                                 (map (fn [line]
                                        [(:name line)
                                         (:ys line)])
                                      lines))]
                (into {:desc (place-desc-of-row row)
                       :city ((comp from-yishuv-code-to-name
                               (nil-to-val "other")
                               (leave-only-nil-and-values-of-set
                                #{3000
                                  4000
                                  5000
                                  7000
                                  70
                                  6100
                                  1031
                                  2800}))
                        (:cityCode row))}
                      (#(try (hash-map
                              :lr1 (log (/ (apply / (:income1 %))
                                           (apply / (:total %))))
                              :lr2 (log (/ (apply / (:income2 %))
                                           (apply / (:total %))))
                              :lr3 (log (/ (apply / (:income3 %))
                                           (apply / (:total %))))
                              :slr12 (log (/ ((:income1 %) 0)
                                             ((:income2 %) 0)))
                              :slr23 (log (/ ((:income2 %) 0)
                                             ((:income3 %) 0)))
                              :slr13 (log (/ ((:income1 %) 0)
                                             ((:income3 %) 0))))
                             (catch Exception e nil))
                       ys-map)))))
       (filter identity)
       to-dataset
       filter-all-nonnil-and-nonNaN-and-nonInf
       ;($where {:place {:$ne ":other"}})
       (#(save % "/home/we/projects/mmi/client/my-scatter/scatter.csv"))))



(comment
  (->> (compute-measures-for-json)
       (map (fn [row]
              
              ))
       ;; (filter identity)
       ;; to-dataset
       ;; filter-all-nonnil-and-nonNaN-and-nonInf
       ;;($where {:place {:$ne ":other"}})
       ;(#(save %
       ;"/home/we/projects/mmi/client/my-scatter/scatter.csv"))
       ))


(def get-records-as-dataset
  (memoize (fn []
             (to-dataset (:records (compute-measures-for-json))))))

(comment
  (view
   (with-data (get-records-as-dataset)
     (add-points (scatter-plot :edu567-mean-given-stayed :edu012-mean-given-stayed)
                 ($ :edu567-mean-given-moved)
                 ($ :edu012-mean-given-moved))))
  (view
   (with-data (get-records-as-dataset)
     (scatter-plot :edu567-mean-given-stayed :edu012-mean-given-stayed
                   :group-by :cityCode))))
