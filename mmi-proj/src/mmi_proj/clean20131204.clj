(comment
  (defn reload []
    (do
      (require 'mmi-proj.clean20131204 :reload-all)
      (in-ns 'mmi-proj.clean20131204))))

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
  (:require [clj-liblinear.core :as liblinear]))

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


(def standard-column-fns
  (conj (identity-map [:EretzLeidaZkPUF
                       :YabeshetMotzaByAvMchlkMchvPUF
                       :OleShnot90MchvPUF
                       :RovaKtvtMegurimPUF
                       :TatRovaKtvtMegurimPUF
                       :SmlAnafKalkaliPUF ;; TODO: transform this
                       :SmlMishlachYadPUF ;; TODO: transform this
                       :TkufatSiyumBniyatDiraMchvPUF
                       ]
                      )
        {:KtvtLifney5ShanaMachozMchvPUF (comp parse-int-or-nil :KtvtLifney5ShanaMachozMchvPUF)
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
                         :TzfifutDiurPUF-reg
                         :NefashotMeshekBayitPUF-reg
                         :SmlAnafKalkaliPUF
                         :SmlMishlachYadPUF])


(defn compute-pca-components [_]
  (let [data-for-pca
        (->> (read-cols-and-rows puf-filename
                                 :seq-transformer #(sample % :size 10000))
             (transform-cols-and-rows
              (select-keys standard-column-fns pca-columns))
             cols-and-rows-to-dataset
             filter-all-nonnil)
        pca
        (principal-components (to-matrix data-for-pca))]
    (vec (for [i (range (ncol data-for-pca))]
           (apply hash-map (interleave
                            (col-names data-for-pca)
                            ($ i (:rotation pca))))))))

(defn get-pca-components []
  (compute-and-save-or-load
   compute-pca-components
   (fn [_] "/home/we/workspace/data/pca/components.clj")
   load-file
   pr-str-to-file
   nil
   :components))


;;;;;;;;;;;;;;;;;;;;


(defn compute-coords [_]
  (->> (read-cols-and-rows "/home/we/workspace/data/salesDetails1.tsv"
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
       (map (fn [place-and-data]
              (conj (first place-and-data)
                    {:mean-x (mean ($ :x (second place-and-data)))
                     :mean-y (mean ($ :y (second place-and-data)))})))
       to-dataset))


(defn get-coords []
  ;; broken!!
  (memoized-compute-and-save-or-load
   compute-coords
   (fn [_] "/home/we/workspace/data/coords.csv")
   #(read-dataset %
                  :header true)
   save
   nil
   :coords))

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
  [row]
  (str (clojure.string/replace
        (from-yishuv-code-to-name (:cityCode row))
        #"o" "")
       "-"
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


(defn get-puf-with-components [components]
  (memoized-compute-and-save-or-load
   compute-puf-with-components
   (fn [_] (str "/home/we/workspace/data/puf-with-components-"
               (hash _)
               ".csv"))
   (fn [filename] (read-dataset filename
                               :header true))
   (fn [filename object]
     (save object filename))
   components
   :puf-with-components))



(defn get-measures-by-place
  [dataset-with-places condition-map columns-to-average averaging-fn]
  (to-dataset
   (for [[place-map subset] ($group-by
                         [:cityCode :statAreaCode]
                         dataset-with-places)]
     (let [cond-rowss-map
           (fmap
            (fn [row-condition]
              (filter row-condition
                      (:rows subset))))]
       (conj place-map
             (apply hash-map
                    (apply concat
                           (for [[cond-name cond-rows] cond-rowss-map
                                 col-name columns-to-average]
                             [(concat-keywords :mean
                                               col-name
                                               :given
                                               cond-name)
                              (apply averaging-fn
                                     (map col-name cond-rows))]))))))))

(comment
  (get-measures-by-place
   (get-puf-with-components (get-pca-components))
   {:stayed (fn [row]
             (= 1 (:KtvtLifney5ShanaMachozMchvPUF row)))
    :moved (fn [row]
             (not= 1 (:KtvtLifney5ShanaMachozMchvPUF row)))}
   [:comp0 :comp1]
   (careful-mean 15))


  ;; (defn get-xmeasures-by-place [sub-puf
  ;;                               ]
  ;;   (to-dataset
  ;;    (for [[k v] ($group-by
  ;;                 [:cityCode :statAreaCode]
  ;;                 sub-puf)]
  ;;      (let [s ($where {:KtvtLifney5ShanaMachozMchvPUF 1} v)
  ;;            m ($where {:KtvtLifney5ShanaMachozMchvPUF {:$ne 1}} v)
  ;;            nm ($where {:new-apt 1} m)]
  ;;        (conj k {:n (nrow v)
  ;;                 :mean-axcomp0-s (careful-mean ($ :axcomp0 s))
  ;;                 :mean-axcomp1-s (careful-mean ($ :axcomp1 s))
  ;;                 :mean-axcomp2-s (careful-mean ($ :axcomp2 s))
  ;;                 :mean-axcomp3-s (careful-mean ($ :axcomp3 s))
  ;;                 :mean-axcomp0-m (careful-mean ($ :axcomp0 m))
  ;;                 :mean-axcomp1-m (careful-mean ($ :axcomp1 m))
  ;;                 :mean-axcomp2-m (careful-mean ($ :axcomp2 m))
  ;;                 :mean-axcomp3-m (careful-mean ($ :axcomp3 m))
  ;;                 ;; :mean-axcomp0-nm (careful-mean ($ :axcomp0 nm))
  ;;                 ;; :mean-axcomp1-nm (careful-mean ($ :axcomp1 nm))
  ;;                 ;; :mean-axcomp2-nm (careful-mean ($ :axcomp2 nm))
  ;;                 ;; :mean-axcomp3-nm (careful-mean ($ :axcomp3 nm))
  ;;                 })))))





  ;; (defn gen-map-data [subd]
  ;;   (let [rows (vec (filter
  ;;                    (fn [row] (every? #(not (or (nil? %)
  ;;                                               (Double/isNaN %))) (vals row)))
  ;;                    (:rows
  ;;                     (add-coords-to-place-dataset
  ;;                      (get-measures-by-place subd)))))
  ;;         uu (vec
  ;;             (map (comp #(* 255/512 %) inc signum -)
  ;;                  (map :mean-ucomp1-from-here rows)
  ;;                  (map :mean-ucomp1-from-there rows)))
  ;;         markers (for [i (range (count rows))]
  ;;                   (let [uui (uu i)
  ;;                         row (rows i)]
  ;;                     {:lon (:mean-x row)
  ;;                      :lat (:mean-y row)
  ;;                      :label (place-desc row)
  ;;                      :color (probability-to-color uui)}))
  ;;         center {:lat (mean (filter identity (map :lat markers)))
  ;;                 :lon (mean (filter identity (map :lon markers)))}
  ;;         zoom 11]
  ;;     {:center center
  ;;      :zoom zoom
  ;;      :markers markers}))


  ;; (comment
  ;;   (spit "../client/data.json"
  ;;         (json/write-str
  ;;          (gen-map-data
  ;;           (get-d)
  ;;           ;; ($where {:cityCode 5000}
  ;;           ;;         (get-d))
  ;;           ;; ($where {:statAreaCode {:$ne nil}}
  ;;           ;;                  (get-d))
  ;;           ))))


  ;; (comment
  ;;   ;; google static map
  ;;   (let [center [32 34.9]
  ;;         n 40
  ;;         url (apply str (concat
  ;;                         ["http://maps.googleapis.com/maps/api/staticmap?"
  ;;                          (str "center="
  ;;                               (first center)
  ;;                               ","
  ;;                               (second center)
  ;;                               "&")
  ;;                          "zoom=13&"
  ;;                          "size=900x900&"
  ;;                          "maptype=roadmap&"
  ;;                          "markers="]
  ;;                         (apply str "color:0xFF00FF|"
  ;;                                (for [i (range n)]
  ;;                                  (str 
  ;;                                   (format "%06X" (+ i
  ;;                                                     (* 256 256 (- 256 i))))
  ;;                                   "|"
  ;;                                   (format "%.2f" (+ (- (first center) 0.05)
  ;;                                                     (/ (rand) 10)))
  ;;                                   ","
  ;;                                   (format "%.2f" (+ (- (second center) 0.05)
  ;;                                                     (/ (rand) 10)))
  ;;                                   "|")
  ;;                                  ))
  ;;                         "&markers="
  ;;                         (apply str "color:0x00FF00|"
  ;;                                (for [i (range n)]
  ;;                                  (str 
  ;;                                   (format "%06X" (+ i
  ;;                                                     (* 256 256 (- 256 i))))
  ;;                                   "|"
  ;;                                   (format "%.2f" (+ (- (first center) 0.05)
  ;;                                                     (/ (rand) 10)))
  ;;                                   ","
  ;;                                   (format "%.2f" (+ (- (second center) 0.05)
  ;;                                                     (/ (rand) 10)))
  ;;                                   "|")
  ;;                                  ))
  ;;                         ["&"
  ;;                          "sensor=false&"
  ;;                          "language=iw"]))
  ;;         ]
  ;;     (println url)
  ;;     (sh "firefox" url)))



  ;; (defn plot-by-d3 [d3data]
  ;;   (spit "../client/d3data.json"
  ;;         (json/write-str d3data)))




  ;; (comment
  ;;   (let [data (filter-all-nonnil-and-nonNaN
  ;;               ($ [:prob-m :prob-n
  ;;                   :mean-ucomp1-os :mean-ucomp1-om
  ;;                   :n :cityCode]
  ;;                  (add-coords-to-place-dataset
  ;;                   (get-measures-by-place (get-d)))))        
  ;;         x-axis #(* 1000 %)
  ;;         y-axis #(- 1000 (* 1000 %))]
  ;;     (plot-by-d3
  ;;      {"h" 2000
  ;;       "w" 1000
  ;;       "lines" [{"x" (x-axis 0) "y" (y-axis 0)}
  ;;                {"x" (x-axis 1) "y" (y-axis 0)}
  ;;                {"x" (x-axis 1) "y" (y-axis 1)}
  ;;                {"x" (x-axis 0) "y" (y-axis 1)}
  ;;                {"x" (x-axis 0) "y" (y-axis 0)}
  ;;                {"x" (x-axis 1) "y" (y-axis 1)}]
  ;;       "circles" (map (fn [row]
  ;;                        (let [color (case (:cityCode row)
  ;;                                      5000 "white"
  ;;                                      4000 "red"
  ;;                                      3000 "yellow"
  ;;                                      70 "blue"
  ;;                                      "#666666")]
  ;;                          {"cx" (x-axis (:prob-m row))
  ;;                           "cy" (y-axis (-
  ;;                                         (:mean-ucomp1-om row)
  ;;                                         (:mean-ucomp1-os row)))
  ;;                           "r" 5
  ;;                           ;; (/ (sqrt (:n row))
  ;;                           ;;     2)
  ;;                           "fill" color
  ;;                           "stroke" color
  ;;                           "opacity" 0.5
  ;;                           "text" (place-desc row)}))
  ;;                      (:rows data))})))


  ;; (comment
  ;;   (let [data (filter-all-nonnil-and-nonNaN
  ;;               ($ [:prob-m :prob-n
  ;;                   :prob-o-os :prob-o-om
  ;;                   :prob-a-os :prob-a-om
  ;;                   :mean-ucomp1-os :mean-ucomp1-om
  ;;                   :n :cityCode :statAreaCode]
  ;;                  (add-coords-to-place-dataset
  ;;                   (get-measures-by-place (get-d)))))]
  ;;     (save
  ;;      ($ [:prob-m :prob-n
  ;;          :prob-o-os :prob-o-om
  ;;          :prob-a-os :prob-a-om
  ;;          :mean-ucomp1-os :mean-ucomp1-om
  ;;          :yishuv-name :desc]
  ;;         (add-column :desc
  ;;                     (map place-desc
  ;;                          (:rows data))
  ;;                     (add-column :yishuv-name
  ;;                                 (map (comp from-yishuv-code-to-name
  ;;                                            (nil-to-val "other")
  ;;                                            (leave-only-nil-and-values-of-set #{3000 4000 5000 7000 70 6100 1031 2800}))
  ;;                                      ($ :cityCode data))
  ;;                                 data)))
  ;;      "../client/my-scatter/scatter.csv")))


  ;; ;;;;;;;;;;;



  ;; (defn get-sales-summary-by-place-map
  ;;   [years]
  ;;   (let [;;;;
  ;;         summary-by-place-by-year
  ;;         (->> (read-cols-and-rows "/home/we/workspace/data/salesDetails1.tsv"
  ;;                                  :delimiter "\t")
  ;;              (transform-cols-and-rows {:cityCode (comp parse-int-or-nil
  ;;                                                        :cityCode)
  ;;                                        :statAreaCode (comp parse-int-or-nil
  ;;                                                            ;; nil happens,
  ;;                                                            ;; for example,
  ;;                                                            ;; when "null"
  ;;                                                            ;; appears in data.
  ;;                                                            :statAreaCode)
  ;;                                        :pricePerMeter (comp parse-int-or-nil
  ;;                                                             :priceByMeter)
  ;;                                        :year #(parse-int-or-nil
  ;;                                                (first (clojure.string/split (:saleDay %) #"-")))})
  ;;              (filter-cols-and-rows #((into #{} years)
  ;;                                      (:year %)))
  ;;              cols-and-rows-to-dataset
  ;;              ($group-by [:year])
  ;;              (map (fn [year-and-data]
  ;;                     [(first year-and-data)
  ;;                      (->> year-and-data
  ;;                           second
  ;;                           ($group-by [:cityCode :statAreaCode])
  ;;                           (map (fn [place-and-data]
  ;;                                  [(first place-and-data)
  ;;                                   {:medppm (median
  ;;                                             (to-seq ($ :pricePerMeter
  ;;                                                        (second place-and-data))))
  ;;                                    :n (nrow (second place-and-data))}]))
  ;;                           (apply concat)
  ;;                           (apply hash-map))]))
  ;;              (apply concat)
  ;;              (apply hash-map))
  ;;         ;;;;
  ;;         unifprice-by-place-by-year
  ;;         (fmap (fn [summary-by-place]
  ;;                 (apply hash-map
  ;;                        (apply concat
  ;;                               (map vector
  ;;                                    (keys summary-by-place)
  ;;                                    (map double (uniformize (map :medppm
  ;;                                                                 (vals summary-by-place))))))))
  ;;               summary-by-place-by-year)
  ;;         ;;;;
  ;;         places
  ;;         (distinct (apply concat
  ;;                          (map (fn [year-and-unifprice-by-place]
  ;;                                 (map first (second year-and-unifprice-by-place)))
  ;;                               unifprice-by-place-by-year)))
  ;;         ;;;;
  ;;         summary-by-place
  ;;         (apply hash-map
  ;;                (apply concat
  ;;                       (for [place places]
  ;;                         [place (into {}
  ;;                                      (for [year years]
  ;;                                        {(keyword (str "unifprice" year))
  ;;                                         (get-in unifprice-by-place-by-year
  ;;                                                 [{:year year} place])
  ;;                                         (keyword (str "n" year))
  ;;                                         (get-in summary-by-place-by-year
  ;;                                                 [{:year year} place :n])}))])))
  ;;         ]
  ;;     summary-by-place))




  ;; ;; (pprint (frequencies (apply concat
  ;; ;;                                       (map #(map second (second %))
  ;; ;;                                            (get-ppm-summary)))))

  ;; ;; (let [ppm-summary ]
  ;; ;;   (println (map (juxt first
  ;; ;;                       (comp count second)) ppm-summary))
  ;; ;;   ppm-summary)






  ;; ;; (defn unified-data-by-place []


  ;; ;;   )



  ;; (comment
  ;;   (def x
  ;;     (set
  ;;      (keys
  ;;       (get-sales-summary-by-place-map))))
  ;;   (def y
  ;;     (set
  ;;      (distinct (:rows ($ [:cityCode :statAreaCode] (get-d))))))
  ;;   (distinct
  ;;    (map identity; (comp from-yishuv-code-to-name :cityCode)
  ;;         (clojure.set/difference x y)))
  ;;   (distinct
  ;;    (map identity; (comp from-yishuv-code-to-name :cityCode)
  ;;         (clojure.set/difference y x))))


  ;; (comment
  ;;   ($join [[:a] [:a]]
  ;;          (dataset [:a :b]
  ;;                   [[1 11]
  ;;                    [2 12]
  ;;                    [3 13]])
  ;;          (dataset [:a :c]
  ;;                   [[1 21]
  ;;                    [2 22]
  ;;                    [4 24]])))

  ;; (def get-sales-summary-by-place
  ;;   (memoize (fn [years]
  ;;              (to-dataset (map #(apply conj %)
  ;;                               (get-sales-summary-by-place-map years))))))

  ;; (def get-sales-summary-by-coords
  ;;   (memoize (fn [years]
  ;;              (add-coords-to-place-dataset
  ;;               (get-sales-summary-by-place)))))

  ;; (defn sort-colnames [adataset]
  ;;   (dataset (sort (col-names adataset))
  ;;            (:rows adataset)))


  ;; (comment
  ;;   (let [summaries-by-coords (sort-colnames
  ;;                              (filter-all-nonnil-and-nonNaN
  ;;                               (get-sales-summary-by-coords (range 2006 2011))))
  ;;         margin 80
  ;;         scale 1000
  ;;         x-axis #(+ margin (* scale %))
  ;;         y-axis #(+ margin (- scale (* scale %)))
  ;;         n-columns (filter #(re-matches #"n.*" (name %))
  ;;                           (col-names summaries-by-coords))]
  ;;     (plot-by-d3
  ;;      {"h" (+ scale margin margin)
  ;;       "w" (+ scale margin margin margin)
  ;;       "lines" [{"x" (x-axis 0) "y" (y-axis 0)}
  ;;                {"x" (x-axis 1) "y" (y-axis 0)}
  ;;                {"x" (x-axis 1) "y" (y-axis 1)}
  ;;                {"x" (x-axis 0) "y" (y-axis 1)}
  ;;                {"x" (x-axis 0) "y" (y-axis 0)}
  ;;                {"x" (x-axis 1) "y" (y-axis 1)}]
  ;;       "circles" (map (fn [row]
  ;;                        (let [color (case (:cityCode row)
  ;;                                      5000 "white"
  ;;                                      4000 "red"
  ;;                                      3000 "yellow"
  ;;                                      6400 "magenta"
  ;;                                      7000 "blue"
  ;;                                      3797 "#44ee99"
  ;;                                      "#777777")]
  ;;                          {"cx" (x-axis (log (/ (:unifprice2008 row) (:unifprice2006 row))))
  ;;                           "cy" (y-axis (log (/ (:unifprice2010 row) (:unifprice2006 row))))
  ;;                           "r" (/ (sqrt (:n2006 row))
  ;;                                  5)
  ;;                           "fill" color
  ;;                           "stroke" color
  ;;                           "opacity" 0.5
  ;;                           "text" (place-desc row)}))
  ;;                      (filter (fn [row]
  ;;                                (< 10
  ;;                                   (apply min (map row n-columns))))
  ;;                              (:rows summaries-by-coords)))})))



  ;; ;;;;;;;;;;;;;;;;;;;;;

  ;; (def get-join-by-coords
  ;;   (memoize (fn []
  ;;              (let [measures-by-place (get-measures-by-place (get-d))
  ;;                    summary-by-place (get-sales-summary-by-place (range 2006 2011))
  ;;                    join-by-place ($join [[:cityCode :statAreaCode]
  ;;                                          [:cityCode :statAreaCode]]
  ;;                                         summary-by-place
  ;;                                         measures-by-place)
  ;;                    join-by-coords (sort-colnames
  ;;                                    (add-coords-to-place-dataset join-by-place))]
  ;;                join-by-coords))))


  ;; (defn careful-log-ratio
  ;;   [x y]
  ;;   (if (>= 0 (min x y))
  ;;     Double/NaN
  ;;     (log-ratio x y)))

  ;; (defn logit
  ;;   [x]
  ;;   (- (log x)
  ;;      (log (- 1 x))))

  ;; (defn logits-difference
  ;;   [x y]
  ;;   (- (logit x)
  ;;      (logit y)))

  ;; (comment
  ;;   (let [data (filter-all-nonnil-and-nonNaN
  ;;               ($ [:cityCode :statAreaCode
  ;;                   :mean-ucomp1-os :mean-ucomp1-om
  ;;                   :prob-a-os :prob-a-om
  ;;                   :prob-o-os :prob-o-om
  ;;                   :unifprice2006 :unifprice2007 :unifprice2008 :unifprice2009 :unifprice2010
  ;;                   :mean-x :mean-y
  ;;                   :n2006 :n2007 :n2008 :n2009 :n2010
  ;;                   ]
  ;;                  (get-join-by-coords)))]
  ;;     (save
  ;;      (filter-all-nonnil-and-nonNaN
  ;;       (sort-colnames
  ;;        ($ [:prob-a-change
  ;;            :prob-o-change
  ;;            :ucomp1-change
  ;;            :unifprice-change-2010-2006
  ;;            :unifprice2006
  ;;            :desc
  ;;            :yishuv-name
  ;;            ]
  ;;           (add-column
  ;;            :prob-o-change
  ;;            (map logits-difference
  ;;                 ($ :prob-o-om data)
  ;;                 ($ :prob-o-os data))
  ;;            (add-column
  ;;             :prob-a-change
  ;;             (map logits-difference
  ;;                  ($ :prob-a-om data)
  ;;                  ($ :prob-a-os data))
  ;;             (add-column
  ;;              :ucomp1-change
  ;;              (map logits-difference
  ;;                   ($ :mean-ucomp1-om data)
  ;;                   ($ :mean-ucomp1-os data))
  ;;              (add-column
  ;;               :unifprice-change-2010-2006
  ;;               (map logits-difference
  ;;                    ($ :unifprice2010 data)
  ;;                    ($ :unifprice2006 data))
  ;;               (add-column :desc
  ;;                           (map place-desc
  ;;                                (:rows data))
  ;;                           (add-column :yishuv-name
  ;;                                       (map (comp from-yishuv-code-to-name
  ;;                                                  (nil-to-val "other")
  ;;                                                  (leave-only-nil-and-values-of-set #{3000 4000 5000 7000 70 6100 1031 2800 9000 7600 7900}))
  ;;                                            ($ :cityCode data))
  ;;                                       data)))))))))
  ;;      "../client/my-scatter/scatter.csv")))




  ;; (defn general-gen-map-data
  ;;   [data colname transf color-func]
  ;;   (let [rows (vec (filter
  ;;                    (fn [row] (every? #(not (or (nil? %)
  ;;                                               (Double/isNaN %))) (vals row)))
  ;;                    (:rows data)))
  ;;         values (vec
  ;;                 (map transf
  ;;                      (map colname rows)))
  ;;         markers (for [i (range (count rows))]
  ;;                   (let [vali (values i)
  ;;                         row (rows i)]
  ;;                     {:lon (:mean-x row)
  ;;                      :lat (:mean-y row)
  ;;                      :label (place-desc row)
  ;;                      :color (color-func vali)}))
  ;;         center {:lat (mean (filter identity (map :lat markers)))
  ;;                 :lon (mean (filter identity (map :lon markers)))}
  ;;         zoom 11]
  ;;     {:center center
  ;;      :zoom zoom
  ;;      :markers markers}))

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


  ;; (defn incanter-dataset-to-weka-dataset
  ;;   [name incanter-dataset]
  ;;   (make-dataset name
  ;;                 (col-names incanter-dataset)
  ;;                 (map vals (:rows incanter-dataset))))


  ;; (defn weka-dataset-to-incanter-dataset
  ;;   [weka-dataset]
  ;;   (dataset 
  ;;    (attribute-names weka-dataset)
  ;;    (map instance-to-map weka-dataset)))



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
  ;; ;;                                       (add-column :desc (map place-desc
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
  )
