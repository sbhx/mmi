(comment
  (require 'mmi-proj.clean :reload-all)
  (in-ns 'mmi-proj.clean))

(ns mmi-proj.clean
  (:import java.lang.Math)
  (:import [java.net URL])
  (:import [javax.swing JComponent JLabel JPanel])
  (:import [org.jfree.chart ChartPanel JFreeChart])
  (:require [clj-time.core :as t])
  (:require clojure.core.matrix)
  (:require clojure.core.matrix.operators)
  (:require [clojure.core.reducers :as r])
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
  (:use [clojure.algo.generic.functor :only [fmap]])
  (:use [clojure.java.shell :only [sh]])
  (:use clojure.pprint)
  (:use (incanter core stats charts io zoo som))
  (:use clj-utils.misc)
  (:use clj-utils.visual)
  (:use [c2.core :only (unify)])
  (:use hiccup.core)
  (:use clojure.stacktrace)
  (:use [clj-ml data clusterers]))

(apply require clojure.main/repl-requires)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn filter-cols-and-rows
  [row-filter-func cols-and-rows]
  {:column-names (:column-names cols-and-rows)
   :rows (filter row-filter-func (:rows cols-and-rows))})

(defn cols-and-rows-to-dataset
  [cols-and-rows]
  (dataset (:column-names cols-and-rows)
           (:rows cols-and-rows)))


(defn put-Latin-around-Hebrew [s]
  (if (re-matches #"(?s).*[אבגדהוזחטיכלמנסעפצקרשת].*" s)
    (str "o " s " o")
    s))


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
                       :YabeshetMotzaByAvMchlkMchvPUF
                       :KtvtLifney5ShanaMachozMchvPUF
                       :KtvtLifney5ShanaMetropolinPUF
                       :OleShnot90MchvPUF
                       :RovaKtvtMegurimPUF
                       :TatRovaKtvtMegurimPUF
                       :SmlAnafKalkaliPUF ;; TODO: transform this
                       :SmlMishlachYadPUF ;; TODO: transform this
                       :TkufatSiyumBniyatDiraMchvPUF
                       ]
                      )
        {:cityCode :SmlYishuvPUF
         :statAreaCode :SmlEzorStatistiKtvtMegurimPUF
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
                    (map (fn [place-and-data]
                           (conj (first place-and-data)
                                 {:mean-x (mean ($ :x (second place-and-data)))
                                  :mean-y (mean ($ :y (second place-and-data)))})))
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
           {{:cityCode (:cityCode row)
             :statAreaCode (:statAreaCode row)}
            (select-keys row [:mean-x
                              :mean-y])})))


;; ;;;;;;;;;;;;;;;;;;;;;

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


(def d-filename "/home/we/workspace/data/d.csv")

(comment
  (time (let [pre-d (read-dataset pre-d-filename
                                  :header true)
              pre-d-1 (conj-cols pre-d
                                 (dataset [:mean-x :mean-y]
                                          (map
                                           ; change nils with {}s
                                           #(if % % {}) 
                                           (map coords-map
                                                (:rows
                                                 ($ [:cityCode
                                                     :statAreaCode]
                                                    pre-d))))))
              pre-d-2 (add-column :ucomp1
                                  (map double (uniformize ($ :comp1 pre-d-1)))
                                  (add-column :yishuv-name
                                              (map from-yishuv-code-to-name
                                                   ($ :cityCode pre-d-1))
                                              pre-d-1))]
          (println (dim pre-d-2))
          (save pre-d-2 d-filename))))

(def get-d
  (memoize #(read-dataset d-filename
                          :header true)))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn comparison [d]
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
        [:cityCode :RovaKtvtMegurimPUF :new-apt] :asc
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

(defn stat-areas [d]
  (distinct
   ($ [:cityCode :statAreaCode]
      d)))

(defn mean-ucomp1-by-place [subd]
  ($rollup mean
           :ucomp1
           [:cityCode :statAreaCode]
           subd))

(defn to-seq [val-or-vals]
  (if (sequential? val-or-vals)
    val-or-vals
    [val-or-vals]))

(defn careful-mean [number-or-numbers]
  (if (number? number-or-numbers)
    nil
    (if (< 15 (count number-or-numbers))
      (mean number-or-numbers))))

(defn get-measures-by-place [subd]
  (to-dataset
   (for [[k v] ($group-by
                [:cityCode :statAreaCode]
                subd)]
     (conj k {:n (nrow v)
              :prob-n (careful-mean
                       ($ :new-apt v))
              :prob-m (careful-mean
                       (map #(if (= 1 %) 0 1)
                            (to-seq
                             ($ :KtvtLifney5ShanaMachozMchvPUF v))))
              :prob-o (careful-mean
                       (map #(if (= 1 %) 1 0)
                            (to-seq
                             ($ :OleShnot90MchvPUF v))))
              :prob-a (careful-mean
                       (map #(if (= 3 %) 1 0)
                            (to-seq
                             ($ :YabeshetMotzaByAvMchlkMchvPUF v))))
              :prob-o-om (careful-mean
                          (map #(if (= 1 %) 1 0)
                               (to-seq
                                ($ :OleShnot90MchvPUF
                                   ($where {:new-apt {:$ne 1}
                                            :KtvtLifney5ShanaMachozMchvPUF {:$ne 1}} v)))))
              :prob-o-os (careful-mean
                          (map #(if (= 1 %) 1 0)
                               (to-seq ($ :OleShnot90MchvPUF
                                          ($where {:new-apt {:$ne 1}
                                                   :KtvtLifney5ShanaMachozMchvPUF 1} v)))))
              :prob-a-om (careful-mean
                          (map #(if (= 3 %) 1 0)
                               (to-seq
                                ($ :YabeshetMotzaByAvMchlkMchvPUF
                                   ($where {:new-apt {:$ne 1}
                                            :KtvtLifney5ShanaMachozMchvPUF {:$ne 1}} v)))))
              :prob-a-os (careful-mean
                          (map #(if (= 3 %) 1 0)
                               (to-seq ($ :YabeshetMotzaByAvMchlkMchvPUF
                                          ($where {:new-apt {:$ne 1}
                                                   :KtvtLifney5ShanaMachozMchvPUF 1} v)))))
              :mean-ucomp1-nm (careful-mean
                               ($ :ucomp1 ($where {:new-apt 1
                                                   :KtvtLifney5ShanaMachozMchvPUF {:$ne 1}} v)))
              ;; :mean-ucomp1-ns (careful-mean
              ;;                  ($ :ucomp1 ($where {:new-apt 1
              ;;                                      :KtvtLifney5ShanaMachozMchvPUF 1} v)))
              :mean-ucomp1 (careful-mean
                               ($ :ucomp1 v))
              :mean-ucomp1-om (careful-mean
                               ($ :ucomp1 ($where {:new-apt {:$ne 1}
                                                   :KtvtLifney5ShanaMachozMchvPUF {:$ne 1}} v)))
              :mean-ucomp1-os (careful-mean
                               ($ :ucomp1 ($where {:new-apt {:$ne 1}
                                                   :KtvtLifney5ShanaMachozMchvPUF 1} v)))}))))

(defn add-coords-to-place-dataset [place-dataset]
  (to-dataset
   (map (fn [row]
          (into row
                (coords-map (select-keys row
                                         [:cityCode
                                          :statAreaCode]))))
        (:rows place-dataset))))


(defn plot [subd]
  (let [chart (scatter-plot [] [])
        rows (vec (filter
                   (fn [row] (every? #(not (or (nil? %)
                                              (Double/isNaN %))) (vals row)))
                   (:rows
                    (add-coords-to-place-dataset
                     (get-measures-by-place subd)))))
        uu (vec
            (uniformize (map -
                             (map :mean-ucomp1-omfrom-here rows)
                             (map :mean-ucomp1-from-there rows))))] ;; NOTE THIS!
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
    ;; (let [city ($where {:cityCode 5000} mean-ucomp1-by-coords)]
    ;;   (add-lines chart
    ;;              ($ :mean-x city)
    ;;              ($ :mean-y city))
    ;;   (.setSeriesPaint
    ;;    (.getRenderer (.getPlot chart) (inc (count rows)))
    ;;      0
    ;;      (java.awt.Color. 1 1 1)))
    chart))


(comment
  ;; playing with charts
  (let [n 999
        chart (scatter-plot [] [])
        uu (vec
            (uniformize (range n)))]
    (doseq [i (range n)]
      (add-points chart
                  [i]
                  [i]))
    (doseq [i (range n)]
      (let [uui (double (uu i))]
        (println uui)
        (.setSeriesPaint
         (.getRenderer (.getPlot chart) i)
         0
         (java.awt.Color. (float uui)
                          (float (- 1 uui))
                          (float (- 1 uui))))))
    (view chart)))

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


(defn gen-map-data [subd]
  (let [rows (vec (filter
                   (fn [row] (every? #(not (or (nil? %)
                                              (Double/isNaN %))) (vals row)))
                   (:rows
                    (add-coords-to-place-dataset
                     (get-measures-by-place subd)))))
        uu (vec
            (map (comp #(* 255/512 %) inc signum -)
                 (map :mean-ucomp1-from-here rows)
                 (map :mean-ucomp1-from-there rows)))
        markers (for [i (range (count rows))]
                  (let [uui (uu i)
                        row (rows i)]
                    {:lon (:mean-x row)
                     :lat (:mean-y row)
                     :label (place-desc row)
                     :color (probability-to-color uui)}))
        center {:lat (mean (filter identity (map :lat markers)))
                :lon (mean (filter identity (map :lon markers)))}
        zoom 11]
    {:center center
     :zoom zoom
     :markers markers}))


(comment
  (spit "../client/data.json"
        (json/write-str
         (gen-map-data
          (get-d)
          ;; ($where {:cityCode 5000}
          ;;         (get-d))
          ;; ($where {:statAreaCode {:$ne nil}}
          ;;                  (get-d))
          ))))


(comment
  ;; google static map
  (let [center [32 34.9]
        n 40
        url (apply str (concat
                        ["http://maps.googleapis.com/maps/api/staticmap?"
                         (str "center="
                              (first center)
                              ","
                              (second center)
                              "&")
                         "zoom=13&"
                         "size=900x900&"
                         "maptype=roadmap&"
                         "markers="]
                        (apply str "color:0xFF00FF|"
                             (for [i (range n)]
                               (str 
                                (format "%06X" (+ i
                                                  (* 256 256 (- 256 i))))
                                "|"
                                (format "%.2f" (+ (- (first center) 0.05)
                                                  (/ (rand) 10)))
                                ","
                                (format "%.2f" (+ (- (second center) 0.05)
                                                  (/ (rand) 10)))
                                "|")
                               ))
                        "&markers="
                        (apply str "color:0x00FF00|"
                               (for [i (range n)]
                                 (str 
                                  (format "%06X" (+ i
                                                    (* 256 256 (- 256 i))))
                                  "|"
                                  (format "%.2f" (+ (- (first center) 0.05)
                                                    (/ (rand) 10)))
                                  ","
                                  (format "%.2f" (+ (- (second center) 0.05)
                                                    (/ (rand) 10)))
                                  "|")
                                 ))
                        ["&"
                         "sensor=false&"
                         "language=iw"]))
        ]
    (println url)
    (sh "firefox" url)))



(defn plot-by-d3 [d3data]
  (spit "../client/d3data.json"
        (json/write-str d3data)))




(comment
  (let [data (filter-all-nonnil-and-nonNaN
              ($ [:prob-m :prob-n
                  :mean-ucomp1-os :mean-ucomp1-om
                  :n :cityCode]
                 (add-coords-to-place-dataset
                  (get-measures-by-place (get-d)))))        
        x-axis #(* 1000 %)
        y-axis #(- 1000 (* 1000 %))]
    (plot-by-d3
     {"h" 2000
      "w" 1000
      "lines" [{"x" (x-axis 0) "y" (y-axis 0)}
               {"x" (x-axis 1) "y" (y-axis 0)}
               {"x" (x-axis 1) "y" (y-axis 1)}
               {"x" (x-axis 0) "y" (y-axis 1)}
               {"x" (x-axis 0) "y" (y-axis 0)}
               {"x" (x-axis 1) "y" (y-axis 1)}]
      "circles" (map (fn [row]
                       (let [color (case (:cityCode row)
                                     5000 "white"
                                     4000 "red"
                                     3000 "yellow"
                                     70 "blue"
                                     "#666666")]
                         {"cx" (x-axis (:prob-m row))
                          "cy" (y-axis (-
                                        (:mean-ucomp1-om row)
                                        (:mean-ucomp1-os row)))
                          "r" 5
                          ;; (/ (sqrt (:n row))
                          ;;     2)
                          "fill" color
                          "stroke" color
                          "opacity" 0.5
                          "text" (place-desc row)}))
                     (:rows data))})))


(comment
  (let [data (filter-all-nonnil-and-nonNaN
              ($ [:prob-m :prob-n
                  :prob-o-os :prob-o-om
                  :prob-a-os :prob-a-om
                  :mean-ucomp1-os :mean-ucomp1-om
                  :n :cityCode :statAreaCode]
                 (add-coords-to-place-dataset
                  (get-measures-by-place (get-d)))))]
    (save
     ($ [:prob-m :prob-n
         :prob-o-os :prob-o-om
         :prob-a-os :prob-a-om
         :mean-ucomp1-os :mean-ucomp1-om
         :yishuv-name :desc]
        (add-column :desc
                    (map place-desc
                         (:rows data))
                    (add-column :yishuv-name
                                (map (comp from-yishuv-code-to-name
                                           (nil-to-val "other")
                                           (leave-only-nil-and-values-of-set #{3000 4000 5000 7000 70 6100 1031 2800}))
                                     ($ :cityCode data))
                                data)))
     "../client/my-scatter/scatter.csv")))


;;;;;;;;;;;

(defn median-of-number-or-numbers
  [number-or-numbers]
  (if (number? number-or-numbers)
      number-or-numbers
      (median number-or-numbers)))

(defn get-sales-summary-by-place-map
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
                                  {:medppm (median-of-number-or-numbers ($ :pricePerMeter (second place-and-data)))
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




;; (pprint (frequencies (apply concat
;;                                       (map #(map second (second %))
;;                                            (get-ppm-summary)))))

;; (let [ppm-summary ]
;;   (println (map (juxt first
;;                       (comp count second)) ppm-summary))
;;   ppm-summary)






;; (defn unified-data-by-place []
  

;;   )



(comment
  (def x
    (set
     (keys
      (get-sales-summary-by-place-map))))
  (def y
    (set
     (distinct (:rows ($ [:cityCode :statAreaCode] (get-d))))))
  (distinct
   (map identity; (comp from-yishuv-code-to-name :cityCode)
        (clojure.set/difference x y)))
  (distinct
   (map identity; (comp from-yishuv-code-to-name :cityCode)
        (clojure.set/difference y x))))


(comment
  ($join [[:a] [:a]]
         (dataset [:a :b]
                  [[1 11]
                   [2 12]
                   [3 13]])
         (dataset [:a :c]
                  [[1 21]
                   [2 22]
                   [4 24]])))

(def get-sales-summary-by-place
  (memoize (fn [years]
             (to-dataset (map #(apply conj %)
                              (get-sales-summary-by-place-map years))))))

(def get-sales-summary-by-coords
  (memoize (fn [years]
             (add-coords-to-place-dataset
              (get-sales-summary-by-place)))))

(defn sort-colnames [adataset]
  (dataset (sort (col-names adataset))
           (:rows adataset)))


(comment
  (let [summaries-by-coords (sort-colnames
                             (filter-all-nonnil-and-nonNaN
                              (get-sales-summary-by-coords (range 2006 2011))))
        margin 80
        scale 1000
        x-axis #(+ margin (* scale %))
        y-axis #(+ margin (- scale (* scale %)))
        n-columns (filter #(re-matches #"n.*" (name %))
                          (col-names summaries-by-coords))]
    (plot-by-d3
     {"h" (+ scale margin margin)
      "w" (+ scale margin margin margin)
      "lines" [{"x" (x-axis 0) "y" (y-axis 0)}
               {"x" (x-axis 1) "y" (y-axis 0)}
               {"x" (x-axis 1) "y" (y-axis 1)}
               {"x" (x-axis 0) "y" (y-axis 1)}
               {"x" (x-axis 0) "y" (y-axis 0)}
               {"x" (x-axis 1) "y" (y-axis 1)}]
      "circles" (map (fn [row]
                       (let [color (case (:cityCode row)
                                     5000 "white"
                                     4000 "red"
                                     3000 "yellow"
                                     6400 "magenta"
                                     7000 "blue"
                                     3797 "#44ee99"
                                     "#777777")]
                         {"cx" (x-axis (log (/ (:unifprice2008 row) (:unifprice2006 row))))
                          "cy" (y-axis (log (/ (:unifprice2010 row) (:unifprice2006 row))))
                          "r" (/ (sqrt (:n2006 row))
                                 5)
                          "fill" color
                          "stroke" color
                          "opacity" 0.5
                          "text" (place-desc row)}))
                     (filter (fn [row]
                               (< 10
                                  (apply min (map row n-columns))))
                             (:rows summaries-by-coords)))})))



;;;;;;;;;;;;;;;;;;;;;

(def get-join-by-coords
  (memoize (fn []
             (let [measures-by-place (get-measures-by-place (get-d))
                   summary-by-place (get-sales-summary-by-place (range 2006 2011))
                   join-by-place ($join [[:cityCode :statAreaCode]
                                         [:cityCode :statAreaCode]]
                                        summary-by-place
                                        measures-by-place)
                   join-by-coords (sort-colnames
                                   (add-coords-to-place-dataset join-by-place))]
               join-by-coords))))


(defn careful-log-ratio
  [x y]
  (if (>= 0 (min x y))
    Double/NaN
    (log-ratio x y)))

(defn logit
  [x]
  (- (log x)
     (log (- 1 x))))

(defn logits-difference
  [x y]
  (- (logit x)
     (logit y)))

(comment
  (let [data (filter-all-nonnil-and-nonNaN
              ($ [:cityCode :statAreaCode
                  :mean-ucomp1-os :mean-ucomp1-om
                  :prob-a-os :prob-a-om
                  :prob-o-os :prob-o-om
                  :unifprice2006 :unifprice2007 :unifprice2008 :unifprice2009 :unifprice2010
                  :mean-x :mean-y
                  :n2006 :n2007 :n2008 :n2009 :n2010
                  ]
                 (get-join-by-coords)))]
    (save
     (filter-all-nonnil-and-nonNaN
      (sort-colnames
       ($ [:prob-a-change
           :prob-o-change
           :ucomp1-change
           :unifprice-change-2010-2006
           :unifprice2006
           :desc
           :yishuv-name
           ]
          (add-column
           :prob-o-change
           (map logits-difference
                ($ :prob-o-om data)
                ($ :prob-o-os data))
           (add-column
            :prob-a-change
            (map logits-difference
                 ($ :prob-a-om data)
                 ($ :prob-a-os data))
            (add-column
             :ucomp1-change
             (map logits-difference
                  ($ :mean-ucomp1-om data)
                  ($ :mean-ucomp1-os data))
             (add-column
              :unifprice-change-2010-2006
              (map logits-difference
                   ($ :unifprice2010 data)
                   ($ :unifprice2006 data))
              (add-column :desc
                          (map place-desc
                               (:rows data))
                          (add-column :yishuv-name
                                      (map (comp from-yishuv-code-to-name
                                                 (nil-to-val "other")
                                                 (leave-only-nil-and-values-of-set #{3000 4000 5000 7000 70 6100 1031 2800 9000 7600 7900}))
                                           ($ :cityCode data))
                                      data)))))))))
     "../client/my-scatter/scatter.csv")))




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
                     :label (place-desc row)
                     :color (color-func vali)}))
        center {:lat (mean (filter identity (map :lat markers)))
                :lon (mean (filter identity (map :lon markers)))}
        zoom 11]
    {:center center
     :zoom zoom
     :markers markers}))

(comment
  (let [data (add-coords-to-place-dataset
              (get-sales-summary-by-place (range 2006 2011)))]
    (spit "../client/data1.json"
          (json/write-str
           (general-gen-map-data data :unifprice2006 identity probability-to-color)))
    (spit "../client/data2.json"
          (json/write-str
           (general-gen-map-data data :unifprice2010 identity probability-to-color)))))


;;;;;;;;;;;;;;;;;;;;;;;;

(defn sets-to-labels
  [sets]
  (let [idx (range (apply max
                          (map #(apply max %)
                               sets)))
        sets-vec (vec sets)]
    (map second
            (sort-by first
                     (apply concat
                            (for [i (range (count sets-vec))]
                              (let [aset (sets-vec i)]
                                (for [j aset]
                                  [(int j) (int i)]))))))))

(comment
  (let [data (filter-all-nonnil-and-nonNaN
              ($ [:unifprice2006 :unifprice2007 :unifprice2008
                  :unifprice2009 :unifprice2010
                  :cityCode :statAreaCode]
                 (get-join-by-coords)))
        clusters (som-batch-train
                  (to-matrix
                   ($ [:unifprice2006 :unifprice2007 :unifprice2008
                       :unifprice2009 :unifprice2010]
                      data)))
        num-clusters (count (:sets clusters))
        labels (sets-to-labels
                (vals (:sets clusters)))
        labelled-data (add-coords-to-place-dataset
                       (conj-cols data {:label labels}))]
    (spit "../client/data2.json"
          (json/write-str
           (general-gen-map-data labelled-data
                                 :label
                                 int
                                 ["red" "green" "blue" "cyan" "magenta" "black" "white"])))))








(comment
  (let [pre-data (filter-all-nonnil-and-nonNaN
                  ($ [
                      :prob-a-om :prob-a-os :prob-o-om :prob-o-os
                          :mean-ucomp1-om :mean-ucomp1-os
                          :unifprice2006 :unifprice2007 :unifprice2008 :unifprice2009 :unifprice2010
                          :n2006 :n2007 :n2008 :n2009 :n2010
                          :cityCode :statAreaCode
                          :mean-x :mean-y]
                         (get-join-by-coords))) 
        data (filter-all-nonnil-and-nonNaN
              ($ [:ucomp1-change
                  :prob-a-change 
                  :prob-o-change
                  :ucomp1-change
                  :unifprice-change-2007-2006
                  :unifprice-change-2008-2007
                  :unifprice-change-2009-2008
                  :unifprice-change-2010-2009
                  :n-change-2007-2006
                  :n-change-2008-2007
                  :n-change-2009-2008
                  :n-change-2010-2009
                  :cityCode :statAreaCode
                  :mean-x :mean-y]
                 (conj-cols (to-dataset
                             {:prob-a-change (map logits-difference
                                                  ($ :prob-a-om pre-data)
                                                  ($ :prob-a-os pre-data))
                              :prob-o-change (map logits-difference
                                                  ($ :prob-o-om pre-data)
                                                  ($ :prob-o-os pre-data))
                              :ucomp1-change (map logits-difference
                                                  ($ :mean-ucomp1-om pre-data)
                                                  ($ :mean-ucomp1-os pre-data))
                              :unifprice-change-2007-2006 (map logits-difference
                                                               ($ :unifprice2007 pre-data)
                                                               ($ :unifprice2006 pre-data))
                              :unifprice-change-2008-2007 (map logits-difference
                                                               ($ :unifprice2008 pre-data)
                                                               ($ :unifprice2007 pre-data))
                              :unifprice-change-2009-2008 (map logits-difference
                                                               ($ :unifprice2009 pre-data)
                                                               ($ :unifprice2008 pre-data))
                              :unifprice-change-2010-2009 (map logits-difference
                                                               ($ :unifprice2010 pre-data)
                                                               ($ :unifprice2009 pre-data))
                              :n-change-2007-2006 (map log-ratio
                                                ($ :n2007 pre-data)
                                                ($ :n2006 pre-data))
                              :n-change-2008-2007 (map log-ratio
                                                ($ :n2008 pre-data)
                                                ($ :n2007 pre-data))
                              :n-change-2009-2008 (map log-ratio
                                                ($ :n2009 pre-data)
                                                ($ :n2008 pre-data))
                              :n-change-2010-2009 (map log-ratio
                                                ($ :n2010 pre-data)
                                                ($ :n2009 pre-data))})
                            pre-data)))
        clusters (som-batch-train
                  (to-matrix
                   ($ [:ucomp1-change
                       :prob-a-change 
                       :prob-o-change 
                       :unifprice-change-2007-2006
                       :unifprice-change-2008-2007
                       :unifprice-change-2009-2008
                       :unifprice-change-2010-2009
                       ;; :n-change-2007-2006
                       ;; :n-change-2008-2007
                       ;; :n-change-2009-2008
                       ;; :n-change-2010-2009
                       ]
                      data)))
        num-clusters (count (:sets clusters))
        labels (sets-to-labels
                (vals (:sets clusters)))
        labelled-data (conj-cols data {:label labels})]
    (spit "../client/data2.json"
          (json/write-str
           (general-gen-map-data labelled-data
                                 :label
                                 int
                                 ["red" "green" "blue" "cyan" "magenta" "black" "white"])))))


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



(comment
  (let [pre-data (filter-all-nonnil-and-nonNaN
                  ($ [
                      :prob-a-om :prob-a-os :prob-o-om :prob-o-os
                      :mean-ucomp1-om :mean-ucomp1-os
                      :unifprice2006 :unifprice2007 :unifprice2008 :unifprice2009 :unifprice2010
                      :n2006 :n2007 :n2008 :n2009 :n2010
                      :cityCode :statAreaCode
                      :mean-x :mean-y]
                     (get-join-by-coords))) 
        data (filter-all-nonnil-and-nonNaN
              ($ [:ucomp1-change
                  :prob-a-change 
                  :prob-o-change
                  :ucomp1-change
                  :unifprice-change-2007-2006
                  :unifprice-change-2008-2007
                  :unifprice-change-2009-2008
                  :unifprice-change-2010-2009
                  :n-change-2007-2006
                  :n-change-2008-2007
                  :n-change-2009-2008
                  :n-change-2010-2009
                  :cityCode :statAreaCode
                  :mean-x :mean-y]
                 (conj-cols (to-dataset
                             {:prob-a-change (map logits-difference
                                                  ($ :prob-a-om pre-data)
                                                  ($ :prob-a-os pre-data))
                              :prob-o-change (map logits-difference
                                                  ($ :prob-o-om pre-data)
                                                  ($ :prob-o-os pre-data))
                              :ucomp1-change (map logits-difference
                                                  ($ :mean-ucomp1-om pre-data)
                                                  ($ :mean-ucomp1-os pre-data))
                              :unifprice-change-2007-2006 (map logits-difference
                                                               ($ :unifprice2007 pre-data)
                                                               ($ :unifprice2006 pre-data))
                              :unifprice-change-2008-2007 (map logits-difference
                                                               ($ :unifprice2008 pre-data)
                                                               ($ :unifprice2007 pre-data))
                              :unifprice-change-2009-2008 (map logits-difference
                                                               ($ :unifprice2009 pre-data)
                                                               ($ :unifprice2008 pre-data))
                              :unifprice-change-2010-2009 (map logits-difference
                                                               ($ :unifprice2010 pre-data)
                                                               ($ :unifprice2009 pre-data))
                              :n-change-2007-2006 (map log-ratio
                                                       ($ :n2007 pre-data)
                                                       ($ :n2006 pre-data))
                              :n-change-2008-2007 (map log-ratio
                                                       ($ :n2008 pre-data)
                                                       ($ :n2007 pre-data))
                              :n-change-2009-2008 (map log-ratio
                                                       ($ :n2009 pre-data)
                                                       ($ :n2008 pre-data))
                              :n-change-2010-2009 (map log-ratio
                                                       ($ :n2010 pre-data)
                                                       ($ :n2009 pre-data))})
                            pre-data)))
        clusterer (make-clusterer :k-means
                                  :number-clusters 3
                                  :number-iterations 10000)
        weka-dataset (incanter-dataset-to-weka-dataset :data
                                                       ($ [:ucomp1-change
                                                           ;; :prob-a-change 
                                                           ;; :prob-o-change 
                                                           :unifprice-change-2007-2006
                                                           :unifprice-change-2008-2007
                                                           :unifprice-change-2009-2008
                                                           :unifprice-change-2010-2009
                                                           ;; :n-change-2007-2006
                                                           ;; :n-change-2008-2007
                                                           ;; :n-change-2009-2008
                                                           ;; :n-change-2010-2009
                                                           ]
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



  (defn random-color
    []
    (format "#%06X"
            (rand-int 16777216)))


(comment
  (let [pre-data (filter-all-nonnil-and-nonNaN
                          ($ [
                              :prob-a :prob-a-om :prob-a-os
                              :prob-o :prob-o-om :prob-o-os
                              :mean-ucomp1-om :mean-ucomp1-os
                              :unifprice2006 :unifprice2007 :unifprice2008 :unifprice2009 :unifprice2010
                              :n2006 :n2007 :n2008 :n2009 :n2010
                              :cityCode :statAreaCode
                              :mean-x :mean-y]
                             (get-join-by-coords)))
        data pre-data
        cluster-to-json
        (fn [colnames filename]
          (let [ 
                clusterer (make-clusterer :expectation-maximization
                                          :number-clusters 10)
                weka-dataset (incanter-dataset-to-weka-dataset
                              :data ($ colnames data))
                _ (clusterer-build clusterer
                                   weka-dataset)
                labels (map (comp parse-int-or-nil name :class instance-to-map)
                            (clusterer-cluster clusterer
                                               weka-dataset))
                _ (println (frequencies labels))
                labelled-data (conj-cols data {:label labels})
                num-classes (count (distinct labels))] 
            (spit filename
                  (json/write-str
                   (general-gen-map-data labelled-data
                                         :label
                                         int
                                         (vec (repeatedly num-classes random-color))
                                         )))))]
    (cluster-to-json [:prob-a-os :prob-o-os :mean-ucomp1-os]
                     "../client/data1.json")
    (cluster-to-json [:prob-a :prob-o :mean-ucomp1]
                     "../client/data2.json")))



  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
