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

(def panel (ChartPanel.
            ^JFreeChart
            (scatter-plot nil nil)))
(nuroko.gui.visual/show panel)
(defn show-chart [chart]
  (do (.setChart panel ^JFreeChart chart)
      (.repaint ^JComponent panel)))


(defn approximately-equal? [x y]
  (< x (* 1.1 y)
     ))

(defn remove-leading-zeros [s]
  (clojure.string/replace s #"^0+" ""))

(defn extract-day-month-year [date-string]
  (map read-string
       (map remove-leading-zeros
            (clojure.string/split date-string #"/"))))


(defn convert-date-string-to-datetime [date-string]
  ;; TODO: Coult I just use clj-time.format/parse as in Clojure Data Analysis Cookbook?
  (let [[day month year] (extract-day-month-year date-string)]
    (t/date-time year month day)))

(defn convert-date-string-to-datetime-of-month [date-string]
  (let [[day month year] (extract-day-month-year date-string)]
    (t/date-time year month)))

(defn and-func [x y]
  (and x y))

(defn filter-all-nonnil [adataset]
  (to-dataset
   (filter #(reduce and-func
                    (vals %))
           (:rows adataset))))


(defn freqs-as-rows [x]
  (map #(hash-map :val (first %) :count (second %))
       (sort-by (comp  - second) (frequencies x))))

(defn print-freqs [x]
  (print-table (freqs-as-rows x)))

(def dataset-filename
  "/home/we/workspace/data/dataset.Thu_Sep_19_16_42_10_IST_2013.csv")

(def d (let [data-from-file (read-dataset
                             dataset-filename
                              :header true)]
         (conj-cols data-from-file
                    (dataset [:date :month :year-as-number] ($map
                                                             (juxt convert-date-string-to-datetime
                                                                   convert-date-string-to-datetime-of-month
                                                                   (comp #(nth % 2)
                                                                         extract-day-month-year))
                                                             :o-תאריך-החלטה-o data-from-file)))))

(pprint {:dim (dim d)
         :col-names (col-names d)})

(->> ($ [:id :o-גוש-o :o-חלקות-o :o-מספרי-המגרשים-לפי-התבע-o] d) :rows frequencies vals freqs-as-rows to-dataset)
($where {:id "עמ/635/2007"} d)
(->> ($ [:id :o-גוש-o :o-חלקות-o :o-מספרי-המגרשים-לפי-התבע-o :o-שם-הזוכה-o] d) :rows frequencies vals freqs-as-rows to-dataset)
($where {:id "בש/235/2009"} d)


(->> ($ [:o--קיבולת-ביח-ד--במגורים-בחדרים--במלונאות-o :o-הוצאות-פיתוח-o :o-הוצאות-פיתוח-ליח-ד/חדר-o] d)
                    filter-all-nonnil
                    :rows
                    (map #( / (:o-/--קיבולת-ביח-ד--במגורים-בחדרים--במלונאות-o %)
                                 (/ (:o-הוצאות-פיתוח-o %)
                                    (:o-הוצאות-פיתוח-ליח-ד/חדר-o %))))
                    (#(hash-map :min-ratio (reduce min %)
                                :max-ratio (reduce max %))))


(let [ ids (distinct ($ :id d))
      split-ids (map #(clojure.string/split % #"/")
                     ids)
      split-ids-grouped-by-year (into (sorted-map) (group-by (fn [x] (Integer/parseInt (nth x 2)))
                                                             split-ids))
      summary-by-year (fmap (fn [split-ids-this-year]
                              (let [idx (map (comp #(Integer/parseInt %)
                                                   second)
                                             split-ids-this-year)
                                    max-idx (reduce max idx)
                                    min-idx (reduce min idx)
                                    count-idx (count idx)]
                                (if (not (= count-idx
                                            (count (distinct idx))))
                                  (println {:error (take 9 split-ids-this-year)}))
                                ;; (assert (= count-idx
                                ;;            (count (distinct idx)))
                                ;;        "no repeating index value in the same year")
                                {:min min-idx
                                 :max max-idx
                                 :coverage-to-max (float (/ count-idx
                                                            max-idx))}))
                            split-ids-grouped-by-year)]
  (to-dataset (map (fn [k-v]
                     (conj {:year (first k-v)}
                           (second k-v)))
                   summary-by-year)))

(to-dataset
 (freqs-as-rows
  (map (comp #(vector (first %)
                      (last %))
             #(clojure.string/split % #"/"))
       ($ :id d))))

(->> ($ [:id :o-תאריך-החלטה-o] d)
     filter-all-nonnil
     :rows
     (map #(vector (last (clojure.string/split (:id %) #"/"))
                   (last (clojure.string/split (:o-תאריך-החלטה-o %) #"/"))))
     (filter #(not (= (first %)
                      (second %))))
     freqs-as-rows
     to-dataset
 )

(->> ($ [:id :o-תאריך-החלטה-o] d)
     filter-all-nonnil
     :rows
     (map #(vector (Integer/parseInt (last (clojure.string/split (:id %) #"/")))
                   (Integer/parseInt (last (clojure.string/split (:o-תאריך-החלטה-o %) #"/")))))
     (map #(apply - %))
     freqs-as-rows
     to-dataset
 )






(to-dataset
 (freqs-as-rows
  (map (comp first
             #(clojure.string/split % #"/"))
       ($ :id d))))


(->>
 ($ :id d)
                    distinct
(map )
(filter #(and
          (= (last %) "2006")))
(map second) (map #(Integer/parseInt %))
sort
)



;; [:o-מספר-תבע-o :o-שכונה-o :o-יעוד-o :o-מספרי-המגרשים-לפי-התבע-o :o-יעוד-מפורט-o :o-הוצאות-פיתוח-למטר-o :o-גוש-o :o-סכום-זכיה-o :o-תאריך-החלטה-o :o-הוצאות-פיתוח-למטר-מבונה-o :o-מספר-הצעות-o :o-הוצאות-פיתוח-ליחד/חדר-o :o-מחיר-שומא-o :o-שטח-במר-o :o-שטח-לבניה-במר-o :o-סטיית-תקן-o :o-מספר-מגרשים-באתר-o :o-ישוב-o :o-ממוצע-הצעות-o :o-הוצאות-פיתוח-o :o-חלקות-o :o-שם-הזוכה-o]

(def month-freqs (into (sorted-map) (frequencies
                                     (sel d :cols :month))))

(def month-freqs-dataset
  (dataset [:month :freq]
           (for [[month freq] month-freqs]
             {:month month :freq freq })))

(defn date-column-to-long [colname adataset]
  (replace-column :month
                  (map clj-time.coerce/to-long
                       (sel month-freqs-dataset :cols colname))
                  adataset))

;; (view
;;  (time-series-plot :month :freq
;;                    :data (date-column-to-long :month month-freqs-dataset)))

($rollup #(mean (filter identity %)) :o-מחיר-שומא-o :month d)

(view
 (time-series-plot :month :o-הוצאות-פיתוח-o
                   :data (date-column-to-long :month
                                              ($rollup #(mean (filter identity %)) :o-הוצאות-פיתוח-o :month d))))



(view
 (scatter-plot-matrix (to-dataset 
                               (map
                                (fn [row]
                                  {:o-סכום־זכיה־למטר־מבונה-o
                                   (/ (:o-סכום-זכיה-o row)
                                      (:o-שטח-לבניה-במר-o row))
                                   :o-הוצאות-פיתוח-למטר-מבונה-o
                                   (:o-הוצאות-פיתוח-למטר-מבונה-o row)})
                                (:rows (filter-all-nonnil
                                        ($ [:o-סכום-זכיה-o :o-שטח-לבניה-במר-o :o-הוצאות-פיתוח-למטר-מבונה-o] d)))))
                      ;;:group-by 
                      :nbins 20 ))


(view
 (scatter-plot-matrix (to-dataset 
                               (map
                                (fn [row]
                                  {:o-סכום־זכיה־למטר־מבונה-o (/ (:o-סכום-זכיה-o row)
                                                                (:o-שטח-לבניה-במר-o row))
                                   :o-מחיר־שומא־למטר־מבונה-o (/ (:o-מחיר-שומא-o row)
                                                              (:o-שטח-לבניה-במר-o row))})
                                (:rows (filter-all-nonnil
                                        ($ [:o-סכום-זכיה-o :o-שטח-לבניה-במר-o :o-מחיר-שומא-o] d)))))
                      ;;:group-by 
                      :nbins 20 ))


(frequencies (map #(map nil?
                        (vals %))
                  (:rows ($ [ :o-שטח-לבניה-במר-o :o-מחיר-שומא-o] d))))


;; :o-שטח-לבניה-במר-o is used until 2001,
;; :o-מחיר-שומא-o  is used from 2009
(->> (map
      (juxt (comp t/year convert-date-string-to-datetime :o-תאריך-החלטה-o)
            (comp nil? :o-מחיר-שומא-o)
            (comp nil? :o-שטח-לבניה-במר-o))
      (:rows d))
     freqs-as-rows
     to-dataset
     ($order :val :asc))


;; The following check is valid only for numeric fields.
(defn find-active-years-of-column [col-keyword]
  (->> (map (comp
             t/year convert-date-string-to-datetime :o-תאריך-החלטה-o)
            (filter (fn [row] (and (col-keyword row)
                                  (not (= (col-keyword row)
                                          ""))))
                    (:rows d)))
       ;;freqs-as-rows to-dataset ($order :val :asc)
       distinct))
(col-names 
 (to-dataset (seq
              (fmap #(reduce (fn [x y] (str x " " y)) (sort (find-active-years-of-column %)))
                    (apply conj (map (fn [col-name] {col-name col-name})
                                     (col-names d))))))
 [:field :active-years])


(def names-of-no-winner #{
                          ;; This set was generated by the following code:
                          ;; (->> d
                          ;;      :rows
                          ;;      (filter (comp (complement nonnil-and-pos) :o-מספר-הצעות-o))
                          ;;      (map :o-שם-הזוכה-o)
                          ;;      frequencies
                          ;;      seq
                          ;;      (map (fn [val-count]
                          ;;             (println (str "\"" (first
                          ;;             val-count)"\"   ;" (second
                          ;;             val-count) " times")))))
                          ;; and some manual filtering of resulting
                          ;; names of people/companies.
                          " "   ;10 times
                          "o  אין זוכה o"   ;3 times
                          "o  המתחם בוטל o"   ;2 times
                          "o  הצעה לא תקינה o"   ;3 times
                          "  "   ;435 times
                          "o  המציעים לא עמדו בתנאי המכרז o"   ;2 times
                          "o   המכרז בישוב זה בוטל o"   ;5 times
                          "o  למתחם לא הוגשו הצעות o"   ;1 times
                          "o  לא היו הצעות o"   ;2 times
                          "o  המציעים זכו במתחמים אחרים ולא o"   ;1 times
                          "o  לא תקין o"   ;6 times
                          "o   לא הוכרזו זוכים למגרש זה בעקבות צו ביניים o"   ;1 times
                          "o  אין הצעות למגרש זה o"   ;729 times
                          "o  אין הצעות. o"   ;3 times
                          "o  הצעה נמוכה o"   ;13 times
                          "o   אין הצעות למגרש זה o"   ;23384 times
                          "o  הועדה מחליטה לא להקצות את o"   ;2 times
                          "o  הצעות נמוכות o"   ;31 times
                          "o  הצעה פסולה o"   ;1 times
                          "o  ההצעה מתחת למחיר המינימום o"   ;2 times
                          "o   בחירת מגרש תערך במחוז o"   ;1859 times
                          "o   ההצעות נפסלו, לא צורפו אישורי מועצה כנדרש o"   ;16 times
                          "o   אין הצעות לחלופה זו o"   ;1 times
                          "o  בחירת מגרש תערך במחוז o"   ;280 times
                          "o  ההצעה היחידה שהוגשה נפסלה o"   ;1 times
                          "o  המציע זכה במגרש אחר ועפי תנאי o"   ;1 times
                          "o   הוקפא עקב צו מניעה o"   ;1 times
                          "o  לא צורפה ערבות נוספת המאפשרת o"   ;2 times
                          "o   מכרז בוטל o"   ;20 times
                          "o  אין הצעות o"   ;796 times
                          "o  המציע למגרש הנל זכה במגרש אחר o"   ;2 times
                          "o   המכרז בוטל עקב מיעוט משתתפים o"   ;65 times
                          "o  המגרש בוטל o"   ;2 times
                          "o  מכרז בוטל o"   ;141 times
                          "o  הצעה מתחת למחיר המינימום o"   ;4 times
                          "o  אין זוכה הצעה מתחת למחיר o"   ;1 times
                          })
;; TODO: Complete this list


(defn query-no-winner [] (query-dataset d {:o-שם-הזוכה-o
                                           {:$in names-of-no-winner}}))

(print-freqs ($ :o-ישוב-o (query-no-winner)))

(defn nonnil-and-pos [x]
  (if x (pos? x) false))

(print-freqs (->> d
                 :rows
                 (map (juxt (comp nonnil-and-pos :o-מספר-הצעות-o)
                            (comp (complement names-of-no-winner) :o-שם-הזוכה-o )))))


;; Making sure that the names-of-no-winner set is full.
;; TODO: This is not necessarily the correct test -- correct it.
(->> d
    :rows
    (filter (comp (complement nonnil-and-pos) :o-מספר-הצעות-o))
    (filter (comp (complement names-of-no-winner) :o-שם-הזוכה-o))
    (map :o-שם-הזוכה-o)
    frequencies
    (map (fn [val-count]
           (println (str "\"" (first val-count) "\"   ;" (second val-count) " times")))))
 

(defn find-frequent-settlements [threshold]
  (->> d
      :rows
      (map :o-ישוב-o)
      frequencies
      (filter (comp (partial <= threshold) second))
      (sort-by (comp - second))
      (map first)
      ))

(pprint
 (find-frequent-settlements 100))


(->>
 ($group-by [:o-ישוב-o] d)
 (filter (comp (partial <= 100)
               nrow))
 (map dim))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn plot [d transf]
  (->> d
       :rows
       (filter #(every? identity (vals %)))
       (map transf)
       (#(histogram % :nbins 50))
       show-chart))

(plot ($ [:o-סכום-זכיה-o] d)
      #(Math/log (first (vals %))))

(plot ($ [:o-הוצאות-פיתוח-ליחד/חדר-o] d)
      #(Math/log (first (vals %))))

(plot ($ [:o-הוצאות-פיתוח-ליחד/חדר-o :o-הוצאות-פיתוח-o ] d)
      #(Math/log (/ (:o-הוצאות-פיתוח-o % )
                    (:o-הוצאות-פיתוח-ליחד/חדר-o %))))

(plot ($ [:o-הוצאות-פיתוח-ליחד/חדר-o :o-הוצאות-פיתוח-o ]
         ($where d))
      #(Math/log (/ (:o-הוצאות-פיתוח-o % )
                    (:o-הוצאות-פיתוח-ליחד/חדר-o %))))

(->> ($ :o-יעוד-o d) freqs-as-rows to-dataset)
 
(->> ($ [:o-יעוד-o :o-יעוד-מפורט-o] d) :rows freqs-as-rows to-dataset)


(->> ($ [:o-יעוד-o :o-הוצאות-פיתוח-ליחד/חדר-o :o-הוצאות-פיתוח-o] d)
     filter-all-nonnil
     :rows
     (map (fn [row] {:log2-units (int (Math/floor (/ (Math/log (/ (:o-הוצאות-פיתוח-o row )
                                                                 (:o-הוצאות-פיתוח-ליחד/חדר-o row)))
                                                    (Math/log 2))))
                    :dest (:o-יעוד-o row)}))
     freqs-as-rows
     (map (fn [row] (conj {:count (:count row)}
                         (:val row))))
     (sort-by str)
     to-dataset )

(->> ($ [:o-ישוב-o :o-שכונה-o :o-יעוד-o :o-תאריך-החלטה-o] d)
     ($where {:o-יעוד-o "o בניה רוויה  o"})
     :rows
     (map (juxt :o-ישוב-o
                :o-שכונה-o
                :o-יעוד-o
                (comp #(nth % 2)
                      extract-day-month-year
                      :o-תאריך-החלטה-o)))
     freqs-as-rows
     (take 200)
     (map (fn [row] (concat
                    [(:count row)]
                    (:val row))))
     to-dataset
     (#(col-names %
                  [:count :o-ישוב-o :o-שכונה-o :o-יעוד-o :year]))
     ;; (let [filename (str dataset-filename ".most-frequent.csv") ]
     ;;   (println (str "writing " filename))
     ;;   (#(save % filename)))
     ;; :rows
     ;; (map :count)
     ;; (reduce +)
     )


(sh "libreoffice" "/home/we/workspace/data/dataset.Mon_Sep_16_01_03_37_IST_2013.csv.most-frequent.csv")


(->> ($ [:o-ישוב-o :o-שכונה-o :o-יעוד-o :year-as-number :o--קיבולת-ביח-ד--במגורים-בחדרים--במלונאות-o] d)
     filter-all-nonnil
     ($where {:o-יעוד-o "o בניה רוויה  o"})
     ($rollup sum
              :o--קיבולת-ביח-ד--במגורים-בחדרים--במלונאות-o
              [:o-ישוב-o
               :o-שכונה-o
               :o-יעוד-o
               :year-as-number])
     ($order :o--קיבולת-ביח-ד--במגורים-בחדרים--במלונאות-o :desc)
     (#(let [filename (str dataset-filename ".revuya-most-capacity.csv")]
          (println (str "writing " filename))
          (save % filename))))

(sh "libreoffice" "/home/we/workspace/data/dataset.Thu_Sep_19_16_42_10_IST_2013.csv.revuya-most-capacity.csv")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def neve-sharet-neighbs (into (hash-set) (filter
                                           #(re-matches  #".*נ.*וה שרת.*" %)
                                           (distinct ($ :o-שכונה-o d)))))

neve-sharet-neighbs

($where {:o-שכונה-o "o  נווה שרת צפון o"} d)


