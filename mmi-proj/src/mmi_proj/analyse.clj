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
(use '(incanter core stats charts io))
(require 'clojure.core.matrix)
(require 'clojure.core.matrix.operators)
(use '[clojure.java.shell :only [sh]])
(use 'clojure.pprint)
(import [java.net URL])
(require 'clojure.data.csv)
(require 'clojure.reflect)


(def d (read-dataset
        "/home/we/projects/mmi/mmi-proj/data/dataset.Tue_Jun_18_23_02_43_IDT_2013.csv"
        :header true))

(defn remove-leading-zeros [s]
  (clojure.string/replace s #"^0+" ""))

(defn extract-day-month-year [date-string]
  (map read-string
       (map remove-leading-zeros
            (clojure.string/split date-string #"/"))))

(defn convert-date-string-to-datetime [date-string]
  (let [[day month year] (extract-day-month-year date-string)]
    (t/date-time year month day)))

(defn convert-date-string-to-datetime-of-month [date-string]
  (let [[day month year] (extract-day-month-year date-string)]
    (t/date-time year month)))


(pprint {:dim (dim d)
         :col-names (col-names d)})

;; [:o-מספר-תבע-o :o-שכונה-o :o-יעוד-o :o-מספרי-המגרשים-לפי-התבע-o :o-יעוד-מפורט-o :o-הוצאות-פיתוח-למטר-o :o-גוש-o :o-סכום-זכיה-o :o-תאריך-החלטה-o :o-הוצאות-פיתוח-למטר-מבונה-o :o-מספר-הצעות-o :o-הוצאות-פיתוח-ליחד/חדר-o :o-מחיר-שומא-o :o-שטח-במר-o :o-שטח-לבניה-במר-o :o-סטיית-תקן-o :o-מספר-מגרשים-באתר-o :o-ישוב-o :o-ממוצע-הצעות-o :o-הוצאות-פיתוח-o :o-חלקות-o :o-שם-הזוכה-o]

(def month-freqs (into (sorted-map) (frequencies
                                     (map convert-date-string-to-datetime-of-month
                                          (filter #(not (= "12/06/2013" %))
                                                  (sel d :cols :o-תאריך-החלטה-o))))))

(def month-freqs-dataset
  (dataset [:month :freq]
           (for [[month freq] month-freqs]
             {:month month :freq freq })))

(view
 (time-series-plot :month :freq
          :data (replace-column :month
                                (map clj-time.coerce/to-long
                                     (sel month-freqs-dataset :cols :month))
                                month-freqs-dataset)))


(pprint
 (into (sorted-map) (frequencies
                     (sel d :cols :o-ישוב-o))))


(sel d :cols [:o-שם-הזוכה-o])

(head
 (query-dataset d {:o-שם-הזוכה-o
                   "o   אין הצעות למגרש זה o"}))


(defn print-freqs [d col-name]
  (print-table (map #(hash-map :val (first %) :count (second %))
                    (tail (sort-by second (frequencies (sel d
                                                            :cols col-name)))))))


(def names-of-no-winner #{
                          "o  אחים חסיד חברה קבלנית לבניה בעמ   o"
                          "o  יוסי אברהמי עבודות הנדסה אזרחית בעמ   o"
                          "o  אין הצעות למגרש זה o"
                          "o  אין הצעות o"
                          "o   הצעות לא תקינות o"
                          "o   המציע זכה במגרש אחר o"
                          "o   בחירת מגרש תערך במחוז o"
                          "o   הצעות נמוכות o"
                          "o   אין הצעות למגרש זה o"})


(defn query-no-winner [] (query-dataset d {:o-שם-הזוכה-o
                                           {:$in names-of-no-winner}}))

(print-freqs (query-no-winner)
             :o-ישוב-o)

(print-freqs d :o-ישוב-o)


(print-freqs d :o-שם-הזוכה-o)
