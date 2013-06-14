(ns mmi-proj.core
  (:require quil.core
            quil.helpers.drawing
            quil.helpers.seqs
            )
  (:require [net.cgrand.enlive-html :as html])
  (:require [clojure.string :as string])
  (:require clojure.pprint)
  (:require clojure.inspector))

(use '(incanter core stats charts io))
(require 'clojure.core.matrix)
(require 'clojure.core.matrix.operators)
(use '[clojure.java.shell :only [sh]])
(import [java.net URL])


;;;;;;;;
;; Data scraping functions

(defn construct-query-url [query]
  (str
   "http://www.mmi.gov.il/tozaotmichrazim/Results.aspx?x=1&"
   "hdnSearchdrpYeshuv=&"
   "hdnSearchdrpMerchav=" (query :merhav) "&"
   "hdnSearchdrpYeud=" (query :yeud) "&"
   "Nnmichraz=&"
   "Ynmichraz=&"
   "Anmichraz=&"
   "drpYeshuv=" (query :yeshuv) "&"
   "TxtTaarich=" (query :from-date) "&"
   "txtAdTaarich=" (query :to-date) "&"
   "ImageButton1.x=27&"
   "ImageButton1.y=15&"
   "NewSearch=true&"
   ))

;; See
;; https://groups.google.com/forum/#!msg/enlive-clj/kl-xVdGRkGQ/S_P0OIIRnJ8J
;; TODO: Learn http://kotka.de/blog/2010/03/memoize_done_right.html
;; TODO: Is it ok for parallelized use?
(def fetch-url
  (memoize (fn  [url]
             (-> url
                 java.net.URL.
                 .getContent
                 (java.io.InputStreamReader. "Windows-1255")
                 html/html-resource
                 ))))


(defn get-results-urls [query]
  (map (fn [s] (str "http://www.mmi.gov.il/tozaotmichrazim/" s))
       (filter (fn [s] (= (subs s 0 7) "results"))
               (map :href (map :attrs
                               (html/select
                                (fetch-url (construct-query-url query))
                                [(html/attr? :href) ]))))))


(defn construct-filename-for-query [query]
  (str
   "/home/we/workspace/data/urls/"
   (clojure.string/replace (apply str query) #"\[|\]|:|\"| " "")
   ".clj")
  )

(defn get-and-save-or-load-results-urls [query]
  (let [filename (construct-filename-for-query query)]
    (if (not (.exists (java.io.File. filename)))
      (let [results-urls (get-results-urls query)]
        (spit
         filename
         (str "'" (pr-str {:query query :results-urls results-urls})))
        ;;(println (str "wrote " (count results-urls) " urls to" filename))
        results-urls)
      (do
        ;;(println (str "already exists: " filename))
        (:results-urls (load-file filename))))))

(defn construct-day-yeshuv-query [date yeshuv]
  {:from-date date :to-date date :yeshuv yeshuv})

(defn construct-day-query [date]
  {:from-date date :to-date date})

(defn construct-dates []
  (for [y (range 1998 2013)
         m (range 1 13)
         d (range 1 32)
         ]
    (str d "-" m "-" y)))

(defn construct-queries []
  (for [date (construct-dates)
        yeshuv [nil 3000 4000 5000]]
    (if (nil? yeshuv)
      (construct-day-query date)
      (construct-day-yeshuv-query date yeshuv))))


(defn get-tables-from-results-url
  [url]
  (let [html (fetch-url url)]
    (html/select
     html
     [ (html/attr= :class "tblSmallText tblLowLines")])))

(defn get-regular-data-from-table-or-tables [table-or-tables]
  ;; Note that it works when the input is a seq of more than one
  ;; table.
  ;; TODO: Understend precisely how it works.
  (map #(map html/text %)
       (map #(html/select % [[:td
                              (html/but (html/has-class "lightGrey"))]])
            (html/select table-or-tables [:tr]))))


(defn get-all-data-from-table-or-tables [table-or-tables]
  ;; Note that it works when the input is a seq of more than one
  ;; table.
  ;; TODO: Understend precisely how it works.
  (map #(map html/text %)
       (map #(html/select % [[:td]])
            (html/select table-or-tables [:tr]))))


(defn string-to-keyword
  "This function takes a string and returns a normalized keyword."
  [input]
  (-> input
      string/lower-case
      (string/replace \space \-)
      keyword))

(defn remove-quotes-tabs-and-newlines [s]
  (clojure.string/replace
   (clojure.string/replace
   s
   #"[\"|#]" "")
   #"[\t|\n]" ""))

(defn is-legitimate-pair? [k v]
  (and
   (not (or (= k "o   o") (= v "o   o"))) ;; niether k nor v is underscore
   (re-matches #"o [אבגדהוזחטיכלמנסעפצקרשת].* o" k) ;; k has a Hebrew letter
   ))

(defn make-even-clean-and-make-keys-keywords [string-cleaner todo & already-done]
  (if (> (length todo) 1) ; a remainder from division into pairs
                                        ; would be neglected
    (let [ rtodo (rest todo)]
      (recur
       string-cleaner
       (rest rtodo)
       (concat
        already-done
        (if (is-legitimate-pair? (first todo) (first rtodo))
          ;; add to already-done the cleaned and keyworded pair
          [(string-to-keyword (string-cleaner (first todo)))
           (string-cleaner (first rtodo))
           ]
          ;; add nothing
          []
          ))))
    already-done))

(defn put-Latin-around-Hebrew [s]
  (if (re-matches #".*[אבגדהוזחטיכלמנסעפצקרשת].*" s)
    (str "o " s " o")
    s))

(defn prepare-for-map [strings]
  (->> strings
       reverse
       (map put-Latin-around-Hebrew)
       (make-even-clean-and-make-keys-keywords remove-quotes-tabs-and-newlines)
       ))

(defn prepare-for-map2 [strings]
  (->> strings
       reverse
       (map put-Latin-around-Hebrew)
       ))

(defn construct-map1-of-results [results-url]
  (->> results-url
       get-tables-from-results-url
       get-regular-data-from-table-or-tables
       (map prepare-for-map)
       (reduce concat)
       (apply hash-map)))

(defn construct-map2-of-results [results-url]
  (let [parts (->> results-url
                   get-tables-from-results-url
                   get-all-data-from-table-or-tables
                   (map prepare-for-map2)
                   vec
                   )
        gush-idx (first (filter
                         #(= (second ( parts %)) "o גוש o")
                         (range (count parts))))
        gush-data (-> gush-idx inc parts rest)
        ]
    {:o-גוש-o (first gush-data)
     :o-חלקות-o (second gush-data)}
    ))

(defn construct-map-of-results [results-url]
  (do
    (println results-url)
    (-> (sorted-map)
        (into (construct-map1-of-results results-url))
        (into (construct-map2-of-results results-url))
        )))

(defn construct-filename-for-results-url [results-url]
  (str
   "/home/we/workspace/data/maps/"
   (clojure.string/replace results-url #"[/|:|\?|\&]" ".")
   ".clj"))

;; TODO: Generalize this fn and the very similar one above.
(defn get-and-save-or-load-map-of-results-urls [results-url]
  (let [filename (construct-filename-for-results-url results-url)]
    (if (not (.exists (java.io.File. filename)))
      (let [map-of-results (construct-map-of-results results-url)]
        (spit
         filename
         (str "'" (pr-str {:results-url results-url :map-of-results map-of-results})))
        (println (str "wrote to " filename))
        map-of-results)
      (do
        (println (str "already exists: " filename))
        (:map-of-results (load-file filename))))))


(defn -main []
  (let [ results-urls
        (flatten
         (doall ( pmap
                  get-and-save-or-load-results-urls 
                  (construct-queries))))]
    (doall (pmap
            get-and-save-or-load-map-of-results-urls
            results-urls))))




;; (defn organize-results-maps-by-url [query]
;;   (->>
;;    (for [results-url (get-results-urls query)]
;;      [results-url (construct-map-of-results results-url)])
;;    flatten
;;    (apply hash-map)
;;    ))


;; (defn get-and-save-results-map [query]
;;   (spit
;;    (construct-filename-for-query query)
;;    (pr-str {:query query
;;             :results-map
;;             })))





;; (defn construct-and-save-map-of-results [results-url]
;;   (do
;;     (println results-url)
;;     (let [results-map (-> (sorted-map)
;;                           (into (construct-map1-of-results results-url))
;;                           (into (construct-map2-of-results results-url))
;;                           )
;;           outfilename (str "/home/we/projects/proj1/data/results-maps/" (clojure.string/replace url #":|\?|\&|/" "-") ".clj")]
;;       (if (.exists (java.io.File. outfilename))
        

;;         )
;;       (spit outfilename results-map)
;;       (println (str "saved results map for " url " to " outfilename))
;;       results-map)))

;; (defn construct-map-of-results [query results-url]
;;   (do
;;     (println results-url)
;;     (-> (sorted-map)
;;         (into (construct-map1-of-results results-url))
;;         (into (construct-map2-of-results results-url))
;;         (into query)
;;         )))

;; (defn construct-and-save-maps-of-results [query]
;;   (let [ maps (->> query
;;                    get-results-urls
;;                    (map #(construct-map-of-results query %))
;;                    doall)
;;         outfilename (str
;;                      "/home/we/projects/proj1/data/maps-of-results."
;;                      (hash query)
;;                      (System/currentTimeMillis)
;;                      ".clj")]
;;     (spit outfilename (str "'" (pr-str maps)))
;;     (println (str "saved results for " query " to " outfilename))
;;     maps))


;; (def results-col-names
;;   (->> {:yeshuv 5000}
;;        get-results-urls
;;                                         ;(take 10)
;;        (map #(construct-map-of-results
;;               {}
;;               ;; {:yeshuv 5000
;;               ;;  :yeud 2
;;               ;;  :from-date 2003
;;               ;;  :to-date 2012}
;;               ;; ;; Note that
;;               ;; ;; the role of this query here
;;               ;; ;; is just to supply column names.
;;               %))
;;        (map keys)
;;        (map #(apply hash-set %))
;;        (reduce clojure.set/union)
;;        sort
;;        (concat [:yeshuv :yeud :from-date :to-date])
;;        ))

;; (defn construct-dataset-for-query [query]
;;   (->> query
;;        get-results-urls
;;        (map #(construct-map-of-results query %))
;;        (dataset results-col-names)))

;; (defn construct-and-save-dataset-for-query [query]
  
;;   (->> query
;;        get-results-urls
;;        (map #(construct-map-of-results query %))
;;        (dataset results-col-names)))

;;  (defn string-to-int [s]
;;   (if (seq s)
;;     (Integer. s)
;;     nil))

;; (def numeric-columns
;;   [:o-הוצאות-פיתוח-o
;;    :o-הוצאות-פיתוח-למטר-o
;;    :o-הוצאות-פיתוח-למטר-מבונה-o
;;    :o-הוצאות-פיתוח-ליחד/חדר-o
;;    :o-שטח-במר-o
;;    :o-שטח-לבניה-במר-o
;;    :o-סכום-זכיה-o
;;    :o-מחיר-שומא-o
;;    :o-מספר-הצעות-o
;;    :o-ממוצע-הצעות-o
;;    :o-סטיית-תקן-o
;;    :o-מספר-מגרשים-באתר-o
;;    ])

;; (defn clean-numeric-string [s]
;;   (apply str (filter #(Character/isDigit %) s)))

;; (defn numeric-string-to-int [s]
;;   (string-to-int (clean-numeric-string s)))

;; ;; (defn numeric-string-to-int [s]
;; ;;   (->> (clojure.string/replace s "," "")
;; ;;        seq
;; ;;        rest ;; remove strange first character
;; ;;        (apply str)
;; ;;        string-to-int
;; ;;        ))

;; (defn clean-numeric-columns [d numeric-columns]
;;   (if (seq numeric-columns)
;;     (recur
;;      (transform-col d (first numeric-columns) numeric-string-to-int)
;;      (rest numeric-columns))
;;     d))


;; (def queries
;;   (for [yeshuv-code [3000 4000 5000
;;                              ]
;;                 to-year [2006 2007 2008 2009
;;                          ]]
;;     {
;;      :yeshuv-code yeshuv-code
;;      ;;:yeud 2
;;      :from-date (dec to-year)
;;      :to-date to-year}
;;     )
;;   )

;; (def datasets
;;   (into {} (for [query queries]
;;              (do
;;                (println query)
;;                [query (construct-dataset-for-query query)]
;;                ))))

;; (def clean-datasets
;;   (apply hash-map (flatten
;;                    (map (fn [[ query d]]
;;                           [query (clean-numeric-columns d numeric-columns)])
;;                          (seq datasets)))))

;; (map dim (vals clean-datasets))


;; (for [ [q d] clean-datasets]
;;   [q (sel d :rows (range 9))])


;; (pprint
;;  (map (fn [name] [name ($ 1 name d)])
;;       results-col-names))


;; (view (sel
;;        d
;;        :rows (range 9)))

;; (view (histogram
;;        (filter #(not ( nil? %)) ($ :o-סכום-זכיה-o  d))
;;        :nbins 100))

;; (save d "/home/we/results.tsv.csv" :delim \tab)
;; (view d)

;; (view (let [ xy (sel d :cols [:o-מחיר-שומא-o :o-סכום-זכיה-o])]
;;                     (xy-plot
;;                      (sel xy :cols 0)
;;                      (sel xy :cols 1)
;;                      :points true) ))

;; (->> ($ :o-יעוד-o d)
;;      frequencies
;;      (sort-by second)
;;      pprint
;;      )


;; ;;;;;;;;; draft




;; ;; (map println (binding [*print-dup* true] (map prn-str
;; ;;                                               {:a 2})))


;; (clojure.java.shell/sh "firefox" (construct-query-url {:yeshuv-code 5000 :from-date "1/7/2007" :to-date  "1/1/2009"}))


;; (map println (get-results-urls  {:yeshuv-code 5000 :from-date "1/7/2007" :to-date  "1/1/2009"}))



;; (map (fn [_] (.size (into () _))) (.values results) )

;; (def results-page (first (first (.values results-links))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

