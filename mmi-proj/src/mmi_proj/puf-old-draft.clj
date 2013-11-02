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
