(ns simple.utils)

;; ;; https://gist.github.com/lynaghk/1141054
;; (defn jsArr
;;   "Recursively converts a sequential object into a JavaScript array"
;;   [seq]
;;   (.array (vec (map #(if (sequential? %) (jsArr %) %)
;;                     seq))))
;; (defn jsObj
;;   "Convert a clojure map into a JavaScript object"
;;   [obj]
;;   (.strobj (into {} (map (fn [[k v]]
;;                            (let [k (if (keyword? k) (name k) k)
;;                                  v (if (keyword? v) (name v) v)]
;;                              (if (map? v)
;;                                [k (jsObj v)]
;;                                [k v])))
;;                          obj))))
