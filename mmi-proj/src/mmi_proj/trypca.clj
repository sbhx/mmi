(comment
  (do
    (require 'mmi-proj.trypca :reload-all)
    (in-ns 'mmi-proj.trypca))
  (defn reload []
    (require 'mmi-proj.trypca :reload-all)))

(ns mmi-proj.trypca
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


(defn mmt [m]
  (clx/* m (clx/t m)))

(defn mtm [m]
  (clx/* (clx/t m) m))

(defn center [vals]
  (map -
       vals
       (repeat (mean vals))))

(binding [gen/*rnd* (java.util.Random. 1)]
  (let [n 1000
        x (repeatedly n gen/float)
        y (repeatedly n gen/float)
        z (map + x y (repeatedly n gen/float))
        data (bind-columns x y z)
        m (ncol data)
        centered-data (apply bind-columns
                             (for [i (range m)]
                               (matrix (center ($ i data)))))
        pca (principal-components centered-data)
        rotation (:rotation pca)
        rotated-data (clx/* centered-data
                            rotation)
        svd (clx/svd centered-data)
        svded-data (clx/* centered-data
                          (:right svd))]
    [;;(mmt rotation)
     ;;(mmt (clx/t rotated-data))
     (correlation rotated-data)
     (correlation svded-data)
     ]))
