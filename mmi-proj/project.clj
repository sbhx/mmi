(defproject mmi-proj "0.1.0-SNAPSHOT"
  :description "getting data from mmi's site"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [incanter/incanter-core "1.5.4"]
                 [incanter/incanter-charts "1.5.4"]
                 [incanter/incanter-zoo "1.5.4"]
                 [clj-time "0.6.0"]
                 [quil "1.6.0"]
                 [com.jmeeks/clj-web-crawler "0.1.0-SNAPSHOT"]
                 [org.clojars.kjw/commons-httpclient "3.1"]
                 [org.clojure/core.logic "0.8.3"]
                 [org.clojure/math.combinatorics "0.0.4"]
                 [enlive "1.0.1"]
                 [ring "0.2.5"]
                 [net.cgrand/moustache "1.0.0-SNAPSHOT"]
                 [me.raynes/fs "1.4.4"]
                 [net.mikera/core.matrix "0.7.2"]
                 [org.clojure/data.csv "0.1.2"]
                 [clojure-csv/clojure-csv "2.0.1"]
                 [com.nuroko/nurokit "0.0.3"]
                 [org.clojure/algo.generic "0.1.1"]
                 [seesaw "1.4.3"]
                 [com.cemerick/pomegranate "0.2.0"]
                 [net.mikera/imagez "0.2.0"]
                 [org.clojure/data.json "0.2.3"]
                 [com.keminglabs/c2 "0.2.3"]
                 [hiccup "1.0.4"]
                 [cc.artifice/clj-ml "0.6.0-SNAPSHOT"]
                 [org.clojure/tools.trace "0.7.6"]
                 [clj-liblinear "0.1.0"]]
  ;;:main mmi-proj.puf
  :jvm-opts ["-Xmx6g"] )

