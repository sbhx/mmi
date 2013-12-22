(defproject simple "0.0.1-SNAPSHOT"
  :description "simple"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [net.drib/strokes "0.5.0"]
                 ;[net.drib/blade "0.1.0"]
                 ;[net.drib/mrhyde "0.5.3"]
                 ]
  :plugins [[lein-cljsbuild "1.0.1"]]
  :min-lein-version "2.0.0"

  :source-paths ["src/clj" "src/cljs"]

  :cljsbuild {:builds [{:source-paths ["src/cljs"]
                        :compiler {:output-to "public/out/simple.js"
                                   :pretty-print true 
                                   :optimizations :simple}}]})
