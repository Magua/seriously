(defproject seriously "0.0.1-SNAPSHOT"
  :description "Clojure tests"
  :license {}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [http-kit "2.1.18"]
                 [compojure "1.4.0"]
                 [org.clojure/data.json "0.2.6"]]
  :main ^:skip-aot seriously.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
