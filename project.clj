(defproject autochrome "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [hiccup "1.0.5"]
                 [org.omcljs/om "1.0.0-beta1"]
                 [garden "1.3.3"]
                 [cheshire "5.8.0"]
                 [clj-http "3.7.0"]
                 [org.clojure/tools.cli "0.3.5"]
                 [com.climate/claypoole "1.1.4"]]
  :main autochrome.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
