(defproject euler "0.1.0-SNAPSHOT"
  :description "Project Euler in Clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/math.numeric-tower "0.0.2"]
                 [org.clojure/core.typed "0.2.25"]]
  :core.typed {:check [euler.utils]})
