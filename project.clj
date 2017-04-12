(defproject me.dryewo/send-more-money "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [midje "1.8.3"]
                 [loco "0.3.1"]
                 [instaparse "1.4.1"]]
  :main ^:skip-aot me.dryewo.send-more-money
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev     {:source-paths ["dev"]
                       :repl-options {:init-ns user}
                       :plugins      [[lein-midje "3.2"]]
                       :dependencies [[org.clojure/tools.namespace "0.2.11"]
                                      [org.clojure/java.classpath "0.2.3"]
                                      [criterium "0.4.4"]]}})