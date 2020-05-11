(defproject settler "0.1.0-SNAPSHOT"
  :description  "FX value date calculation library"
  :url          "https://github.com/shaolang/settler"
  :license      {:name  "Apache Software License 2.0"
                 :url   "https://www.apache.org/licenses/LICENSE-2.0/"}

  :profiles {:dev {:dependencies [[org.clojure/clojure      "1.10.0"]
                                  [org.clojure/test.check   "0.10.0"]]}}

  :repl-options {:init-ns settler.core})
