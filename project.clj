(defproject timesheet "0.1.0"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 ;; Compojure - A basic routing library
                 [compojure "1.6.1"]
                 ;; Our Http Library for client/server
                 [http-kit "2.3.0"]
                 ;; Ring defaults - for query params etc
                 [ring/ring-defaults "0.3.2"]
                 ;; Clojure JSON Library
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/tools.namespace "1.1.0"]]
  :main timesheet.core
  :plugins [[cider/cider-nrepl "0.25.3"]]
  :target-path "target/%s"
  :jvm-opts ["-Dclojure.server.repl={:port 5555 :accept clojure.core.server/repl}"] ; notice that the map is not quoted.
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
