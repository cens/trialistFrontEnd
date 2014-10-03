(defproject trialist "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [cljs-ajax "0.2.3"]
                 [compojure "1.1.5"]
                 [hiccup "1.0.4"]
                 [garden "1.1.4"]
                 [org.clojure/clojurescript "0.0-2030"]
                 [jayq "2.5.0"]
                 [hickory "0.5.3"]
                 [prismatic/dommy "0.1.2"]]
  :plugins [[lein-ring "0.8.5"]
            [lein-cljsbuild "1.0.0"]]
  :ring {:handler trialist.core/handler}
  :profiles {:dev {:dependencies [[ring-mock "0.1.5"]]}}
  :cljsbuild {
    :builds [{
        :notify-command ["/Users/maku/Projects/trialist-front-end/trialist-cljs/sync"]
        ; ClojureScript source directory:
        :source-paths ["src-cljs"]
        ; The standard ClojureScript compiler options:
        ; (See the ClojureScript compiler documentation for details.)
        :compiler {
          :externs ["resources/externs.js"]
          :output-to "resources/public/js/main.js"  ; default: target/cljsbuild-main.js
          :optimizations :simple
          ;:optimizations :whitespace
          ;:optimizations :advanced
          :pretty-print true
          ;:pretty-print false
}}]})
