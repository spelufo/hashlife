(defproject canvas-hashlifes "0.1.0"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2030"]]
  :plugins [[lein-cljsbuild "1.0.0"]]
  :cljsbuild
  {:builds
   [{:id "dev" ;; Google Closure only merges it and minifies it
     :source-paths ["src/"]
     :compiler {:optimizations :whitespace
                :pretty-print true
                :output-dir "out/dev" ;; ClojureScript compiler output
                :output-to "out/dev/hashlife.js"
                :source-map "out/dev/hashlife.js.map"
                }}
    {:id "adv" ;; Passes through full blown Google Closure optimizations
     :source-paths ["src/"]
     :compiler {:optimizations :advanced
                :output-dir "out/adv" ;; ClojureScript compiler output
                :output-to "out/adv/hashlife.js"
                :source-map "out/adv/hashlife.js.map"
                }}]})

;; ClojureScript Up and Running
;; https://github.com/emezeske/lein-cljsbuild/blob/master/example-projects/advanced/project.clj
