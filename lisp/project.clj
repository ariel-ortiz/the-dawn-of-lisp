(defproject lisp "0.1.0"
  :description "How to write eval and apply in Clojure."
  :url "https://github.com/ariel-ortiz/the-dawn-of-lisp/"
  :license {:name "GNU General Public License v3.0"
            :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot lisp.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
