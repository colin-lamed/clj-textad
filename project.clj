(defproject clj-textad "0.1.0-SNAPSHOT"
  :description "Text Adventure"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [io.aviso/pretty "0.1.12"]
                 [org.clojure/core.incubator "0.1.3"]]
  :aot :all
  :main textad.main)
