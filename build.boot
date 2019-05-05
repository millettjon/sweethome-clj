;; See: https://github.com/boot-clj/boot/wiki/Boot-Environment
(set-env!
 :source-paths   #{"src"}
 :resource-paths #{"resources"}
 :dependencies `[
                 [org.clojure/clojure ~(clojure-version)]
                 [org.clojure/core.incubator "0.1.4"]
                 [ch.qos.logback/logback-classic "1.2.3"]
                 [org.clojure/tools.nrepl "0.2.13"]
                 [adzerk/boot-test "1.2.0" :scope "test"]

                 ;; Command to install to local maven repo:
                 ;; See:http://corfield.org/blog/2017/11/17/boot-localrepo/
                 ;; boot -d seancorfield/boot-localrepo install-artifact -f $SWEETHOME3D_JAR -P sweethome3d/sweethome3d -v 5.4
                 [sweethome3d/sweethome3d "5.4" :scope "provided"]
                 ])

(require '[adzerk.boot-test :refer :all])

;; boot javac watch speak test
(deftask dev
  []
  (comp
   (javac)
   (watch)
   (speak)
   (test)))

(deftask build
  "Builds this project as a SweetHome3D plugin."
  []
  (comp
   (javac)
   (uber :exclude-scope #{"provided"})
   (sift :to-resource #{#"jam/sweethome3d.*\.clj"})
   (jar  :file "nrepl.jar")
   (sift :include #{#"nrepl.jar"})
   (target)))
