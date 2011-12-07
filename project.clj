(defproject cl-svm "1.0"
  :description "libsvm wrapper"
  :dependencies [[org.clojure/clojure "1.3.0"]
								 [org.clojure/clojure-contrib "1.2.0"]
                 [tw.edu.ntu.csie/libsvm "3.1"]]
	:dev-dependencies [[org.clojars.weavejester/autodoc "0.9.0"]]
	:main cl-svm.bow
	:autodoc { :name "cl-svm", :page-title "cl-svm API Documentation"})

