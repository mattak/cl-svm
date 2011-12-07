(ns cl-svm.bow
  (:gen-class)
  (:use [ma3.io] [ma3.util] [cl-svm.core])
  (:use [clojure.java.shell]))

(defn -main
  []
  (prn "please use with lepl"))

;; bag of words wrap
;; INSTALL
;;   git clone http://github.com/mattak/bow
;;   cd bow
;;   make && make localbin
;;-----------------------------------------

(defn sh-out
  "get shell output result"
  [& args]
  (:out (apply sh args)))

(defn sh-put
  "print shell output stream"
  [& args]
  (print (apply sh-out args)))

(defn desc
  "extract descriptor"
  [type imgfile savefile]
  (sh-put "desc" imgfile savefile "-t" type "-g"))

(defn desc-all
  "desc for all imgfiles and join to savefile"
  [type imgfiles savefile]
  (let [namefiles (map #(str (file-head %) "." type) imgfiles)]
    (let [thr (Thread. (fn [] (map #(desc type %1 %2) imgfiles namefiles)))]
      (.start thr)
      (.join thr)
      ;(apply file-join (conj namefiles savefile))
      ;(doseq [f namefiles]
        ;(delete-file f))
    )))

(defn desc-show
  "show descriptor"
  [type filepath]
  (sh-put "desc" filepath "-t" type "-g" "-s"))

(defn makebook
  "make save book. save plain text file"
  [k outfile & descfiles]
  (apply sh-put (conj descfiles outfile k "-p" "cbook")))

(defn makeword
"make word.
  ex: (makeword cat.orb.book cat.rbf [0 \"cat1.orbs\" \"cat2.orbs\"] [1 \"etc1.orbs\" ...] ...)"
  [book savefile & key-descfile]
  (let [keyfile (flatten (map #(conj (seq (rest %)) (str "-" (first %))) key-descfile))]
    (apply sh-put (conj keyfile savefile book))))



