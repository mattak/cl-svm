(ns cl-svm.core
  (:use ma3.io)
	(:use ma3.util)
	(:use [clojure.contrib.seq :only (indexed)]
        [clojure.contrib.str-utils :only (str-join)])
	(:import
    (java.util Random)
    (libsvm svm svm_problem svm_model svm_parameter svm_node)))

;; util
;;----------
(defn is-type-oriented [type data]
  (let [strtype (str type)
        datatype (str (.getClass data))
        orient (fn [s] (if-let [v (re-seq #"class \[*L?(.+?);?$" s)]
                            (second (first v))))]
    (println (orient strtype) (orient datatype))
    (= (orient strtype) (orient datatype))
    ))

(defn array-depth [elm]
  (if-let [v (re-seq #"class (\[*)L?" (str (.getClass elm)))]
    (count (second (first v)))))

(defn into-svmnode [data]
  (let* [rows (count data)
         cols (count (first data))
         arr (make-array svm_node rows cols) idata (indexed data)]
    (doseq [rdata idata]
      (let [y (first rdata) cdata (indexed (second rdata))]
        (doseq [kv cdata]
          (let [x (first kv) val (second kv)]
            (aset arr y x
              (dofield (svm_node.)
                (index x)
                (value val)))))))
    arr))

(defn seq-svmnode
  "array to sequence svm node data"
  [data]
  (let [type (.getClass data)]
    (cond
        (= type svm_node)
          (vector (.index data) (.value data))
        (= type (.getClass (make-array svm_node 0)))
          (map #(seq-svmnode %) (seq data))
        (= type (.getClass (make-array svm_node 0 0)))
          (map #(seq-svmnode %) (seq data))
        true false
     )))

(defn str-svmnode
  "node to libsvm format"
  [data]
  (let* [depth (array-depth data)
        seq-data (seq-svmnode data)
        svmstr (fn [node] (format "%d:%f" (first node) (second node)))
        svmstr1 (fn [nodes] (str-join " " (map #(svmstr %) nodes)))
        svmstr2 (fn [nodess] (map #(svmstr1 %) nodess))]
    (cond (= depth 0) (svmstr seq-data)
          (= depth 1) (svmstr1 seq-data)
          (= depth 2) (svmstr2 seq-data)
          true nil)))

(defn pprint-problem
  "pretty print svm problem"
  [prob]
  (let [nodestr (str-svmnode (. prob x))
        labelstr (map #(str (int %)) (. prob y))]
    (println (str-join "\n" (map #(str %1 " " %2) labelstr nodestr)))))

;; load
;;----------
(defn model-load [file]
  (svm/svm_load_model file))

(defn node-load-bylines [lines]
  (for [line lines]
    (for [vals (re-seq #"\d+:\S+" line)]
      (let [elms (re-seq #"[^:]+" vals)]
        (let [idx (first elms) val (second elms)]
          (Double/parseDouble val)
          )))))

(defn node-load
	"svm node load. (<svm_node_array> ...)"
  [#^String file]
  (into-svmnode (node-load-bylines (lazy-input-line (tois file)))))

(defn label-load-bylines
  "svm label load"
  [lines]
  (let* [darr (double-array (count lines))
         iline (indexed lines)]
    (doseq [kv iline]
      (let [idx (first kv)
            line (second kv)]
        (let [val (Double/parseDouble (first (re-seq #"^\d+" line)))]
          (aset darr idx val))))
    darr))

(defn problem-load [#^String file]
	(let [prob (svm_problem.) lines (lazy-input-line (tois file))]
		(let [rows (count lines) cols (count (re-seq #"\d+:\S+" (first lines)))]
			(dofield prob
				(l rows)
				(x (into-svmnode (node-load-bylines lines)))
				(y (label-load-bylines lines))))
		prob))

;; parameter
;;-------------
(defn default-param
  "svm default parameter : RBF, C_SVC"
  []
	(let [p (svm_parameter.)]
		(dofield p
			(svm_type svm_parameter/C_SVC)
			(kernel_type svm_parameter/RBF)
			(degree 2)
			(gamma 0.2)
			(coef0 1)
			(nu 0.2)
			(cache_size 100)
			(C 100)
			(eps 1e-3)
			(p 0.1)
			(shrinking 1)
			(probability 1)
			(nr_weight 0)
			(weight_label nil)
			(weight nil)
		)))

(defn rbf-param
  "RBF kernel svm parameter"
  []
  (let [p (default-param)]
    (dofield p
      (kernel_type svm_parameter/RBF))))

(defn linear-param
  "linear kernel svm parameter"
  []
  (let [p (default-param)]
    (dofield p
      (kernel_type svm_parameter/LINEAR))))

;; learn
;;-------------
(defn train
	"svm trainner"
  [#^svm_parameter param #^svm_problem prob]
    (svm/svm_train prob param))

;; test
;;-------------
(defn predict-file
  "predict testfile"
  [model testfile]
	(let [nodeslist (node-load testfile)]
    (map #(svm/svm_predict model %) nodeslist)))

(defn predict
  "predict node"
  [model nodes]
  (let [depth (array-depth nodes)]
    (cond (= depth 1)
            (svm/svm_predict model nodes)
          (= depth 2)
            (map #(svm/svm_predict model %) nodes))))

(defn predict-prob
  "predict probability"
  [model nodes]
  (let* [estimates (double-array (.nr_class model))
         depth (array-depth nodes)
         proc (fn [node]
                (let [v (svm/svm_predict_probability model node estimates)]
                  [v (vec estimates)]))]
    (cond (= depth 1)
            (proc nodes)
          (= depth 2)
            (map #(proc %1) nodes))))

;; io
;;-------------
;(defn model-save)

;; data
;;-------------
(defn random-problem [fnc size dim]
  (let* [rnd (Random.)
         randdata (fn [idx]
                    (dofield (svm_node.)
                      (index idx)
                      (value (.nextDouble rnd))))
         prob (dofield (svm_problem.)
                (l size)
                (x (make-array svm_node size dim))
                (y (double-array size)))]
    (dotimes [row size]
      (dotimes [idx dim]
        (let [d (randdata idx)]
          (aset (. prob x) row idx d)))
      (aset (. prob y) row (fnc (aget (. prob x) row))))
    prob))

