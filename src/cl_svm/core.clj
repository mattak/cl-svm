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
(defmacro node-value
  "a little bit shorten accessor for svmnode[pos].value"
  [node pos]
  `(.value (aget ~node ~pos)))

(defmacro node-index
  "a little bit shorten accessor for svmnode[pos].index"
  [node pos]
  `(.index (aget ~node ~pos)))


(defn is-type-oriented 
  "define is this type oriented?"
  [type data]
  (let [strtype (str type)
        datatype (str (.getClass data))
        orient (fn [s] (if-let [v (re-seq #"class \[*L?(.+?);?$" s)]
                            (second (first v))))]
    (println (orient strtype) (orient datatype))
    (= (orient strtype) (orient datatype))
    ))

(defn array-depth
  "return depth of java array"
  [elm]
  (if-let [v (re-seq #"class (\[*)L?" (str (.getClass elm)))]
    (count (second (first v)))))

(defn into-node
  "svmnode (java) to clojure vector in sequence"
  [data]
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

(defn seq-node
  "array to sequence svm node data"
  [data]
  (let [type (.getClass data)]
    (cond
        (= type svm_node)
          (vector (.index data) (.value data))
        (= type (.getClass (make-array svm_node 0)))
          (map #(seq-node %) (seq data))
        (= type (.getClass (make-array svm_node 0 0)))
          (map #(seq-node %) (seq data))
        true false
     )))

(defn str-node
  "node to libsvm format"
  [data]
  (let* [depth (array-depth data)
        seq-data (seq-node data)
        svmstr (fn [node] (format "%d:%f" (first node) (second node)))
        svmstr1 (fn [nodes] (str-join " " (map #(svmstr %) nodes)))
        svmstr2 (fn [nodess] (map #(svmstr1 %) nodess))]
    (cond (= depth 0) (svmstr seq-data)
          (= depth 1) (svmstr1 seq-data)
          (= depth 2) (svmstr2 seq-data)
          true nil)))

(defn str-label
  "to string label"
  [label]
  (map #(str (int %)) label))

(defn str-problem
  "to string svm problem"
  [prob]
  (let [nodestr (str-node (. prob x))
        labelstr (map #(str (int %)) (. prob y))]
    (map #(str %1 " " %2) labelstr nodestr)))

;; load
;;----------
(defn load-model
  "load model by svm_load_model"
  [file]
  (svm/svm_load_model file))

(defn load-node-bylines
  "load svmnode by lines"
  [lines]
  (for [line lines]
    (for [vals (re-seq #"\d+:\S+" line)]
      (let [elms (re-seq #"[^:]+" vals)]
        (let [idx (first elms) val (second elms)]
          (Double/parseDouble val)
          )))))

(defn load-node
	"svm node load. (<svm_node_array> ...)"
  [#^String file]
  (into-node (load-node-bylines (lazy-input-line (tois file)))))

(defn load-label-bylines
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

(defn load-problem
  "load svm problem"
  [#^String file]
	(let [prob (svm_problem.) lines (lazy-input-line (tois file))]
		(let [rows (count lines) cols (count (re-seq #"\d+:\S+" (first lines)))]
			(dofield prob
				(l rows)
				(x (into-node (load-node-bylines lines)))
				(y (load-label-bylines lines))))
		prob))


;; save
;;-------------
(defn save-model
  "save svm model"
  [#^String file #^svm_model model]
  (svm/svm_save_model file model))

(defn save-problem
  "save problem"
  [#^String filepath #^svm_problem prob]
  (str2file filepath (str-join "\n" (str-problem prob))))

(defn save-label
  "save parameter of problem"
  [#^String filepath #^svm_problem prob]
  (str2file filepath (str-join "\n" (str-label (.y prob)))))

(defn save-node
  "save svmnode"
  [#^String filepath prob-or-node]
  (let [node (if (= (class prob-or-node) svm_problem)
                (seq-node (. prob-or-node x))
                (seq-node prob-or-node))]
      (str2file (str-join "\n" (seq-node node)))))

;; parameter
;;-------------
(defn libsvm-default-param
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

(defmacro get-hashed-param
  [param]
  (let* [symbs '(svm_type kernel_type degree gamma coef0
                 nu cache_size C eps shrinking probability
                 nr_weight weight_label weight)
         gparam (gensym)
         keys (apply vector (map #(keyword %) symbs))]
    `(let [~gparam ~param]
        (reduce
            #(assoc %1 (first %2) (second %2))
            {}
            (map vector
                ~keys
                ~(apply vector (map #(list '. gparam %) symbs)))))))

(defmacro set-hashed-param
  "hash to libsvm/svm_parameter"
  [hashparam]
  (let [symbs '(svm_type kernel_type degree gamma coef0
                nu cache_size C eps shrinking probability 
                nr_weight weight_label weight)
        ghash (gensym)
        keys (apply vector (map #(keyword %) symbs))]
    `(let
        [~ghash ~hashparam]
        (dofield
          (libsvm-default-param)
          ~@(map #(list %1 (list %2 hashparam))
                symbs
                keys)))))

(defn change-param
  "change parameter to new"
  [hash diff]
  (reduce #(assoc %1 (first %2) (second %2))
    (reduce dissoc hash (keys diff))
    diff))

(defn default-param
  []
  (get-hashed-param
    (libsvm-default-param)))

(defn rbf-param
  "RBF kernel svm parameter"
  []
  (change-param (default-param) {:kernel_type svm_parameter/RBF}))

(defn linear-param
  "linear kernel svm parameter"
  []
  (change-param (default-param) {:kernel_type svm_parameter/LINEAR}))


;; learn
;;-------------
(defn train
	"svm trainner"
  [param_hash #^svm_problem prob]
  (svm/svm_train prob (set-hashed-param param_hash)))

(declare predict-summary)

(defn train-best
  "get best trained parameter"
  [paramlst #^svm_problem prob]
  (for [param paramlst]
    (let [model (train param prob)]
      (assoc (predict-summary model prob) :model model))))

;; test
;;-------------
(defn predict-file
  "predict testfile"
  [model testfile]
	(let [nodeslist (load-node testfile)]
    (map #(svm/svm_predict model %) nodeslist)))

(defn predict
  "predict node"
  [model nodes]
  (let [depth (array-depth nodes)]
    (cond (= depth 1)
            (svm/svm_predict model nodes)
          (= depth 2)
            (map #(svm/svm_predict model %) nodes))))

(defn predict-problem
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

(defn predict-summary
  "learned model is correct?
   return summary of predicted result"
  [#^svm_model model #^svm_problem prob]
  (let* [result (predict model (.x prob))
         total (count result)
         correct (filter #(identity %) (map #(= %1 %2) result (.y prob)))
         correct-cnt (count correct)]
    {:total total
     :correct correct-cnt
     :wrong (- total correct-cnt)
     :percent (double (/ correct-cnt total))}
  ))

;; data
;;-------------
(defn random-problem
  "create random problem by user function.
   datarange is [0,1]"
  [fnc size dim]
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

