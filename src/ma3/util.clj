(ns ma3.util)

;; doto for field
;;--------------------------
(defmacro dofield
	"macro for multiple field set!"
  [x & forms]
    (let [gx (gensym)]
      `(let [~gx ~x]
        ~@(map (fn [f]
                (if (seq? f)
                  `(set! (. ~gx ~(first f)) ~(second f))))
          forms)
      ~gx)))

