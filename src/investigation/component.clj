(ns investigation.component)

(defmacro defcomponent [name inlets expression]
  `(def ~name
     {:name ~(str name)
      :inlets ~inlets
      :transformer (fn [~'connected-inlets ~'state]
                     (let [~'read-inlet (fn [inlet-key#]
                                      (if (contains? ~'connected-inlets inlet-key#)
                                        ((inlet-key# ~'connected-inlets) ~'state)
                                        ~'state))]
                       ~expression))
      }))

;(macroexpand-1 '(defcomponent foo {:bar #(identity 1)} (identity 2)))
