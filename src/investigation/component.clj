(ns investigation.component)

(defmacro defcomponent [name signals expression]
  `(def ~name
     (let [signals# (atom ~signals)]
       {:name ~(str name)
        :signals signals#
        :transformer (fn [~(quote state)]
                       (let [~(quote signal) (fn [k#]
                                               (if (contains? @signals# k#)
                                                 ((k# @signals#) ~(quote state))
                                                 ~(quote state)))]
                         ~expression))
        })))

