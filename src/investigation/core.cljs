(ns ^:figwheel-always investigation.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [reagent.core :as reagent :refer [atom]]))

(def tick-flip-1
  ^{:visible-name "tick-flip-1"}
  (fn [state]
    (if (< (:r state) 300)
      (update-in state [:r] + 5.0)
      {:r 0.0
       :col (if (= 0 (:col state)) 255 0)})))


(def tick-flip-2
  ^{:visible-name "tick-flip-2"}
  (fn [state]
    (if (< (:r state) 100)
      (update-in state [:r] + 5.0)
      {:r 0.0
       :col (if (= 0 (:col state)) 100 0)})))

(def draw-lines-1
  ^{:visible-name "draw-lines-1"}
  (fn [state]
    (let [hw (* 0.4 (q/width))
          hh (* 0.4 (q/height))
          num-lines (quot (q/width) 10)
          make-line (fn []
                      (let [rand-ang (q/random 0 q/TWO-PI)
                            r (:r state)
                            x2 (+ hh (* (q/sin rand-ang) r))
                            y2 (+ hw (* (q/cos rand-ang) r))]
                        [:line
                         {:x1 hh :y1 hw :x2 x2 :y2 y2}
                         {:stroke (:col state)}]))]

    (assoc state :shapes (map make-line (range num-lines))))))

(def draw-lines-2
  ^{:visible-name "draw-lines-2"}
  (fn [state]
    (let [hw (* 0.6 (q/width))
          hh (* 0.6 (q/height))
          num-lines (quot (q/width) 100)
          make-line (fn []
                      (let [rand-ang (q/random 0 q/TWO-PI)
                            r (:r state)
                            x2 (+ hh (* (q/sin rand-ang) r))
                            y2 (+ hw (* (q/cos rand-ang) r))]
                        [:line
                         {:x1 hh :y1 hw :x2 x2 :y2 y2}
                         {:stroke (:col state)}]))]

      (assoc state :shapes (map make-line (range num-lines))))))

(def pipeline-options [[tick-flip-1 tick-flip-2] [draw-lines-1 draw-lines-2]])
(def pipeline (atom [tick-flip-1 draw-lines-1]))

;; -- plumbing

(defmulti draw-shape! first)
(defmethod draw-shape! :line [[_ params pen]]
  (do
    (q/stroke (:stroke pen))
    (q/line (:x1 params)
            (:y1 params)
            (:x2 params)
            (:y2 params))))

(defn run-pipeline [state]
  ((apply comp @pipeline) state))

(defn setup []
  (q/frame-rate 30)
  (run-pipeline {}))

(defn draw-state [state]
  (let [shapes (:shapes state)]
    (doseq [shape shapes] (draw-shape! shape))))

(q/defsketch switcheroo
             :host "hello-quil-cljs"
             :size [500 500]
             :setup setup
             :update run-pipeline
             :draw draw-state
             :middleware [m/fun-mode])

(defn fn-name [f]
  (-> f meta :visible-name))

(defn pipeline-stage [idx stage]
  (let [active-fn (nth @pipeline idx)]
    ^{:key idx} [:li
                 {:on-click (fn [_]
                              (let [other-fn (first (remove #(= % active-fn) stage))
                                    swapped-pipeline (assoc @pipeline idx other-fn)]
                                (reset! pipeline swapped-pipeline)))}
                 (fn-name active-fn)]))

(defn controls []
  [:div
   [:h1 "Pipeline:"]
   [:ul
    (doall (map-indexed pipeline-stage pipeline-options))]])

(reagent/render-component [controls] (. js/document (getElementById "controls")))

