(ns ^:figwheel-always investigation.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [goog.string.StringBuffer]
            [reagent.core :as reagent :refer [atom]])
  (:require-macros [investigation.component :as component]))

(defn log [o]
  (.log js/console (pr-str o)))

(defn log-o [o] (.log js/console o))


(defn route-signal! [component key signal-fn]
  (let [signal-router (:signals component)]
    (swap! signal-router assoc key signal-fn)))

(defn make-uuid
  []
  (letfn [(f [] (.toString (rand-int 16) 16))
          (g [] (.toString  (bit-or 0x8 (bit-and 0x3 (rand-int 15))) 16))]
    (UUID. (.toString
             (goog.string.StringBuffer.
               (f) (f) (f) (f) (f) (f) (f) (f) "-" (f) (f) (f) (f)
               "-4" (f) (f) (f) "-" (g) (f) (f) (f) "-"
               (f) (f) (f) (f) (f) (f) (f) (f) (f) (f) (f) (f))))))

(defn make-component [component]
  {:component component
   :id (make-uuid)})

(component/defcomponent tick-flip-1
                        {:radius-factor #(identity 100.0)}
                        (if (< (:r state) 300)
                          (update-in state [:r] + (signal :radius-factor))
                          {:r 0.0
                           :col (if (= 0 (:col state)) 255 0)}))

(route-signal! tick-flip-1 :radius-factor #(identity 1.0))

(component/defcomponent
  draw-lines-1 {}
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

    (assoc state :shapes (map make-line (range num-lines)))))

(def pipeline (atom [(make-component tick-flip-1)
                     (make-component draw-lines-1)]))

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
  (let [transformers (map #(-> % :component :transformer) @pipeline)
        t (or (:t state) 0)
        state-with-t (assoc state :t (inc t))]
    ((apply comp transformers) state-with-t)))

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

(defn pipeline-stage [idx stage]
  (let [component (:component stage)
        id (:id stage)
        name (:name component)]
    ^{:key idx} [:li name " :: " id]))

(defn controls []
  [:div
   [:h1 "Pipeline:"]
   [:ul
    (doall (map-indexed pipeline-stage @pipeline))]])

(reagent/render-component [controls] (. js/document (getElementById "controls")))

