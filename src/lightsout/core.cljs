(ns lightsout.core
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(def percent-out 20)
(def debug false)

(defn random-percent-bool [p]
  (if debug true
      (let [result (< (rand-int 100) p)]
        ;; (prn "random chance:" result)
        result)))

(defn new-board [n]
  (let [result 
        (vec (repeatedly n
               (fn [] (vec
                 (repeatedly n
                   #(not (random-percent-bool percent-out)))))))]
    ;; (prn "new board:" result)
    result))

;; (println "Something changed...")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (let [board-size 8]
                     (atom {:state :playing
                            :board-size board-size
                            :board (new-board board-size)})))

(defn new-game! []
  ;; (prn "new game!")
  (swap! app-state assoc-in
         [:board]
         (new-board (:board-size @app-state)))
  (swap! app-state assoc-in [:state] :playing))

(defn get-board-pos [c r]
  (get-in (:board @app-state) [c r]))

(defn set-board-pos! [c r value]
  (swap! app-state assoc-in [:board c r] value))

(defn toggle-board-pos [board c r]
  (let [new-val (not (get-in board [c r]))]
    ;; (prn "new value:" new-val)
    (assoc-in board [c r] new-val)))

(defn find-neighbors-ud [board c r]
  (case c
    0 [[1 r]]
    (dec (count (first board)))
      [[(- (count (first board)) 2) r]]
    (let []
      ;; (prn "default UD")
      [[(dec c) r] [(inc c) r]])))

(defn find-neighbors-lr [board c r]
  ;; (prn (count (first board)))
  (case r
    0 [[c 1]]
    (dec (count (first board)))
      [[c (- (count (first board)) 2)]]
    (let []
      ;; (prn "default case for lr")
      [[c (dec r)] [c (inc r)]])))

(defn valid-coords? [max coords]
  (and (>= (first coords) 0)
       (< (first coords) max)
       (>= (second coords) 0)
       (< (second coords) max)))

(defn find-neighbors [board c r]
  (filter (partial valid-coords? (count (first board))) (apply conj (find-neighbors-ud board c r) (find-neighbors-lr board c r))))

(defn flip [board c r]
  (let [neighbors (conj (find-neighbors board c r) [c r])]
    ;; (prn "neighbors" neighbors)
    (vec (filter #(vector? %)
                 (reduce (fn [current-board coords]
                           (apply (partial toggle-board-pos current-board)
                                  coords))
                         board
                         neighbors)))))

;; (prn "test board" (flip (new-board 5) 4 2))

;; (prn "find-neighbors:" (find-neighbors (new-board 5) 2 2))

(defn game-over? [board]
  ;; (prn "the board:")
  ;; (prn board)
  ;; (prn "game-over?" board)
  (let [over? 
        (every? (fn [item] (= true item))
                (map (fn [col]
                       (let [result 
                             (every? #(= false %) col)]
                         (prn "column" result)
                         result))
                     board))]
    (prn "game-over-check" over?)
    over?))

(prn "game-over?" (game-over? [[false false] [false false]]))

(defn make-move! [c r]
  (let [next-board (flip (:board @app-state) c r)]
    (swap! app-state assoc-in [:board] next-board)
    (swap! app-state assoc-in [:state]
           (if (game-over? next-board)
             :game-over
             :playing))))

(defn blank [i j state]
  [:rect {:width 0.9
          :height 0.9
          :fill "#999"
          :x i
          :y j
          :on-click (fn click-rect [e]
                      (if (= state :playing) (make-move! i j)))}])

(defn filled [i j state]
  [:rect (assoc-in (second (blank i j state)) [:fill] "#f88")])

(defn won [i j state]
  [:rect (assoc-in (second (blank i j state)) [:fill] "#8f8")])

(defn board-size []
  (:board-size @app-state))

; 5x5 patterns
;  . x x . x
;  x x x . .
;  x x . x x
;  . . x x x
;  x . x x .

(defn lightsout []
  (let [state (:state @app-state)]
    
    [:center
     [:div
      [:h1 (if (= state :playing)
             "Turn out the lights!"
             "You did it!")]
      [:div [:button
             {:on-click #(new-game!)}
             "Restart"]]
      (into
       [:svg {:view-box (str "0 0 " (board-size) " " (board-size))
              :width 500
              :height 500}]
       (for [i (range (count (:board @app-state)))
             j (range (count (:board @app-state)))]
         (if (= state :game-over)
           (won i j state)
           (let [value (get-board-pos i j)]
             (if value
               (filled i j state)
               (blank i j state))))))]]))

(reagent/render-component [lightsout]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
   ;; (swap! app-state update-in [:__figwheel_counter] inc)
   ;; (swap! app-state assoc-in [:board] (new-board 5))
   
   (println (str "app-state: " @app-state))
)


