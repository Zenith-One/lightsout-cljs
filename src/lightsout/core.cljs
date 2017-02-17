(ns lightsout.core
  (:require [reagent.core :as reagent :refer [atom]]
            [goog.string :as gstring]))

(enable-console-print!)

(declare new-game!)

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

(defonce app-state
  (let [board-size 8]
    (atom {:state :waiting
           :board-size board-size
           :board (new-board board-size)
           :moves 0
           :time 0
           :timer 0})))

(defn inc-time! []
  (swap! app-state update-in [:time] inc))

(defn tick! [next-fn]
  (case (:state @app-state)
    :playing (do (inc-time!)
                (js/setTimeout next-fn 1000))
    :starting (new-game!)
    (println "Game over. Stopping timer.")))

(defn zero-pad [s]
  (if (< (count (str s)) 2)
    (str "0" s)
    s))

(defn display-time [seconds]
  (let [d (js/Date. nil)]
    (.setHours d 0)
    (.setSeconds d seconds)
    (str
     (zero-pad (.getHours d)) ":"
     (zero-pad (.getMinutes d)) ":"
     (zero-pad (.getSeconds d)))))

(defn get-timer [id]
  (fn []
    (if (= (:timer @app-state) id)
      (tick! (get-timer id))
      (println "Not the current timer. Bailing."))))

(defn start-new-game! []
  (new-game!))

(defn new-game! []
  ;; (prn "new game!")
  (let [new-timer-id (inc (:timer @app-state))]
    (js/setTimeout (get-timer new-timer-id)
                   1000)
    (swap! app-state assoc-in [:timer] new-timer-id))
  (swap! app-state assoc-in [:moves] 0)
  (swap! app-state assoc-in [:time] 0)
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
                         result))
                     board))]
    over?))



(prn "game-over?" (game-over? [[false false] [false false]]))

(defn make-move! [c r]
  (let [next-board (flip (:board @app-state) c r)]
    (swap! app-state assoc-in [:board] next-board)
    (swap! app-state assoc-in [:state]
           (if (game-over? next-board)
             :game-over
             :playing))
    (swap! app-state update-in [:moves] inc)))

(defn blank [i j state]
  [:rect {:width 0.9
          :height 0.9
          :fill "#888"
          :x i
          :y j
          :stroke "#666"
          :stroke-width 0.015
          :on-click (fn click-rect [e]
                      (if (= state :playing) (make-move! i j)))}])

(defn assoc-in-multiple [base & settings-pairs]
  (reduce (fn set-settings-pair [acc pair]
            (assoc-in acc [(first pair)] (second pair)))
          base
          settings-pairs))

(defn filled [i j state]
  (let [settings (assoc-in-multiple
                  (second (blank i j state))
                  [:fill "#f88"]
                  [:stroke "#d55"])]
    [:rect settings]))

(defn won [i j state]
  (let [settings (assoc-in-multiple
                  (second (blank i j state))
                  [:fill "#2a2"]
                  [:stroke "#282"])]
    [:rect settings]))

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
    
    [:center {:style {:max-width 600
                      :margin "0 auto"}}
     [:div
      [:h1 (case state
             :playing "Turn out the lights!"
             :game-over "You did it!"
             "Click New Game to begin.")]
      [:div
       [:div {:class "third"}
        [:p [:strong "Moves: "] (:moves @app-state)]]
       [:div {:class "third"}
        [:a {:href "#"
             :on-click (fn [e]
                         (.preventDefault e)
                         (start-new-game!))
             :class (if (or (= state :game-over)
                            (= state :waiting))
                      "button game-over"
                      "button")}
         [:i {:class "fa fa-refresh"}
          (gstring/unescapeEntities
           (str " &nbsp;&nbsp;"
                (if (= state :playing)
                  "Restart"
                  "New Game")))]]]
       [:div {:class "third"}
        [:p [:strong "Time: "] (display-time (:time @app-state))]]]
      
      (into
       [:svg {:view-box (str "0 0 " (board-size) " " (board-size))
              :width "70%"
              :height "80%"
              :style {:margin-top "20px"
                      :max-width "500px"}}]
       (for [i (range (count (:board @app-state)))
             j (range (count (:board @app-state)))]
         (case state
           :waiting (blank i j state)
           :game-over (won i j state)
           :playing            
           (let [value (get-board-pos i j)]
             (if value
               (filled i j state)
               (blank i j state))))))]]))

(reagent/render-component [lightsout]
                          (. js/document (getElementById "app")))

(defn win-game! []
  (swap! app-state assoc-in [:state] :game-over))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
   ;; (swap! app-state update-in [:__figwheel_counter] inc)
   ;; (swap! app-state assoc-in [:board] (new-board 5))
   
   (println (str "app-state: " @app-state))
)


