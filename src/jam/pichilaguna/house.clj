(ns jam.pichilaguna.house
  (:require
   [jam.sweethome3d.box-model :refer [when-sh parse]]
   [jam.sweethome3d :as sh]))

;; --------------------------------------------------
;; HOUSE

(when-sh
    (let [defaults {:level {:total-height 300        ; total height
                            :thickness    25         ; floor
                            :height       (- 300 25) ; empty space
                            }
                    :wall  {:outside {:thickness 20}
                            :inside  {:thickness 12}
                            :default :inside}}]
      (swap! sh/state assoc :defaults defaults))
  (sh/invoke-ui
     (fn []
       (let [hw 700  ;; 720= 6*120, 3*240
             hl 2000 ;;hl 2040= 17*120, 8.5*240

             stone-floor   {:floor {:texture ["Floor" "eTeksScopia#multi-grey-stones-floor-tiles"]}}
             wood-floor    {:floor {:texture ["Floor" "eTeksScopia#english-parquet-1" :angle 90]}}
             bath-floor    {:floor {:texture ["Floor" "OlaKristianHoff#beige_tiles"]}}
             kitchen-floor {:floor {:texture ["Floor" "OlaKristianHoff#beige_brown_tiles"]}}
             concrete-floor  {:floor {:texture ["Wall"  "eTeksScopia#concrete-wall-1"]}}

             door    (sh/load-furniture "Doors and windows" "eTeks#doorFrame")
             door100 {:width 100 :height 206 :furniture door}
             door70  {:width 70 :height 206 :furniture door}
             door80  {:width 80 :height 206 :furniture door}
             door90  {:width 90 :height 206 :furniture door}
             center  {:align :center}
             stair   {:width     100
                      :depth     500
                      :height    400
                      :angle     (/ Math/PI 2)
                      :mirrored  true
                      :furniture (sh/load-furniture "Staircases" "OlaKristianHoff#stair_straight_open_stringer")}
             outside {:type :outside}]
         (sh/clean-home)
         (parse
          [:level#bottom {:elevation 0}
           [:box {:width hw :length 1500 :fill :right}
            ;; bedrooms
            [:box {:d 500 :fill :forward}
             [:wall.back outside]
             [:wall.left outside]
             [:wall.front outside]
             [:box#bed-e {:d (/ hw 2)} wood-floor
              [:wall.front]
              [:wall.right {:fill :forward}
               [:box {:d 20}]
               [:door door80]]]
             [:box#bed-w {:d (/ hw 2)} wood-floor
              [:wall.right {:fill :backward}
               [:box {:d 20}]
               [:door door80]]
              ]]

            ;; living
            [:box#living {:d 400} wood-floor
             [:wall.back outside]
             [:wall.front outside {:fill :right}
              [:box {:d 20}]
              [:door door90]]]

            ;; stair/utility/bath block
            [:box {:d hw :fill :forward}
             [:wall.back outside]
             [:box#stair-well {:d 120 :fill :right} wood-floor
              [:wall.front]
              [:wall.right outside]
              [:wall.left
               [:door door90 {:align :center}]]
              [:box {:d 100}] ; landing
              [:_ stair {:d 500}]]
             [:box {:d (- hw 120) :fill :right}
              [:box {:d 400 :fill :forward}
               [:wall.right outside]
               [:box#utility {:d (- hw 120 200)} concrete-floor
                [:wall.left {:fill :forward}
                 [:box {:d 40}]
                 [:door door90]]]
               [:box {:d 200 :fill :right}
                [:box#bath {:d 400} bath-floor
                 [:wall.back]
                 [:wall.front outside]
                 [:wall.left {:fill :backward}
                  [:box {:d 20}]
                  [:door door80]]]
                ]]]]]])
         (parse 
          [:level#main {:elevation 300}
           [:box {:width (+ hw 500) :length hl :fill :forward}
            [:box#main-wing {:d     hw
                             :fill  :right
                             :label {:dy -400}}

             [:wall.left outside]
             [:wall.back outside
              [:door door100 {:align [:closet :center]}]]
             [:wall.right outside]
             [:wall.front outside {:align :left :d (- hl 500) :fill :right}
              [:box {:d 400}]
              [:door door100]
              [:door door100 {:align [:greenhouse :center]}]]

             [:box#greenhouse {:d 300} stone-floor
              [:wall.right outside {:fill :forward}
               [:door door90 {:align [:hallway :center]}]]]

             [:box#living-room {:d 700} wood-floor]
             [:box {:d 1000 :fill :forward}
              [:box {:d 250 :fill :left}
               [:box#bath {:d 110} bath-floor
                [:wall.left
                 [:door door70 {:align [:hallway :center]}]]]
               [:box#mud {:d 290} stone-floor]
               [:box {:d 600 :fill :forward}
                [:box#stair {:d 110 :fill :right} wood-floor
                 [:_ stair {:d 500}]]
                [:box#hallway {:d 140} wood-floor]]]
              [:box {:d 450 :fill :right}
               [:box#kitchen {:d 400} kitchen-floor]
               [:box#pantry {:d 200} kitchen-floor
                [:wall.left {:fill :backward}
                 [:box {:d 70}]
                 [:door door70 {:d 100}]]
                [:wall.back
                 [:door door90 center]]]
               [:box#closet {:d (+ 55 90 55)} wood-floor
                [:wall.left]
                [:wall.back [:door door90 center]]]
               [:box#bath {:d 200} bath-floor
                [:wall.left [:door door90 center]]
                [:wall.back]]]]]

            ;; master wing and deck
            [:box {:d 500 :fill :left}
             [:box#bedroom {:d 500} wood-floor
              [:wall.back [:door door90 {:align [:closet :center]}]]
              [:wall.right outside]
              [:wall.left outside {:height 325}
               [:door door90 {:align [:deck :center]}]]
              [:wall.front outside {:height [275 325]}]]
             [:box {:d 1500 :fill :forward}
              [:box#deck {:d 320} stone-floor]
              [:box {:d 180 :fill :left}
               [:box#landing {:d 100} stone-floor]]]]]])

         ;; loft
         (parse
          [:level#loft {:elevation 600}
           [:box {:width hw :length hl :fill :right}
            [:wall.back outside {:height [-0.5 (* 100 20 1/5)]}]
            [:wall.front outside {:height [(* 100 20 1/5) -0.5]}]
            [:wall.right outside {:height (* 100 20 1/5)}]

            [:box {:d 1000}]
            [:box#loft {:d 1000 :fill :right} wood-floor
             [:box {:d 400 :fill :forward}
              [:box {:d 120}]
              ;; tele novela room
              [:box#novela {:d (- hw 120)} wood-floor
               [:wall.right {:fill :forward}
                [:box {:d 20}]
                [:door door90]]]]
             ;; art room
             [:box#art-room {:d 600 :fill :forward} wood-floor
              [:box {:d 120 :fill :right}
               [:box {:d 200}
                [:wall.right
                 [:door door90 {:align :center}]]
                [:wall.front]
                ]]]]]])))))

#_ (sh/inspect-selected)
