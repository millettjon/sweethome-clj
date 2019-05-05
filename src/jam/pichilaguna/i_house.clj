(ns jam.pichilaguna.i-house
  (:require [jam.sweethome3d
             :refer [state clean-home add-level add-walls create-room delete-wall invoke-ui midpoint dim]
             :as sh]))

;; lengths in cm
(def defaults
  {:level {:total-height 300 ; total height
           :thickness 25 ; floor
           :height (- 300 25); empty space
           }
   :width 640
   :wall {:exterior {:thickness 15}
          :interior {:thickness 10}
          :virtual {:thickness 0.5}}})
(swap! state assoc :defaults defaults)

#_ (do
  (defn build
    []
    (clean-home)

    (let [hw               (-> defaults :width)
          wood-floor       {:texture ["Floor" "eTeksScopia#english-parquet-1" :angle 90]}
          bath-floor       {:texture ["Floor" "OlaKristianHoff#beige_tiles"]}
          kitchen-floor    {:texture ["Floor" "OlaKristianHoff#beige_brown_tiles"]}
          concrete         {:texture ["Wall"  "eTeksScopia#concrete-wall-1"]}
          gray-stone-floor {:texture ["Floor" "eTeksScopia#multi-grey-stones-floor-tiles"]}]

      ;; ========== BOTTOM LEVEL ==========
      (add-level "bottom" {:elevation 0})

      ;; exterior walls
      (add-walls :exterior {:closed? true}
                 [[:x 0    :y 0  :house-E-0]
                  [:x 1400       :house-S-0]
                  [        :y hw :house-W-0]
                  [:x 0          :house-N-0]])

      ;; separate bedroom from main room
      (add-walls :interior
                 [[:x 600 :y :house-W-0 :bedroom-0]
                  [       :y :house-E-0           ]])

      ;; separate bedrooms from each other
      (add-walls :interior
                 [[:x :house-N-0 :y '(midpoint :house-N-0) :bedroom-sep-0]
                  [:x :bedroom-0                                         ]])
      (create-room :around [:bedroom-sep-0 :E] :floor wood-floor)
      (create-room :around [:bedroom-sep-0 :W] :floor wood-floor)

      ;; closets
      (let [entry-width 100]
        (doseq [i [1 -1]]
          (add-walls :interior
                     [[:x '(- (dim :bedroom-0) 75) :y `(+ (dim :bedroom-sep-0) (* ~i ~entry-width))]
                      [:x :bedroom-0                                                               ]])))

      ;; utility room
      (add-walls :interior
                 [[:x '(- (dim :house-S-0) 300) :y 0          :utility-0]
                  [                             :y :house-W-0           ]])
      (create-room :around [:utility-0 :S] :floor concrete)

      ;; bath room
      (add-walls :interior
                 [[:x :bedroom-0 :y 150 :bath-sep-0]
                  [:x :utility-0       ]])
      (create-room :around [:bath-sep-0 :E] :floor bath-floor)
      (create-room :around [:bath-sep-0 :W] :floor wood-floor)

      ;; ========== MAIN LEVEL ==========
      (add-level "main" {:elevation 300})

      ;; exterior walls
      (add-walls :exterior {:closed? true}
                 [[:x 0    :y 0  :house-E-1]
                  [:x 2300       :house-S-1]
                  [        :y hw :house-W-1]
                  [:x 0          :house-N-1]])

      ;; greenhouse
      (add-walls :interior
                 [[:x 375 :y 0 :greenhouse]
                  [       :y hw           ]])
      (create-room :around [:greenhouse :N] :floor gray-stone-floor)

      ;; bodega
      (let [bodega-width 475
            hall-width   125
            bath-width   125
            bath-length  (/ bodega-width 2)
            mud-width    bath-width
            mud-length   bath-length]
        (add-walls :interior
                   [[:x `(- (dim :house-S-1) ~bodega-width) :y 0                            :bodega-N-1]
                    [                                       :y (- hw hall-width bath-width) :bodega-W-1]
                    [:x :house-S-1                                                                     ]])
        (create-room :around [:bodega-W-1 :E] :floor bath-floor)

        ;; bath
        (add-walls :interior
                   [[:x :bodega-N-1                       :y hw                :bath-1-N]
                    [                                     :y (- hw bath-width) :bath-1-E]
                    [:x `(- (dim :house-S-1) ~mud-length)                      :bath-1-S]
                    [                                     :y hw                         ]]))
      (create-room :around [:bath-1-N :S] :floor bath-floor)

      ;; entry/mud
      ;; Create a temp a wall to define mud area room.
      (add-walls :virtual
                 [[:x :bath-1-S :y :bath-1-E :entry-temp]
                  [:y :bodega-W-1]])
      (create-room :around [:entry-temp :S] :floor gray-stone-floor)

      ;; kitchen
      (let [kitchen-width 500
            stair-width   (+  100 5)
            fridge-width  80]
        (add-walls :interior
                   [[:x `(- (dim :bodega-N-1) (* 2 ~stair-width)) :y 0                ]
                    [                                             :y   :bodega-W-1    ]
                    [:x #(- % fridge-width)                            :thickness 0.5 :kitchen-temp-1]
                    [:x #(- % (- kitchen-width fridge-width))          :thickness 0.5 :kitchen-temp-2]
                    [                                             :y 0                ]]))
      (create-room :around [:kitchen-temp-1 :E] :floor kitchen-floor)
      (create-room :around [:greenhouse :S] :floor wood-floor)

      ;; garage
      (let [garage-width 675]
        (add-walls :exterior
                   [[:x :house-S-1          :y hw              ]
                    [:x #(+ % garage-width)        :garage-S-1 ]
                    [                       :y 0   :thickness 0.5 :garage-temp]
                    [:x :house-S-1                             ]]))
      (create-room :around [:house-S-1 :S] :floor concrete)

      ;; delete temp walls
      (delete-wall :entry-temp)
      (delete-wall :garage-temp)
      (delete-wall :kitchen-temp-1)
      (delete-wall :kitchen-temp-2)

      ;; ========== TOP LEVEL ==========
      (add-level "top" {:elevation 600})

      (let [peak {:x      1900
                  :height 450}]
        (add-walls :exterior {:closed? true}
                   [[:x 0           :y 0  :height 0   ]
                    [:x peak              :height peak]
                    [:x :garage-S-1       :height 0   ]
                    [               :y hw             ]
                    [:x peak              :height peak]
                    [:x 0                 :height 0   ]
                    ]))

      ;; loft
      (add-walls :interior
                      ;; loft start x
                 [[:x 1110 :y 0  :height 10 :loft-N]
                  [        :y hw                   ]])

      ;; stair
      (add-walls :interior
                                        ;; loft width
                 [[:x '(+ (dim :loft-N) 370) :y 0        :stair-N-2]
                  [                          :y (/ hw 2)           ]])

      ;; closet
      (add-walls :interior
                 [[:x :stair-N-2 :y hw]
                            ;; closet width
                  [:y (- hw 190)]
                            ;; stair width
                                ;; closet width
                  [:x #(+ % 264 230) :closet-S-2]
                  [:y hw]
                  ])
      (create-room :around [:closet-S-2 :N] :floor wood-floor)

      ;; loft room ends at stair wall
      (add-walls :virtual
                 [[:x :stair-N-2 :y (/ hw 2) :loft-temp]
                  [              :y :closet-S-2]])
      (create-room :around [:loft-temp :N] :floor wood-floor)


      ;; bath
      (add-walls :interior
                                           ;; stair width
                 [[:x '(+ (dim :stair-N-2) 264) :y 0       :bath-N-2]
                  [                             :y (/ hw 2)]
                  [:x :closet-S-2]
                  [:y 0]])
      (create-room :around [:bath-N-2 :S] :floor bath-floor)

      ;; bedroom
      (add-walls :interior
                                            ;; bedroom width
                 [[:x '(+ (dim :closet-S-2) 320) :y 0 :bed-2-S]
                  [:y hw]])

      (create-room :around [:bed-2-S :N] :floor wood-floor)
      (create-room :around [:bed-2-S :S] :floor wood-floor)
;;      (create-room :around [:loft-temp :S] :floor plywood)
      (delete-wall :loft-temp)


      ;; note: double stairs are 2.64 (1.32 each)
      ;;       seems too wide, how wide do stairs need to be for a king bed?
      ;;       can always lift large items over loft

      ;; kitchen/loft start at 1110 = (+ 449 457 204)
      ;; ? what is slope of wall?
      ;; ? how high is peak? appears to be at 4.5m (max is 5m)
      ;; ? what is x coordinate of peak?
      ;;   basically over the start of the master bedroom
      ;; ? how high is ceiling at:
      ;;   - greenhouse wall
      ;;   - outside of loft
      ;;   - inside edge of loft
      ;;   - inside edge of master bedroom


      (sh/set-walls-alpha 0.0)
      (sh/set-level "main")
      (sh/set-compass (sh/degrees-to-radians -90))
      ))

  (invoke-ui build))
