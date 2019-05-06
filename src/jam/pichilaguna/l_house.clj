(ns jam.pichilaguna.l-house
  (:require [jam.sweethome3d
             :refer [state clean-home add-level add-walls create-room delete-wall
                     invoke-ui midpoint dim dimension-line load-furniture add-door add-furniture]
             :as sh]))

;; lengths in cm
(def defaults
  {:level {:total-height 300        ; total height
           :thickness    25         ; floor
           :height       (- 300 25) ; empty space
           }
   :wall  {:exterior {:thickness 20}
           :interior {:thickness 12}
           :virtual  {:thickness 0.5 :height 50}}})
(swap! state assoc :defaults defaults)

(when (sh/connected?)
  (defn build
    []
    (clean-home)
    (sh/set-compass (* Math/PI 3/2))

    (let [iw               (-> defaults :wall :interior :thickness)
          iw2              (/ iw 2)
          ew               (-> defaults :wall :exterior :thickness)
          ew2              (/ ew 2)
          hw               (- 640 ew)
          hl               (- 2100 ew)
          wood-floor       {:texture ["Floor" "eTeksScopia#english-parquet-1" :angle 90]}
          bath-floor       {:texture ["Floor" "OlaKristianHoff#beige_tiles"]}
          kitchen-floor    {:texture ["Floor" "OlaKristianHoff#beige_brown_tiles"]}
          concrete         {:texture ["Wall"  "eTeksScopia#concrete-wall-1"]}
          gray-stone-floor {:texture ["Floor" "eTeksScopia#multi-grey-stones-floor-tiles"]}
          door             (load-furniture "Doors and windows" "eTeks#doorFrame")
          stair            (load-furniture "Staircases" "OlaKristianHoff#stair_straight_open_stringer")
          stair-w          80
          stair-l          500]

      ;; ========== BOTTOM LEVEL ==========
      (add-level "bottom" {:elevation 0})

      (let [hl-0      1200
            bed-l     500
            bed-entry 100
            utility-w 300
            utility-l 400]
        ;; exterior walls
        (add-walls :exterior {:closed? true}
                   [[:x 0    :y 0  :house-E-0]
                    [:x hl-0       :house-S-0]
                    [        :y hw :house-W-0]
                    [:x 0          :house-N-0]])
        (if-let [room (create-room :around [:house-E-0 :W])]
          (doto room
            (.setAreaXOffset 100)
            (.setName "Bottom Floor")
            (.setNameXOffset 100)
            (.setNameYOffset -150)
            (.setAreaYOffset -120)))
        (dimension-line :house-E-0 :house-W-0 {:align :outside})
        (dimension-line :house-N-0 :house-S-0 {:align :outside})
        
        ;; separate bedroom from main room
        (add-walls :interior
                   [[:x bed-l :y :house-W-0 :bedroom-0]
                    [:y :house-E-0]])

        ;; separate bedrooms from each other
        (add-walls :interior
                   [[:x :house-N-0 :y '(midpoint :house-N-0) :bedroom-sep-0]
                    [:x :bedroom-0                                         ]])
        (create-room :around [:bedroom-sep-0 :E] :floor wood-floor)
        (create-room :around [:bedroom-sep-0 :W] :floor wood-floor)

        ;; closets
        (doseq [i [1 -1]]
          (add-walls :interior
                     [[:x '(- (dim :bedroom-0) 75) :y `(+ (dim :bedroom-sep-0) (* ~i ~bed-entry))]
                      [:x :bedroom-0]])
          (add-door door :bedroom-0 (+ (* i (/ bed-entry 2)) (/ hw 2)) {:width 80}))
        (dimension-line :house-E-0 :bedroom-sep-0 {:align :inside})
        (dimension-line :house-N-0 :bedroom-0 {:align :inside})

        ;; stair
        (add-walls :interior
                   [[:x :house-S-0 :y (+ ew2 stair-w iw2) :stairs-0-W]
                    [:x #(- % utility-w)]])
        (add-walls :virtual
                   [[:x `(-> :house-S-0 dim (- ~utility-w)) :y :stairs-0-W :stairs-0-N]
                    [:y 0]])
        (add-furniture stair {:x `(-> :house-S-0 dim (- ~utility-w) (+ (/ ~stair-l 2)))
                              :y        (+ ew2 (/ stair-w 2))
                              :angle    (/ Math/PI 2)
                              :depth    stair-l
                              :mirrored true})
        (dimension-line :stairs-0-W :house-E-0 {:align :inside})

        ;; utility room
        (add-walls :interior
                   [[:x :stairs-0-N :y :stairs-0-W :utility-N-0]
                    [:y (+ utility-l iw2) :utility-W-0]
                    [:x :house-S-0]])
        (create-room :around [:utility-N-0 :S] :floor concrete)
        (add-door door :utility-N-0 (-> 80 (/ 2) (+ 10) (* -1)) {:width 80})
        (dimension-line :utility-W-0 :stairs-0-W {:align :inside})
        (dimension-line :utility-N-0 :house-S-0 {:align :inside})


        ;; bath room
        (add-walls :interior
                   [[:x :utility-N-0 :y :utility-W-0 :bath-N-0]
                    [:y :house-W-0 ]])
        (create-room :around [:bath-N-0 :S] :floor bath-floor)
        (add-door door :bath-N-0 50 {:width 80})
        (dimension-line :house-W-0 :utility-W-0 {:align :inside})


        (create-room :around [:bath-N-0 :N] :floor wood-floor)
        (add-door door :house-W-0 (-> bed-l (+ 150) (* -1)))
        (dimension-line :house-W-0 :house-E-0 {:align :inside :offset (+ utility-w 30)})
        (dimension-line :bedroom-0 :utility-N-0 {:align :inside})

        )

      ;; ========== MAIN LEVEL ==========
#_(when false
    (add-level "main" {:elevation 300})

    (let [greenhouse-w  300
          living-w      700
          kitchen-w     400
          pantry-w      200           
          pantry-l      375
          bath-w        110
          bath-l        200
          mud-l         bath-l
          mud-w         bath-w
          hall-w        (+ 125 (-> defaults :wall :interior :thickness))
          garage-w      700
          master-hall-w 100
          master-w      500
          master-l      900
          master-bath-l 400]

        ;; exterior walls
      (add-walls :exterior {:closed? true}
                   [[:x 0    :y 0  :house-E-1]
                    [:x hl         :house-S-1]
                    [        :y hw :house-W-1] ; cut out for hall to master wing
                    [:x #(- % (- mud-l iw2) ) :thickness 0.5 :house-hall-temp]
                    [:x #(- % (+ master-hall-w iw2)) :house-hall-W]
                    [:x 0          :house-N-1]])
      (if-let [room (create-room :around [:house-E-1 :W])]
          (doto room
            (.setAreaYOffset 140)
            (.setAreaXOffset -400)
            (.setName "Main Floor")
            (.setNameYOffset 100)
            (.setNameXOffset -400)))

        ;; greenhouse
        (add-walls :interior
                   [[:x greenhouse-w :y 0 :greenhouse]
                    [       :y hw           ]])
        (create-room :around [:greenhouse :N] :floor gray-stone-floor)
        #_ (let [furniture (sh/load-furniture "Doors and windows" "eTeks#doorFrame")]
          (sh/add-door furniture :house-hall-W -100))
        #_ (let [furniture (sh/load-furniture "Doors and windows" "OlaKristianHoff#window_shop")]
          (sh/add-door furniture :house-hall-W -300))

        #_ (let [furniture (sh/load-furniture "Staircases" "OlaKristianHoff#stair_straight_open_stringer")]
             (sh/add-furniture furniture {:x 300 :y 300}))

        ;; living room
        (add-walls :virtual
                   [[:x `(+ (dim :greenhouse) ~living-w) :y 0 :living]
                    [:y hw]])
        (create-room :around [:greenhouse :S] :floor wood-floor)

        ;; kitchen
        (add-walls :virtual
                   [[:x `(+ (dim :living) ~kitchen-w) :y 0 :kitchen]
                    [:y pantry-l]
                    [:x #(- % kitchen-w)]
                    [:y 0]])
        (create-room :around [:kitchen :N] :floor kitchen-floor)

        ;; pantry
        (add-walls :interior
                   [[:x :kitchen :y 0]
                    [:y pantry-l]
                    [:x #(+ % pantry-w) :pantry-S]
                    [:y 0]])
        (create-room :around [:pantry-S :N] :floor wood-floor)

        ;; laundry/storage
        (add-walls :interior
                   [[:x :pantry-S :y pantry-l :laundry-W]
                    [:x hl]])
        (create-room :around [:pantry-S :S] :floor kitchen-floor)

        ;; mud
        (add-walls :interior
                   [[:x (-> hl
                            (- (/ master-w 2))
                            (+ (/ master-hall-w 2) iw2))
                     :y hw :mud]
                    [:y #(- % mud-w)]])
        (add-walls :virtual
                   [[:x :mud :y (- hw bath-w)]
                    [:y :laundry-W]])
        (create-room :around [:mud :S] :floor gray-stone-floor)

        ;; bath
        (add-walls :interior
                   [[:x (- hl (/ master-w 2) (/ master-hall-w 2) iw2)
                     :y hw :bath-S-1]
                    [:y #(- % bath-w)]
                    [:x #(- % bath-l) :bath-N-1]
                    [:y hw]
                    ])
        (create-room :around [:bath-N-1 :S] :floor kitchen-floor)

        ;; hallway
        (create-room :around [:mud :N] :floor wood-floor)

        ;; garage
        (add-walls :exterior
                   [[:x :house-S-1 :y hw]
                    [:x #(+ % garage-w) :garage-S ]
                    [:y 0]])
        (add-walls :virtual
                   [[:x :house-S-1 :y 0]
                    [:x :garage-S]])
        (create-room :around [:house-S-1 :S] :floor concrete)

        ;; ===== MASTER WING =====
        ;; Exterior walls
        (add-walls :exterior
                   [[:x hl :y hw :master-S]
                    [:y #(+ % master-l)]
                    [:x #(- % master-w) :master-N]
                    [:y hw]])
        (if-let [room (create-room :around [:house-hall-temp :W])]
          (doto room
            (.setAreaYOffset 120)
            (.setName "Master Wing")
            (.setNameYOffset 80)))

        ;; bath
        (add-walls :interior
                   [[:x :bath-S-1 :y hw :master-bath-S]
                    [:y #(+ % master-bath-l)]
                    [:x :master-N]])
        (create-room :around [:master-bath-S :N] :floor bath-floor)

        ;; closet
        (add-walls :interior
                   [[:x :mud :y hw :master-closet-N]
                    [:y #(+ % master-bath-l)]
                    [:x :master-S]])
        (create-room :around [:master-closet-N :S] :floor wood-floor)

        ;; hall
        (add-walls :virtual
                   [[:x :bath-S-1 :y (+ hw master-bath-l) :master-hall-W]
                    [:x :mud]])
        (create-room :around [:house-hall-temp :W] :floor wood-floor)

        ;; bedroom
        (create-room :around [:master-hall-W :W] :floor wood-floor)
)
        )


;; ========== TOP LEVEL ==========
#_      (add-level "top" {:elevation 600})

#_      (let [peak {:x      1900
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
#_      (add-walls :interior
                      ;; loft start x
                 [[:x 1110 :y 0  :height 10 :loft-N]
                  [        :y hw                   ]])

      ;; stair
#_      (add-walls :interior
                                        ;; loft width
                 [[:x '(+ (dim :loft-N) 370) :y 0        :stair-N-2]
                  [                          :y (/ hw 2)           ]])

      ;; closet
#_      (add-walls :interior
                 [[:x :stair-N-2 :y hw]
                            ;; closet width
                  [:y (- hw 190)]
                            ;; stair width
                                ;; closet width
                  [:x #(+ % 264 230) :closet-S-2]
                  [:y hw]
                  ])
#_      (create-room :around [:closet-S-2 :N] :floor wood-floor)

      ;; loft room ends at stair wall
#_      (add-walls :virtual
                 [[:x :stair-N-2 :y (/ hw 2) :loft-temp]
                  [              :y :closet-S-2]])
#_      (create-room :around [:loft-temp :N] :floor wood-floor)


      ;; bath
#_      (add-walls :interior
                                           ;; stair width
                 [[:x '(+ (dim :stair-N-2) 264) :y 0       :bath-N-2]
                  [                             :y (/ hw 2)]
                  [:x :closet-S-2]
                  [:y 0]])
#_      (create-room :around [:bath-N-2 :S] :floor bath-floor)

      ;; bedroom
#_      (add-walls :interior
                                            ;; bedroom width
                 [[:x '(+ (dim :closet-S-2) 320) :y 0 :bed-2-S]
                  [:y hw]])

#_      (create-room :around [:bed-2-S :N] :floor wood-floor)
#_      (create-room :around [:bed-2-S :S] :floor wood-floor)
;;      (create-room :around [:loft-temp :S] :floor plywood)
#_      (delete-wall :loft-temp)


      (sh/delete-walls #(= :virtual (:type %)))
#_      (delete-wall :house-hall-temp)

      (sh/set-walls-alpha 0.0)
#_      (sh/set-level "main")
      ))

  (invoke-ui build))
