(ns jam.pichilaguna.l-house
  (:require [jam.sweethome3d
             :refer [state clean-home add-level add-walls create-room delete-wall
                     invoke-ui midpoint dim dimension-line load-furniture add-door add-furniture]
             :as sh]
            [jam.sweethome3d.math-wrap :as m]))

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
          hl               1900
          wood-floor       {:texture ["Floor" "eTeksScopia#english-parquet-1" :angle 90]}
          bath-floor       {:texture ["Floor" "OlaKristianHoff#beige_tiles"]}
          kitchen-floor    {:texture ["Floor" "OlaKristianHoff#beige_brown_tiles"]}
          concrete         {:texture ["Wall"  "eTeksScopia#concrete-wall-1"]}
          gray-stone-floor {:texture ["Floor" "eTeksScopia#multi-grey-stones-floor-tiles"]}
          door             (load-furniture "Doors and windows" "eTeks#doorFrame")
          stair            (load-furniture "Staircases" "OlaKristianHoff#stair_straight_open_stringer")
          stair-w          80
          stair-l          500
          stair-x          900]

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
        (create-room :around [:bedroom-sep-0 :E] :floor wood-floor :name "bedroom")
        (create-room :around [:bedroom-sep-0 :W] :floor wood-floor :name "bedroom")

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
        (add-furniture stair {:x        `(-> :house-S-0 dim (- ~utility-w) (+ (/ ~stair-l 2)))
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
        (create-room :around [:utility-N-0 :S] :floor concrete :name "utility")
        (add-door door :utility-N-0 (-> 80 (/ 2) (+ 10) (* -1)) {:width 80})
        (dimension-line :utility-W-0 :stairs-0-W {:align :inside})
        (dimension-line :utility-N-0 :house-S-0 {:align :inside})


        ;; bath room
        (add-walls :interior
                   [[:x :utility-N-0 :y :utility-W-0 :bath-N-0]
                    [:y :house-W-0 ]])
        (create-room :around [:bath-N-0 :S] :floor bath-floor :name "bath")
        (add-door door :bath-N-0 50 {:width 80})
        (dimension-line :house-W-0 :utility-W-0 {:align :inside})


        (create-room :around [:bath-N-0 :N] :floor wood-floor :name "living")
        (add-door door :house-W-0 (-> bed-l (+ 150) (* -1)))
        (dimension-line :house-W-0 :house-E-0 {:align :inside :offset (+ utility-w 30)})
        (dimension-line :bedroom-0 :utility-N-0 {:align :inside})
        )

      ;; ========== MAIN LEVEL ==========
      (add-level "main" {:elevation 300})

      (let [
            ;; length blocks
            greenhouse-w    300
            living-w        600
            kitchen-w       400
            pantry-w        200
            master-closet-w (+ 55 90 55)
            master-bath-w   200
            total-l         (+ greenhouse-w living-w kitchen-w pantry-w master-closet-w master-bath-w)
            _               (assert (= hl total-l) (str "total length mismatch total-l=" total-l))

            ;; width blocks
            master-bath-l 400
            hall-w        110
            guest-bath-w  110
            pantry-l      master-bath-l
            total-w       (+ master-bath-l hall-w guest-bath-w)
            _             (assert (= hw total-w) (str"total width mismatch total-w=" total-w))

            ;; misc
            guest-bath-l 200
            mud-l        guest-bath-l
            mud-w        guest-bath-w
            deck-w       300
            garage-w     400
            master-w     500
            master-l     400
            default-h    (-> defaults :level :height)
            master-h     (+ default-h 50)]

        ;; exterior walls
        (add-walls :exterior {:closed? true}
                   [[:x 0 :y 0 :house-E-1]
                    [:x hl :house-S-1]
                    [:y (+ hw master-w) :master-W-1]
                    [:x #(- % master-l) :height master-h :master-N-1]
                    [:y #(- % master-w) :house-W-1]
                    [:x 0 :height default-h :house-N-1]])
        (dimension-line :house-W-1 :house-E-1 {:align :center :from :end})
        (dimension-line :house-N-1 :house-S-1 {:align :outside :from :end})
        (dimension-line :master-W-1 :house-W-1 {:align :center})
        (if-let [room (create-room :around [:house-E-1 :W])]
          (doto room
            (.setAreaYOffset 0)
            (.setAreaXOffset -400)
            (.setName "Main Floor")
            (.setNameYOffset -40)
            (.setNameXOffset -400)))

        ;; entry door
        (add-door door :house-S-1 (-> hall-w (/ 2) (+ mud-w)) {:width 90})

        ;; greenhouse
        (add-walls :interior [[:x greenhouse-w :y 0 :greenhouse] [:y hw]])
        (create-room :around [:greenhouse :N] :floor gray-stone-floor :name "greenhouse")
        (dimension-line :house-E-1 :house-W-1 {:align :inside})
        (dimension-line :house-N-1 :greenhouse {:align :center})
        (add-door door :greenhouse (+ guest-bath-w (/  hall-w 2) ))
        (add-door door :house-W-1 (- 0 (/ greenhouse-w 2)))

        ;; living room
        (add-walls :virtual
                   [[:x `(+ (dim :greenhouse) ~living-w) :y 0 :living-S]
                    [:y hw]])
        (create-room :around [:greenhouse :S] :floor wood-floor :name "living")
        (dimension-line :greenhouse :living-S {:align :center :from :end})
        (add-door door :house-W-1 (- 0 greenhouse-w (* living-w 1/4)))

        ;; kitchen
        (add-walls :virtual
                   [[:x :living-S :y hw :kitchen-N]
                    [:y #(- % pantry-l) :kitchen-E]
                    [:x #(+ % kitchen-w)]])
        (add-walls :interior
                   [[:x `(-> :living-S dim (+ ~kitchen-w)) :y hw :kitchen-S]
                    [:y :kitchen-E]])
        (create-room :around [:kitchen-N :S] :floor kitchen-floor :name "kitchen")
        (dimension-line :kitchen-N :kitchen-S {:align :center})
        (dimension-line :kitchen-E :house-W-1 {:align :inside})

        ;; pantry
        (add-walls :interior
                   [[:x `(-> :kitchen-S dim (+ ~pantry-w)) :y hw :pantry-S]
                    [:y #(- % pantry-l)]])
        (add-walls :virtual
                   [[:x :kitchen-S :y :kitchen-E]
                    [:x :pantry-S]])
        (create-room :around [:pantry-S :N] :floor kitchen-floor :name "pantry")
        (dimension-line :kitchen-S :pantry-S {:align :center})

        ;; master bath
        (add-walls :interior
                   [[:x hl :y hw]
                    [:x #(- % master-bath-w) :master-bath-N]
                    [:y #(- % master-bath-l) :master-bath-E]
                    [:x hl]])
        (create-room :around [:master-bath-N :S] :floor bath-floor :name "bath")
        (dimension-line :master-bath-N :house-S-1 {:align :center})
        (add-door door :master-bath-N (/ master-bath-l 2))

        ;; closet
        (add-walls :interior [[:x :master-bath-N :y :master-bath-E :master-closet-E] [:x :pantry-S]])
        (add-walls :interior [[:x :master-bath-N :y hw :master-closet-W] [:x :pantry-S]])
        (create-room :around [:master-bath-N :N] :floor wood-floor :name "closet")
        (dimension-line :pantry-S :master-bath-N {:align :center})
        (add-door door :master-closet-E (/ master-closet-w 2))
        (add-door door :master-closet-W (/ master-closet-w 2))

        ;; bedroom
        (create-room :around [:master-closet-W :W] :floor wood-floor :name "bedroom")
        (add-door door :master-N-1 (/ master-w 2))

        ;; entry/mud
        (add-walls :interior [[:x (- hl mud-l) :y 0 :mud-N] [:y mud-w]])
        (add-walls :virtual [[:x :mud-N :y mud-w] [:y :master-bath-E]])
        (create-room :around [:mud-N :S] :floor gray-stone-floor :name "entry")
        
        ;; guest bath
        (add-walls :interior [[:x :mud-N :y guest-bath-w :guest-bath-W]
                              [:x #(- % guest-bath-l)]
                              [:y 0]])
        (create-room :around [:guest-bath-W :E] :floor bath-floor :name "bath")
        (dimension-line :guest-bath-W :house-E-1 {:align :center})
        (dimension-line :master-bath-E :guest-bath-W {:align :center})
        (add-door door :guest-bath-W (-> 70 (/ 2) (+ 15) (* -1)) {:width 70})

        ;; hallway
        (create-room :around [:guest-bath-W :W] :floor wood-floor :name "hall")

        ;; stair
        (add-furniture stair {:x        (+ stair-x (/ stair-l 2))
                              :y        (+ ew2 (/ stair-w 2))
                              :angle    (/ Math/PI 2)
                              :depth    stair-l
                              :mirrored true})

        ;; deck
        (add-walls :virtual [[:x :house-N-1 :y :house-W-1 :deck-N]
                             [:y #(+ % deck-w) :deck-W]
                             [:x :master-N-1]])
        (create-room :around [:house-W-1 :W] :floor gray-stone-floor :name  "deck")
        (dimension-line :deck-W :house-W-1 {:align :center})
        (dimension-line :deck-W :house-E-1 {:align :outside})

        ;; garage
        (add-walls :virtual
                   [[:x :house-S-1 :y 0 :garage-E]
                    [:x #(+ % garage-w) :garage-S]
                    [:y hw]
                    [:x :house-S-1]])
        (create-room :around [:house-S-1 :S] :floor concrete :name "car port")
        (dimension-line :house-S-1 :garage-S {:align :center})
        )


      ;; ========== LOFT ==========
      (add-level "loft" {:elevation 600})
      (let [peak-x      1700
            peak-h      325
            s-wall-h    200
            front-angle (Math/atan2 peak-h peak-x)
            loft-start  900
            loft-N-h    (-> front-angle Math/tan (* loft-start))]
        (add-walls :exterior {:closed? true}
                   [[:x 0 :y 0 :height 0 :house-E-2]
                    [:x peak-x :height peak-h]
                    [:x :garage-S :height s-wall-h]
                    [:y :house-W-0]
                    [:x peak-x :height peak-h]
                    [:x 0 :height 0]])
        (add-walls :interior
                   [[:x loft-start :y 0 :height loft-N-h :loft-N]
                    [:y hw]])
        (create-room :around [:loft-N :S] :floor wood-floor :name "loft")
        (dimension-line :house-N-1 :loft-N {:align :center})
        (dimension-line :loft-N :garage-S {:align :center :from :end})
        (dimension-line :house-W-1 :house-E-1 {:align :center :from :end})
        )

      (sh/delete-walls #(= :virtual (:type %)))

      (sh/set-walls-alpha 0.0)
      (sh/set-level "main")
      ))

  (invoke-ui build))
