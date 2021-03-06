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
          hl               2000
          wood-floor       {:texture ["Floor" "eTeksScopia#english-parquet-1" :angle 90]}
          bath-floor       {:texture ["Floor" "OlaKristianHoff#beige_tiles"]}
          kitchen-floor    {:texture ["Floor" "OlaKristianHoff#beige_brown_tiles"]}
          concrete         {:texture ["Wall"  "eTeksScopia#concrete-wall-1"]}
          gray-stone-floor {:texture ["Floor" "eTeksScopia#multi-grey-stones-floor-tiles"]}
          door             (load-furniture "Doors and windows" "eTeks#doorFrame")
          stair            (load-furniture "Staircases" "OlaKristianHoff#stair_straight_open_stringer")
          stair-w          100
          stair-l          500
          stair-h          400
          stair-x          1000]

      ;; ========== BOTTOM LEVEL ==========
      (add-level "bottom" {:elevation 0})

      (let [hl-0         1400
            bed-l        500
            bed-entry-w  120
            bed-closet-w 75
            living-w     400
            bath-w       200
            bath-l       300
            utility-w    300
            utility-l    400]
        ;; exterior walls
        (add-walls :exterior {:closed? true}
                   [[:x 0    :y 0  :house-E-0]
                    [:x hl-0       :house-S-0]
                    [        :y hw :house-W-0]
                    [:x 0          :house-N-0]])
        (if-let [room (create-room :around [:house-E-0 :W])]
          (doto room
            (.setAreaXOffset 0)
            (.setName "Bottom Floor")
            (.setNameXOffset 0)
            (.setNameYOffset -150)
            (.setAreaYOffset -120)))
        (dimension-line :house-E-0 :house-W-0 {:align :outside})
        (dimension-line :house-N-0 :house-S-0 {:align :outside})
        (add-door door :house-W-0 (-> bed-l (+ 100 (/ 90 2))(* -1)))
        
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
                     [[:x `(- (dim :bedroom-0) ~bed-closet-w) :y `(+ (dim :bedroom-sep-0) (-> ~hw (/ 2) (- ~bed-entry-w) (* ~i)))]
                      [:x :bedroom-0]])
          (add-door door :bedroom-0 (+ (/ hw 2) (* i (- (/ hw 2) (/ (+ ew2 bed-entry-w) 2)))) {:width 80}))
        (dimension-line :house-E-0 :bedroom-sep-0 {:align :inside})
        (dimension-line :house-N-0 :bedroom-0 {:align :inside})

        ;; living room
        (add-walls :interior
                   [[:x `(+ (dim :bedroom-0) ~living-w) :y :house-E-0 :living-S-0]
                    [:y :house-W-0]])
        (create-room :around [:living-S-0 :N] :floor wood-floor :name "living")
        (dimension-line :house-W-0 :house-E-0 {:align :inside :offset (+ bed-l 30)})
        (dimension-line :bedroom-0 :living-S-0 {:align :inside})

        ;; stair
        (add-walls :interior
                   [[:x :house-S-0 :y (+ ew2 stair-w iw2) :stairs-W-0]
                    [:x :living-S-0]])
        (add-furniture stair {:x        (+ stair-x (/ stair-l 2))
                              :y        (+ ew2 (/ stair-w 2))
                              :angle    (/ Math/PI 2)
                              :depth    stair-l
                              :height   stair-h
                              :width    stair-w
                              :mirrored true})
        (add-door door :living-S-0 (+ ew2 stair-w iw 45 (/ 80 2)))
        ;;                                       gap for panel on E wall
        (dimension-line :stairs-W-0 :house-E-0 {:align :inside})

        ;; bath room
        (add-walls :interior
                   [[:x :living-S-0 :y (- hw bath-w) :bath-E-0]
                    [:x #(+ % bath-l) :bath-S-0]
                    [:y hw]])
        (create-room :around [:bath-E-0 :W] :floor bath-floor :name "bath")
        (add-door door :living-S-0 (- hw ew2 10 (/ 80 2)) {:width 80})
        (dimension-line :bath-E-0 :house-W-0 {:align :inside})
        (dimension-line :bath-S-0 :living-S-0 {:align :inside :offset 160})

        ;; utility room
        (create-room :around [:living-S-0 :S] :floor concrete :name "utility")
        (add-door door :living-S-0 (+ ew2 (/ stair-w 2)))
        (dimension-line :living-S-0 :house-S-0 {:align :inside :offset 150})
        (dimension-line :house-W-0 :stairs-W-0 {:align :inside})
        (dimension-line :bath-S-0 :house-S-0 {:align :inside})
        )

      ;; ========== MAIN LEVEL ==========
      (add-level "main" {:elevation 300})

      (let [
            ;; length blocks
            greenhouse-w    300
            living-w        700
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
        (add-door door :kitchen-S (+ 65 ew2 10 (/ 70 2)))
        (dimension-line :kitchen-N :kitchen-S {:align :center})
        (dimension-line :kitchen-E :house-W-1 {:align :inside})

        ;; pantry
        (add-walls :interior
                   [[:x :kitchen-S :y (- hw pantry-l) :pantry-E]
                    [:x #(+ % pantry-w) :pantry-S]
                    [:y hw]])
        (create-room :around [:pantry-S :N] :floor kitchen-floor :name "pantry")
        (dimension-line :kitchen-S :pantry-S {:align :center})
        (add-door door :pantry-E :center)

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
        (create-room :around [:master-bath-N :N] :floor wood-floor :name "hall/closet")
        (dimension-line :pantry-S :master-bath-N {:align :center})
        (add-door door :master-closet-E (/ master-closet-w 2))
        (add-door door :master-closet-W (/ master-closet-w 2))

        ;; bedroom
        (create-room :around [:master-closet-W :W] :floor wood-floor :name "bedroom")
        (add-door door :master-N-1 (/ master-w 2))

        ;; guest bath
        (add-walls :interior [[:x (- hl guest-bath-w) :y 0 :guest-bath-N]
                              [:y :master-bath-E]])
        (create-room :around [:guest-bath-N :S] :floor gray-stone-floor :name "bath")
        (dimension-line :guest-bath-N :house-S-1 {:align :center})
        (dimension-line :master-bath-E :house-E-1 {:align :center :offset 75})
        (add-door door :guest-bath-N :center {:width 70})

        ;; entry
        (add-door door :house-E-1 (-> master-closet-w (/ 2) (+ master-bath-w) (* -1)) {:width 90})
        (add-walls :virtual [[:x :pantry-S :y 0 :entry-N] [:y :master-bath-E]])
        (create-room :around [:entry-N :S] :floor gray-stone-floor :name "entry")

        ;; hallway
        (create-room :around [:entry-N :N] :floor wood-floor :name "hall")

        ;; stair
        (add-furniture stair {:x        (+ stair-x (/ stair-l 2))
                              :y        (+ ew2 (/ stair-w 2))
                              :angle    (/ Math/PI 2)
                              :depth    stair-l
                              :width    stair-w
                              :height   stair-h
                              :mirrored true})

        ;; car port
        #_ (add-walls :exterior
                   [[:x :entry-N :y 0 :height master-h]
                    [:y #(- % 500)]])

        ;; deck
        (add-walls :virtual [[:x :house-N-1 :y :house-W-1 :deck-N]
                             [:y #(+ % deck-w) :deck-W]
                             [:x '(- (dim :master-N-1) 120)]
                             [:y :master-W-1]
                             [:x :master-N-1]])

        (create-room :around [:house-W-1 :W] :floor gray-stone-floor :name  "deck")
        (dimension-line :deck-W :house-W-1 {:align :center})
        (dimension-line :deck-W :house-E-1 {:align :outside})
        )


      ;; ========== LOFT ==========
      (add-level "loft" {:elevation 600})
      (let [peak-x      1400
            peak-h      370
            s-wall-h    250
            front-angle (Math/atan2 peak-h peak-x)
            loft-start  1000
            loft-w      400
            loft-N-h    (-> front-angle Math/tan (* loft-start))]
        (add-walls :exterior {:closed? true}
                   [[:x 0 :y 0 :height 0 :house-E-2]
                    [:x peak-x :height peak-h]
                    [:x :house-S-1 :height s-wall-h]
                    [:y :house-W-0]
                    [:x peak-x :height peak-h]
                    [:x 0 :height 0]])
        (add-walls :interior
                   [[:x (+ loft-start loft-w) :y hw :height peak-h :office-N]
                    [:y 200]
                    [:x #(+ % 200) :height (- peak-h (* 200 (/ (- peak-h s-wall-h) 600))) :maker-hall]
                    [:y 0]])
        (add-door door :maker-hall (-> 80 (/ 2) (+ ew2 10) (* -1)))

        (add-walls :virtual
                   [[:x loft-start :y 0 :height loft-N-h :loft-N]
                    [:y hw]])
        (create-room :around [:loft-N :S] :floor wood-floor :name "loft")
        (create-room :around [:office-N :S] :floor wood-floor :name "maker space")
        #_ (dimension-line :house-N-1 :loft-N {:align :center})
        #_ (dimension-line :loft-N :house-S-1 {:align :center :from :end})
        #_ (dimension-line :house-W-1 :house-E-1 {:align :center :from :end})
        )

      (sh/delete-walls #(= :virtual (:type %)))

      (sh/set-walls-alpha 0.0)
      #_ (sh/set-level "bottom")
      #_ (sh/set-level "main")
      ))

  (invoke-ui build))
