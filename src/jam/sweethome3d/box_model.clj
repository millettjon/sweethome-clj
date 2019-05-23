(ns jam.sweethome3d.box-model
  (:require [jam.sweethome3d :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is are]]))

(defmacro when-sh
  "Executes body only when connected to Sweethome3D."
  [& body]
  `(when (sh/connected?)
     ~@body))

(defn p+
  "Adds points"
  [& points]
  (apply merge-with + points))

(defn p-
  "Adds points"
  [& points]
  (apply merge-with - points))

(defn points-to-array
  [points]
  (reduce (fn [v p] (conj v [(p :x) (p :y)])) [] points))

(defn points-map-to-list
  [points]
  (map #(->> % (into []) flatten)
       points))
#_ (points-map-to-list [{:x 0 :y 0}])
#_ (points-map-to-list [nil {:x 0 :y 0} {:x 1 :y 1}])
#_ (points-map-to-list (filter identity [nil {:x 0 :y 0} {:x 1 :y 1}]))

(defn nilify-partial-points
  "Replace points with less than 2 dimensions with nil."
  [points]
  (let [f (fn [p] (if (= 2 (count p))
                    p
                    nil))]
    (->> points
         (map f)
         (into []))))
#_ (let [points [{:y 160} {:x 300, :y 160} {:x 300, :y 250} {:y 250}]]
      points(nilify-partial-points points))


;; get fill direction, and inherited width, length, and points
(defn draw-box
  [{:keys [width length d] :as box} {:keys [parent sibling] :as context}]
  (if (and parent (:points parent))
    (do
      (assert d)
      (let [fill          (:fill parent)
            _             (assert fill)
            use-sibling?  (and sibling (not= :wall (::type sibling)))
            points        (:points (if use-sibling?
                                     sibling
                                     parent))
            _             (assert points)
            [p0 p1 p2 p3] points]
        (if use-sibling?
          ;; fill from sibling points (note walls have only start and end points)
          (case fill
            :right    [p1 (p+ p1 {:x d}) (p+ p2 {:x d}) p2]
            :left     [(p- p0 {:x d}) p0 p3 (p- p3 {:x d})]
            :forward  [p3 p2 (p+ p2 {:y d}) (p+ p3 {:y d})]
            :backward [(p- p0 {:y d}) (p- p1 {:y d}) p1 p0])
          ;; fill from parent points (i.e., this is the first sibling with points)
          (case fill
            :right    [p0 (p+ p0 {:x d}) (p+ p3 {:x d}) p3]
            :left     [(p- p1 {:x d}) p1 p2 (p- p2 {:x d})]
            :forward  [p0 p1 (p+ p1 {:y d}) (p+ p0 {:y d})]
            :backward [(p- p3 {:y d}) (p- p2 {:y d}) p2 p3]))))

    (do
      (assert (not (nil? width)))
      (assert (not (nil? length)))
      [{:x 0 :y 0} {:x length :y 0} {:x length :y width} {:x 0 :y width}])
    ))

(deftest draw-box-test
  (let [points   [{:x 0 :y 0} {:x 200 :y 0} {:x 200 :y 100} {:x 0 :y 100}]
        ctx-box (fn [fill] {:parent {:fill fill :points points}})
        ctx-sibling (fn [fill]
                       (let [context (ctx-box fill)
                             sibling {:d 20}
                             sibling (assoc sibling :points (draw-box sibling context))]
                         (assoc context :sibling sibling)))]
    (are [box context expected] (= (draw-box box context) expected)
      ;; simple box
      {:width 100 :length 200} {}
      points
    
      ;; nested box; fill right
      {:d 50} (ctx-box :right)
      [{:x 0 :y 0} {:x 50 :y 0} {:x 50 :y 100} {:x 0 :y 100}]

      ;; nested box; fill left
      {:d 50} (ctx-box :left)
      [{:x 150 :y 0} {:x 200 :y 0} {:x 200 :y 100} {:x 150 :y 100}]    

      ;; nested box; fill forward
      {:d 50} (ctx-box :forward)
      [{:x 0 :y 0} {:x 200 :y 0} {:x 200 :y 50} {:x 0 :y 50}]

      ;; nested box; fill backward
      {:d 50} (ctx-box :backward)
      [{:x 0 :y 50} {:x 200 :y 50} {:x 200 :y 100} {:x 0 :y 100}]
      )
    
    (are [box context expected] (= (draw-box box context) expected)
      ;; nested box w/ sibling; fill right
      {:d 5} (ctx-sibling :right)
      [{:x 20 :y 0} {:x 25 :y 0} {:x 25 :y 100} {:x 20 :y 100}]

      ;; nested box w/ sibling; fill left
      {:d 5} (ctx-sibling :left)
      [{:x 175 :y 0} {:x 180 :y 0} {:x 180 :y 100} {:x 175 :y 100}]    

      ;; nested box w/ sibling; fill forward
      {:d 5} (ctx-sibling :forward)
      [{:x 0 :y 20} {:x 200 :y 20} {:x 200 :y 25} {:x 0 :y 25}]

      ;; nested box w/ sibling; fill backward
      {:d 5} (ctx-sibling :backward)
      [{:x 0 :y 75} {:x 200 :y 75} {:x 200 :y 80} {:x 0 :y 80}]
      )))

(defn parse-tag
  "Parses a tag into type, id, and class parts."
  [tag]
  (let [[type name classes] (->> tag name
                                 (re-matches #"([^\.#]+?)(?:#([^\.]+))?(?:\.(.*))?") rest)
        m {::type (keyword type)}
        m (if name
            (assoc m :name name)
            m)
        f (fn [m class_]
            (let [class_ (keyword class_)]
              (assert (#{:left :right :front :back} class_))
              (assoc m :on class_)))
        m (if classes
            (reduce f (assoc m :on []) (str/split classes #"\."))
            m)]
    m))

(deftest parse-tag-test
  (are [tag expected] (= expected (parse-tag tag))
    "foo" {::type :foo}
    "foo#bar" {::type :foo :name "bar"}
    "foo.bar" {::type :foo :on ["bar"]}
    "foo.bar.baz" {::type :foo :on ["bar" "baz"]}
    "foo#bar.baz" {::type :foo :name "bar" :on ["baz"]}
    "foo#bar.baz" {::type :foo :name "bar" :on ["baz" "gorp"]}))
#_ (parse-tag "foo#bar")

;; PARSING CALL FLOW
;; parse: form
;;   normalize-element: form context
;;     parse-element: form context     <-- multimethod
;;        parse-children: parent children
;;           normalize-element

;; only root item has both length and width
;; other items have one of the two and inherit the other from the parent
;; 1st child must have :align
;; 2nd siblings use the same :align
;; all children must sum to the same dimensions as the parent

(defmulti parse-element (fn [element context]
                          (::type element)))

#_ (defn normalize-element
  [[tag & args :as element]
   {:keys [parent sibling] :as context}]
  (let [[opts children] (if (map? (first args))
                          [(first args) (rest args)]
                          [{} args])
        m               (parse-tag tag)
        normalized-form (merge opts m {:children children})]
    (parse-element normalized-form context)))

(defn normalize-element
  [[tag & args :as element]
   {:keys [parent sibling] :as context}]
  (let [opts            (apply merge (take-while map? args))
        opts            (or opts {})
        children        (drop-while map? args)
        m               (parse-tag tag)
        normalized-form (merge opts m {:children children})]
    (parse-element normalized-form context)))

(def state
  (atom nil))

(defn parse
  [form]
  (let [context {}
        _       (reset! state {:named-elements {} :unresolved-elements []})
        result  (normalize-element form context)]
    #_ (prn "parse: result=" result)
    #_ (prn "parse: state=" @state)
    ;; (prn "parse: unresolved-elements=" (@state :unresolved-elements))
    ;; (prn "parse: target=" (get-in @state [:named-elements (-> [:hallway :center] first name)]))

    ;; Process unresolved elements.
    (doseq [[element context] (@state :unresolved-elements)]
      (parse-element element context))
    result))

(defn parse-children
  "Parses children of element and returns a list of parsed children."
  [{:keys [children] :as parent}]
  (let [init   {:parent parent :new-children []}
        f      (fn [{:keys [parent new-children] :as r-state} child]
                 (let [new-context {:parent  parent
                                    :sibling (last new-children)}
                       new-child   (normalize-element child new-context)]
                   #_ (prn "parse-children f: child" child "new-child" new-child)
                   (update r-state :new-children conj new-child)))
        result (reduce f init children)]
    (assoc parent :children (result :new-children))))

(defmethod parse-element :level
  [{:keys [name children elevation] :as level}
   {:keys [parent sibling]}]
  (assert (nil? parent))                ; must be a root element
  (assert (nil? sibling))               ; must be a root element
  (assert (#{0 1} (count children)))    ; has at most 1 child
  (assert name "level name required")
  (assert elevation "level elevation required")
  (let [swh-level (when-sh (sh/add-level name {:elevation elevation}))]
    (parse-children level)))

(defmethod parse-element :box
  [{:keys [name children width length d fill room floor] :as box}
   {:keys [parent sibling] :as context}]

  (if (nil? parent)
    (do
      (assert (not (nil? width)))
      (assert (not (nil? length))))
    (if (nil? d)
      (do
        (assert (not (nil? width)))
        (assert (not (nil? length))))))

  (let [points (draw-box box context)
        box    (assoc box :points points)]
    (when name
      (swap! state assoc-in [:named-elements name] box))

    ;; Create sweethome3d objects
    (when-sh
        (when (or name room)
          (let [room (sh/create-room (points-to-array points) :name name :floor floor)
                {:keys [:dy] :as label} (box :label)]
            (if dy
              (doto room
                (.setNameYOffset dy)
                (.setAreaYOffset (+ dy 30)))))
          (sh/dimension-lines-box box)))

    (parse-children box)))

(defmethod parse-element :wall
  [{:keys [type on align d] :as wall}
   {:keys [parent sibling] :as context}]
  (assert (not (nil? parent)))
  (assert (not (nil? on)))
  (let [points (:points parent)
        points (case on
                 ;; A wall uses only 2 of the 4 corner points.
                 ;; Calculate whih 2 keeping all 4 in clockwise order from 1 (upper left) 1 to 4 (lower left).
                 :right (let [[p2 p3 :as v] (subvec points 1 3)
                              [pa pb]       (case align
                                              :back  [p2 (p+ p2 {:y d})]
                                              :front [(p- p3 {:y d}) p3]
                                              nil    v)]
                          [nil pa pb nil])
                 :left  (let [[p4 p1 :as v] [(last points) (first points)]
                              [pa pb]       (case align
                                              :back  [(p+ p1 {:y d}) p1]
                                              :front [p4 (p- p4 {:y d})]
                                              nil    v)]
                          [pb nil nil pa])
                 :back  (let [[p1 p2 :as v] (subvec points 0 2)
                              [pa pb]       (case align
                                              :left  [p1 (p+ p1 {:x d})]
                                              :right [(p- p2 {:x d}) p1]
                                              nil    v)]
                          [pa pb nil nil])
                 :front (let [[p3 p4 :as v] (subvec points 2 4)
                              [pa pb]       (case align
                                              :left  [(p+ p4 {:x d}) p4]
                                              :right [p3 (p- p3 {:x d})]
                                              nil    v)]
                          [nil nil pa pb]))
        wall   (assoc wall
                      :points points
                      :sh (when-sh (first (sh/add-walls type (points-map-to-list (filter identity points))))))]
    (parse-children wall)))

(defmethod parse-element :door
  [{:keys [width height align furniture children] :as door}
   {:keys [parent sibling] :as context}]
  (assert (= (::type parent) :wall))
  (assert (empty? children))

  (let [points (if align
                 (if (keyword? align)
                   (case align
                     ;; align to parent wall
                     :center [(sh/center (parent :points))])
                   (if-let [target (get-in @state [:named-elements (-> align first name)])]
                     ;; align to target object
                     (let [align-to      (second align)
                           pp            (->> parent :points (some identity))
                           [p0 p1 p2 p3] (target :points)]
                       (prn p0 p1 p2 p3)
                       (case align-to
                         :center (case (:on parent)
                                   (:left :right) [{:x (pp :x)
                                                    :y (-> [p0 p3] sh/center :y)}]
                                   (:front :back) [{:x (-> [p0 p1] sh/center :x)
                                                    :y (pp :y)}])))))
                 ;; Use width as distance :d.
                 (let [points (draw-box (assoc door :d width) context)]
                   (nilify-partial-points points)))]
    (if points
      (let [_       (prn "parse-element-door: points" points)
            door    (assoc door :points points)
            sh-door (when-sh
                        (let [wall (parent :sh)]
                          (sh/add-door2 door wall)))
            door    (assoc door :sh sh-door)])
      (do
        (prn "parse-element-door: saving unresolved")
        (swap! state update-in [:unresolved-elements] conj [door context])
        door))))

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
       (let [stone-floor   {:floor {:texture ["Floor" "eTeksScopia#multi-grey-stones-floor-tiles"]}}
             wood-floor    {:floor {:texture ["Floor" "eTeksScopia#english-parquet-1" :angle 90]}}
             bath-floor    {:floor {:texture ["Floor" "OlaKristianHoff#beige_tiles"]}}
             kitchen-floor {:floor {:texture ["Floor" "OlaKristianHoff#beige_brown_tiles"]}}
             ;; concrete         {:texture ["Wall"  "eTeksScopia#concrete-wall-1"]}
             door          (sh/load-furniture "Doors and windows" "eTeks#doorFrame")
             door100       {:width 100 :height 206 :furniture door}
             door90        {:width 90 :height 206 :furniture door}
             door70        {:width 70 :height 206 :furniture door}
             center        {:align :center}
             outside       {:type :outside}]
         (sh/clean-home)
         (parse 
          [:level#main {:elevation 300}
           [:box {:width (+ 620 500) :length 2000 :fill :forward}
            [:box#main-wing {:d     620
                             :fill  :right
                             :label {:dy -400}}

             [:wall.left outside]
             [:wall.back outside
              [:door door100 {:align [:closet :center]}]]
             [:wall.right outside]
             [:wall.front outside {:align :left :d 1600 :fill :right}
              [:box {:d 400}]
              [:door door100]
              [:door door100 {:align [:greenhouse :center]}]]

             [:box#greenhouse {:d 300} stone-floor
              [:wall.right {:fill :forward}
               [:door door90 {:align [:hallway :center]}]]]

             [:box#living-room {:d 700} wood-floor]
             [:box {:d 1000 :fill :forward}
              [:box {:d 220 :fill :left}
               [:box#bath {:d 110} bath-floor
                [:wall.left
                 [:door door70 center]]]
               [:box#mud {:d 290} stone-floor]
               [:box {:d 600 :fill :forward}
                [:box#stair {:d 100} wood-floor
                 ;;[:stair {:d}]
                 ]
                [:box#hallway {:d 120} wood-floor]]]
              [:box {:d 400 :fill :right}
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
             [:box#bedroom {:d 400} wood-floor
              [:wall.back [:door door90 {:align [:closet :center]}]]
              [:wall.right outside]
              [:wall.left outside
               [:door door90 {:align [:deck :center]}]]
              [:wall.front outside]]
             [:box {:d 1600 :fill :forward}
              [:box#deck {:d 300} stone-floor]
              [:box {:d 200 :fill :left}
               [:box#landing {:d 100} stone-floor]]]]]])))))

#_ [:wall.front {:type :outside :fill :right}
    [:segment {:d 1600}] <- wall segment that takes texture etc...
    [:space]
    [:door]
    [:window]
    [:post]
    [:window]
    [:post]
    ]


;; TODO: fix checking that children are < that size of parent
;; TODO: add stairs
;; TODO: add furniture
;; TODO: add windows
;; TODO: add other levels

(deftest create-level-test
  (is (= {:elevation 300, :type :level, :name "main", :children []}
         (parse [:level#main {:elevation 300}]))))

(deftest create-level-with-box-test
  (is (= {:elevation 0, :type :level, :name "bottom",
          :children [{:width 620, :length 2000, :type :box, :name nil,
                      :points [{:x  0 :y 0} {:x 2000 :y 0} {:x 2000 :y 620} {:x 0 :y 620}]
                      :children []
                      }]}
         (parse [:level {:elevation 0}
                 [:box#house {:width 620 :length 2000}]]))))

(deftest nested-box-test
  (is (= {:width  620,
          :length 2000,
          :fill   :right,
          :type   :box,
          :name   nil,
          :children
          [{:d        300,
            :type     :box,
            :name     nil,
            :children [],
            :points
            [{:x 0, :y 0}
             {:x 300, :y 0}
             {:x 300, :y 620}
             {:x 0, :y 620}]}],
          :points
          [{:x 0, :y 0}
           {:x 2000, :y 0}
           {:x 2000, :y 620}
           {:x 0, :y 620}]}
         (parse [:box {:width 620 :length 2000 :fill :right}
                 [:box {:d 300}]]))))

(deftest two-nested-box-test
  (is (= {:width 620,
          :length  2000,
          :fill    :right,
          :type    :box,
          :name    nil,
          :children
          [{:d        300,
            :type     :box,
            :name     nil,
            :children [],
            :points
            [{:x 0, :y 0}
             {:x 300, :y 0}
             {:x 300, :y 620}
             {:x 0, :y 620}]}
           {:d        700,
            :type     :box,
            :name     nil,
            :children [],
            :points
            [{:x 300, :y 0}
             {:x 1000, :y 0}
             {:x 1000, :y 620}
             {:x 300, :y 620}]}],
          :points
          [{:x 0, :y 0}
           {:x 2000, :y 0}
           {:x 2000, :y 620}
           {:x 0, :y 620}]}
         (parse [:box {:width 620 :length 2000 :fill :right}
                 [:box {:d 300}]
                 [:box {:d 700}]]))))

(deftest double-nested-box-test
  (is (= {:width 620,
             :length 2000,
             :fill :right,
             :type :box,
             :name nil,
             :children
             [{:d 300,
               :fill :forward,
               :type :box,
               :name nil,
               :children
               [{:d 100,
                 :type :box,
                 :name nil,
                 :children [],
                 :points
                 [{:x 0, :y 0}
                  {:x 300, :y 0}
                  {:x 300, :y 100}
                  {:x 0, :y 100}]}
                {:d 110,
                 :type :box,
                 :name nil,
                 :children [],
                 :points
                 [{:x 0, :y 100}
                  {:x 300, :y 100}
                  {:x 300, :y 210}
                  {:x 0, :y 210}]}
                {:d 410,
                 :type :box,
                 :name nil,
                 :children [],
                 :points
                 [{:x 0, :y 210}
                  {:x 300, :y 210}
                  {:x 300, :y 620}
                  {:x 0, :y 620}]}],
               :points
               [{:x 0, :y 0}
                {:x 300, :y 0}
                {:x 300, :y 620}
                {:x 0, :y 620}]}
              {:d 700,
               :type :box,
               :name nil,
               :children [],
               :points
               [{:x 300, :y 0}
                {:x 1000, :y 0}
                {:x 1000, :y 620}
                {:x 300, :y 620}]}],
             :points
             [{:x 0, :y 0}
              {:x 2000, :y 0}
              {:x 2000, :y 620}
              {:x 0, :y 620}]}
         (parse [:box {:width 620 :length 2000 :fill :right}
                 [:box {:d 300 :fill :forward} 
                  [:box {:d 100}]
                  [:box {:d 110}]
                  [:box {:d 410}]]
                 [:box {:d 700}]]))))

;; TESTS
;; - setup a test harness to create a model
;; - create a room from points with no walls
;; - create a room with 4 walls
;; - create a room with 1 wall
;; - create 2 room nested in a box
;; - create a doubly nested room
;; - auto create dimension lines when a room is created


;; = is pure data structure
;; = can be composed
;; = no macros needed
;; = turtles all the way down
;; = centers walls

;; ? use hiccup syntax?
;; use relative coordinates

;; wrap -> shrink-wrap fixed space from outside
;; fill -> fill fixed space from inside
[:wrap {:name :outside-wall :thickness 20}

 ;; 1st item is relative to 0, 0
 [:box#house {:width 600 :length 2000}
  [:box#greenhouse {:align  :left ;; infer :move :right (always be filling box)
                    :length 300
                    :walls  {:right [{:thickness 20} [:door {:height 206 :width 90
                                                             :align  [:hallway :left :center]}]]}}]
  [:box#living {:length 700}]
  [:box {:length 700}
   [:box#stair {:align [:left :back] :move :front :width 100}]
   [:box#hallway {:width 110}] ; infer :align :front
   [:box {:width 380}
    [:box#kitchen {:align :left :length 400}] ; <- infer :move :right
    [:box#pantry {:length 200}]]]
  [:box {:length 400} ; <- assert fill is complete
   [:box {:align :back :width 220}
    [:box#entry {:align :left :length 200}]
    [:box#bath {:length 100}]]
   [:box {:width 380}
    [:box#master-closet {:align :left :width 200}]
    [:box#master-bath {:width 200}]]]]

;; ? how to add dimension lines? auto add if is room
;; ? how to add floors? :floor (floors :bath) ; auto makes it a room?
;; ? how to add rooms? :room true (use label as text, add dimension lines)
;; ? is wrap needed? maybe just use boxes all the way down
;; ? setup simple examples w/ devcards?
;; ? handle floors?
;; ? use css nameing for symbols? :foo#bar
;; - calculate and make sure lengths match (when closing each box)

;; = having the box construct add the walls might be able to account for the centering
;; = having walls be on center may help with alignment of framing and plywood

 ;; 2nd item is relative to first etc
 [:box {:name   :master-wing
        :align  [:right :front]
        :move   :front
        :length 400 :width 500}]]


[:wrap {:name :outside-wall :thickness 20}

 ;; 1st item is relative to 0, 0
 [:box#house {:d 600 :length 2000 :fill :right}
  [:box#greenhouse {:d 300
                    :walls  {:right [{:thickness 20} [:door {:height 206 :d 90
                                                            :align  [:hallway :left :center]}]]}}]
  [:box#living {:d 700}]
  [:box {:d 700 :fill :front}
   [:box#stair {:d 100}]
   [:box#hallway {:d 110}]
   [:box {:d 380 :fill :right}
    [:box#kitchen {:d 400}]
    [:box#pantry {:d 200}]]]
  [:box {:d 400 :fill :front}
   [:box {:d 220 :fill :right}
    [:box#entry {:d 200}]
    [:box#bath {:d 100}]]
   [:box {:d 380 :fill :right}
    [:box#master-closet {:d 200}]
    [:box#master-bath {:d 200}]]]]]
