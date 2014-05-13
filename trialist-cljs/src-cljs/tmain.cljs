(ns trialist.tmain
 (:require [dommy.core :as dom]
           [goog.dom :as gdom]
           [clojure.string :as cstr]
           [trialist.net :as net]
           [goog.net.cookies :as cookie])
 (:use-macros
  [dommy.macros :only [sel1 sel node]]))

(def data (atom {}))
(def metadata (atom {}))
(def results (atom {}))

(defn remove! [selector]
 (if-let [item (sel1 selector)]
   (dom/remove! item)))

(def url "/app/")
(def client "preempt-viz")
(declare signin)

(def auth-token (cookie/get "auth_token"))

(declare load-data)

(dom/listen! (sel1 :#choose-patient) :submit
   (fn [evt]
     (.preventDefault evt)
     (let [patient  (sel1 :#patient)
           patient  (dom/value patient)]
           (load-data auth-token patient))))

  (dom/listen! (sel1 :#outcome-summary-button) :click
    (fn [evt]
      (let [btn (sel1 :#outcome-summary-button)
	          txt (dom/text btn)
            txt (if (= txt "Outcome Summary")
                    (str "Hide " txt)
                    "Outcome Summary")
            hgt (if (= txt "Outcome Summary") 1458 1900)]
        (dom/set-style! (sel1 :#svg-wrapper) :height (str hgt "px"))
        (dom/set-text! btn txt))))

(defn signin [username password patient]
  ;(dom/set-style! (sel1 :#loading) :opacity "1")
  (dom/set-text! (sel1 :#msg) "Signing In...")
  (net/POST
   (str url "user/auth_token")
     {:user username
      :password password
      :client client}
     (fn [res]
       (.log js/console (pr-str res))
       (if (= (get res :result) "failure")
           (dom/set-text! (sel1 :#msg) (get-in res [:errors 0 :text]))
           (load-data (get res :token)
                      (if patient patient username))))))

(declare render)

(defn load-data [token patient]
  (dom/set-text! (sel1 :#msg) "Loading Data...")
  (dom/set-text! (sel1 "#username tspan") patient)
  (net/POST
   (str url "stream/read")
   {:auth_token token
    :client client
    :observer_id "io.omh.trialist"
    :observer_version "2013013000"
    :stream_id "data"
    :stream_version "2013013000"
    :username patient
    :chronological false}
   (fn [res]
     (if (= (get res :result) "failure")
       (dom/set-text! (sel1 :#msg) (get-in res [:errors 0 :text]))
       (do
         (reset! data (get-in res [:data 0 :data :data]))
	 (.log js/console (pr-str @data))
         (reset! metadata (get-in res [:data 0 :data :metadata]))
	 (.log js/console (pr-str @metadata))
         (net/POST (str url "stream/read")
                   {:auth_token token
                    :client client
                    :observer_id "io.omh.trialist"
                    :observer_version "2013013000"
                    :stream_id "results"
                    :stream_version "2013013000"
                    :username patient
                    :chronological "false"}
                   (fn [res2]
                     (if (= (get res2 :result) "failure")
                         (dom/set-text! (sel1 :#msg) (get-in res2 [:errors 0 :text]))
                         (do
                           (reset! results res2)
	 		   (.log js/console (pr-str @results))
                           (render))))))))))

(defn labels []
  (dom/set-text! (sel1 "#treatment-a tspan")
                 (get-in @metadata [:regimen_a 0]))
  (dom/set-text! (sel1 "#treatment-b tspan")
                 (get-in @metadata [:regimen_b 0]))
  (dom/set-text! (sel1 "#date tspan")
                 (str "From: " (get @metadata :trial_start_date)
                      " To: "  (get @metadata :trial_end_date))))

(defn setup-grid []
  (let [cycles (get @metadata :number_of_cycles)
        c2      "#cycles-2"
        c3      "#cycles-3"
        c4      "#cycles-4"
        new    (cond (= cycles 2)
                     (do (remove! c3) (remove! c4))
                     (= cycles 3)
                     (do (remove! c2) (remove! c4))
                     (= cycles 4)
                     (do (remove! c2) (remove! c3)))]))

(defn pairs []
  (let [cycle-ab-pairs (get @metadata :cycle_ab_pairs)
        prs (cstr/split cycle-ab-pairs  "")
        prs (filter (fn[v] (not= v ",")) prs)]
        (vec prs)))

(defn ab-pairs []
  (let [cycles (get @metadata :number_of_cycles)
        pairs (pairs)]
    (doseq [i (range 1 (inc (count pairs)))]
       (let [reg (get pairs (dec i))
             clr (if (=  "A" reg) "#ff0000" "#000080")
             id (str "#c" cycles "-p" i " tspan")
             elm (sel1 id)]
         (if elm
             (-> (sel1 id)
                 (dom/set-text! reg)
                 (dom/set-style! :fill clr)))))))

(defn clone-ids [elm]
  (if-let [id (dom/attr elm :id)]
    (dom/attr! elm :id (str id "-clone")))
  (doseq [child (gdom/getChildren elm)]
    (clone-ids child)))

(defn clone-grid []
 (let [grid      (sel1 "#cycles-2")
       grid      (if (nil? grid) (sel1 "#cycles-3") grid)
       grid      (if (nil? grid) (sel1 "#cycles-4") grid)
       clone     (.cloneNode grid true)]
  ;(clone-ids clone)
   (dom/set-attr! clone :transform "translate(-32.409, 464)")
   (dom/append! (sel1 :#svg) clone)))

(defn x-time [o]
  (let [timestamp (get o :timestamp)
        ;start-date (get @metadata :trial_start_date)
        start-date (get @metadata :trial_start_date)
        start-epoc (.parse js/Date start-date)
        end-date (get @metadata :trial_end_date)
        end-epoc (.parse js/Date end-date)
        span (- end-epoc start-epoc)
        ts (.parse js/Date timestamp)
        ratio (/ (- end-epoc ts) span)
        ratio (- 1 ratio)]
    (* 260 ratio)))

(defn y-coord
  ([o k]
   (let [n (get o k)]
     (y-coord n)))
  ([n]
   (let [n (- 10 n)
         n (/ n 10)]
     (* 267 n))))

(defn before-end-date [datum]
  (< (.parse js/Date (get datum :timestamp))
     (.parse js/Date (get @metadata :trial_end_date))))

(def svg-ns "http://www.w3.org/2000/svg")

(defn svg-node [node-type]
  (let [svg (sel1 :#svg)
        svg-elm (.createElementNS js/document svg-ns node-type)]
    svg-elm))

(defn get-clr [reg1 reg2]
  (cond
   (and (= reg1 "A") (= reg2 "A")) "red"
   (and (= reg1 "B") (= reg2 "B")) "#000080"
   (and (= reg1 "A") (= reg2 "B")) "url(#red-blue)"
   (and (= reg1 "B") (= reg2 "A")) "url(#blue-red)"))

(defn graph-1[]
  (let [cnt (count @data)
        g (svg-node "g")]
    (dom/set-attr! g :id "graph-1"
                     :transform "translate(57, 193)")
    (doseq [i (range 1 (count @data))]
      (if-let [n (get @data (dec i))]
          (let [o (get @data i)
                line (svg-node "line")]
                (-> line (dom/set-attr! "x1" (x-time n)
                                        "y1" (y-coord n :averagePainIntensity)
                                        "x2" (x-time o)
                                        "y2" (y-coord o :averagePainIntensity))
                         (dom/set-style! "stroke" (get-clr (get n :regimen) (get o :regimen)))
                         (dom/set-style! "stroke-width" 2))
                 (dom/append! g line))))
    (dom/append! (sel1 :#svg) g)))

(defn graph-2-a []
  (let [data-a (vec (filter (fn[i] (= (get i :regimen) "A")) @data))
        cnt (count data-a)
        w (+ 135 (/ 135 cnt))
        g (svg-node "g")]
        (-> g (dom/set-attr! :id "graph-2-a"
                             :transform "translate(421, 193)"))
        (doseq [i (range 1 (count data-a))]
          (if-let [n (get  data-a (dec i))]
            (let [o (get  data-a i)
                  line (svg-node "line")]
              (-> line (dom/set-attr! "x1" (* w (/ (dec i) cnt))
                                      "y1" (y-coord n :averagePainIntensity)
                                      "x2" (* w (/ i cnt))
                                      "y2" (y-coord o :averagePainIntensity))
                       (dom/set-style! "stroke" "red")
                       (dom/set-style! "stroke-width" 2))
              (dom/append! g line))))
   (dom/append! (sel1 :#svg) g)))

(defn graph-2-b []
  (let [data-b (vec (filter (fn[i] (= (get i :regimen) "B")) @data))
        cnt (count data-b)
        w (+ 135 (/ 135 cnt))
        g (svg-node "g")]
        (-> g (dom/set-attr! :id "graph-2-b"
                             :transform "translate(556, 193)"))
        (doseq [i (range 1 (count data-b))]
          (if-let [n (get  data-b (dec i))]
            (let [o (get  data-b i)
                  line (svg-node "line")]
              (-> line (dom/set-attr! "x1" (* w (/ (dec i) cnt))
                                      "y1" (y-coord n :averagePainIntensity)
                                      "x2" (* w (/ i cnt))
                                      "y2" (y-coord o :averagePainIntensity))
                       (dom/set-style! "stroke" "#000080")
                       (dom/set-style! "stroke-width" 2))
              (dom/append! g line))))
   (dom/append! (sel1 :#svg) g)))

(def conversion-rates
     {:cognitiveFunctionWorkingHarderPrompt (fn [v] (* 2.5 (- v 1)))
      :cognitiveFunctionFoggyThinkingPrompt (fn [v] (* 2.5 (- v 1)))
      :fatiguePrompt (fn [v] (* 2.5 (- v 1)))
      :constipationPrompt (fn [v] (* 2.5 (- v 1)))
      :drowsinessPrompt (fn [v] (* 2 (- v 1)))
      :sleepDisturbancePrompt (fn [v] (* 2.5 (- 5 v))) })

(defn ave [symptom reg]
  (let [dat (filter (fn [i] (= reg (get i :regimen))) @data)
        dat (map (fn [i] (get i symptom)) dat)
	ave (/ (reduce + dat) (count dat))
	rate (get conversion-rates symptom)
        rate (if rate rate 1)
	ave (if (fn? rate) (rate ave) (* rate ave))]
    ave))

(defn rnd-ave [symptom reg]
  (.round js/Math (ave symptom reg)))

(defn pain-ave [reg cyc]
  (let [dat (filter (fn [i] (and (= reg (get i :regimen))
                                 (= cyc (get i :cycle)))) @data)
        dat (map (fn [i] (get i :averagePainIntensity)) dat)
        ave (/ (reduce + dat) (count dat))] ave))

(defn pain-aves [cycles]
  (cond
   (= cycles 1)
    {:A1 (pain-ave "A" 1)
     :B1 (pain-ave "B" 1)}
   (= cycles 2)
    {:A1 (pain-ave "A" 1)
     :B1 (pain-ave "B" 1)
     :A2 (pain-ave "A" 2)
     :B2 (pain-ave "B" 2)}
   (= cycles 3)
    {:A1 (pain-ave "A" 1)
     :B1 (pain-ave "B" 1)
     :A2 (pain-ave "A" 2)
     :B2 (pain-ave "B" 2)
     :A3 (pain-ave "A" 3)
     :B3 (pain-ave "B" 3)}
   (= cycles 4)
    {:A1 (pain-ave "A" 1)
     :B1 (pain-ave "B" 1)
     :A2 (pain-ave "A" 2)
     :B2 (pain-ave "B" 2)
     :A3 (pain-ave "A" 3)
     :B3 (pain-ave "B" 3)
     :A4 (pain-ave "A" 4)
     :B4 (pain-ave "B" 4)}))

(defn graph-3 []
  (.log js/console "graph 3")
  (let [pairs (pairs)
        cycles (get @metadata :number_of_cycles)
        aves   (pain-aves cycles)
        w (* 2 cycles)
        w (/ 270 w)
        g (svg-node "g")]
    (.log js/console "w" w)
    (dom/set-attr! g :id "graph-3"
                     :transform "translate(58,586)")
        (doseq [i (range 0 (count pairs))]
          (let [ab (get pairs i)
                t  (take i pairs)
                f  (filter #{ab} t)
                c  (count f)
                k  (keyword (str ab (inc c)))
                rect (svg-node "rect")]
             (when rect
		(-> rect
                 (dom/set-attr! :x (* w i)
                                :y (y-coord (get aves k))
                                :width w
                                :height 3)
                 (dom/set-style! :fill (if (= ab "A") "red" "#000080")))
            (dom/append! g rect))))
    (dom/append! (sel1 :#svg) g)))

(defn bars [id symptom]
  (let [rect-a (sel1 (str "#" id "-a"))
        rect-b (sel1 (str "#" id "-b"))
        a (ave symptom "A")
        a (/ a 10)
        b (ave symptom "B")
        b (/ b 10)]
     (dom/set-attr! rect-a :height (* 270 a))
     (dom/set-attr! rect-b :height (* 270 b))
     (if (< a b) (dom/set-style! rect-a :stroke "linegreen" :stroke-width 2)
                 (dom/set-style! rect-b :stroke "linegreen" :stroke-width 2))))

(defn remove-neuro []
  (.log js/console "remove-neuro")
  (remove! :#np-pain-bars1)
  (remove! :#np-pain-bars2)
  (remove! :#np-pain-bars3)
  (remove! :#np-pain-bars4)
  (remove! :#np-pain-bars5)
  (remove! :#np-pain-outcome)
  (remove! :#np-pain-summary))

(defn graph-4 []
  (bars "pain" :averagePainIntensity)
  (bars "fatigue" :fatiguePrompt)
  (bars "drowsiness" :drowsinessPrompt)
  (bars "sleep" :sleepDisturbancePrompt)
  ;(bars "thinking" :cognitiveFunctionFoggyThinkingPrompt)
  (bars "thinking" (keyword (get @metadata :cognitiveFunctionPromptKey)))
  (bars "const" :constipationPrompt)
  (if (get-in @data [0 :painHotness])
      (bars "np-pain" :painHotness)
      (remove-neuro)))

(defn confidence [cat]
  (let [median (sel1 (str "#" cat "-median"))
        score (get-in @results [:data 0 :data (keyword cat)])
        median-effect (get-in score [:graph_5 :median_effect 0])
        more-effective (get-in score [:graph_5 :more_effective_regimen 0])
        m-trans (* 248 median-effect)
        m-trans (if (= more-effective "A" )(* -1 m-trans) m-trans)
        confidence (sel1 (str "#" cat "-confidence"))
        ub  (get-in score [:graph_5 :upper_bound 0])
        lb  (get-in score [:graph_5 :lower_bound 0])
        lbr (get-in score [:graph_5 :lower_bound_regimen 0])
        ubr (get-in score [:graph_5 :upper_bound_regimen 0])
        x1  (* 257 lb)
        x1  (if (= lbr "A") (* -1 x1) x1)
        x2  (* 257 ub)
        x2  (if (= ubr "A") (* -1 x2) x2)
        d   (str "M " x1 ",0 " x2 ",0")]
    (when median
     (dom/set-attr! median :transform (str "translate(" m-trans ", -1)"))
     (dom/set-attr! confidence :d d))))

(defn graph-5 []
 (confidence "pain")
 (confidence "fatigue")
 (confidence "neuropathic_pain")
 (confidence "thinking_problems")
 (confidence "drowsiness")
 (confidence "constipation")
 (confidence "sleep_problems"))

(defn summary [cat]
  (let [summary-ac (sel1 (str "#" cat "-summary-ac"))
        summary-am (sel1 (str "#" cat "-summary-am"))
        summary-bm (sel1 (str "#" cat "-summary-bm"))
        summary-bc (sel1 (str "#" cat "-summary-bc"))
        scores (get-in @results [:data 0 :data (keyword cat) :graph_6])
        wid 494
        ac (get-in scores [:a_clinically_better 0])
        ac (* (js/parseFloat ac) wid)
        am (get-in scores [:a_marginally_better 0])
        am (* (js/parseFloat am) wid)
        bm (get-in scores [:b_marginally_better 0])
        bm (* (js/parseFloat bm) wid)
        bc (get-in scores [:b_clinically_better 0])
        bc (* (js/parseFloat bc) wid)]
    (when summary-ac
     (dom/set-attr! summary-ac :width ac)
     (dom/set-attr! summary-am :x ac :width am)
     (dom/set-attr! summary-bm :x (+ ac am) :width bm)
     (dom/set-attr! summary-bc :x (+ ac am bm) :width bc))))

(defn graph-6 []
      (summary "pain")
      (summary "fatigue")
      (summary "thinking_problems")
      (summary "drowsiness")
      (summary "constipation")
      (summary "sleep_problems")
      (summary "neuropathic_pain"))

(declare text)

(defn render []
  (dom/set-text! (sel1 :#msg) "Rendering Labels...")
  (labels)
  (setup-grid)
  (dom/set-text! (sel1 :#msg) "Rendering Grids...")
  (ab-pairs)
  (clone-grid)
  (dom/set-text! (sel1 :#msg) "Rendering Graph 1...")
  (graph-1)
  (dom/set-text! (sel1 :#msg) "Rendering Graph 2...")
  (graph-2-a)
  (graph-2-b)
  (dom/set-text! (sel1 :#msg) "Rendering Graph 3...")
  (graph-3)
  (dom/set-text! (sel1 :#msg) "Rendering Graph 4...")
  (graph-4)
  (dom/set-text! (sel1 :#msg) "Rendering Graph 5...")
  (graph-5)
  (dom/set-text! (sel1 :#msg) "Rendering Graph 6...")
  (graph-6)
  (text)
  (remove! :#choose-patient)

  (dom/remove-class! (sel1 :#svg-wrapper) "hidden")
  (dom/remove-class! (sel1 :#outcome-summary-wrapper) "hidden")
  (dom/remove-class! (sel1 :#text-view) "hidden"))


(defn text []
(dom/set-text! (sel1 :#text-view)(str
"ANALGESIA
=================================================================
**Scored 0-10 (lower scores denote better outcomes)**

*Treatment* ---------------------------------------- *A*  *B*

Average pain intensity (P):------------------------- " (rnd-ave :averagePainIntensity "A") "/10 " (rnd-ave :averagePainIntensity "B") "/10"

(if (get-in @data [0 :painHotness])
  (str
"
Neuropathic pain descriptors:

  Sharp: ------------------------------------------- " (rnd-ave :painSharpness "A") "/10 " (rnd-ave :painSharpness "B") "/10

  Hot: --------------------------------------------- " (rnd-ave :painHotness "A") "/10 " (rnd-ave :painHotness "B") "/10

  Sensitive to touch: ------------------------------ " (rnd-ave :painSensitivity "A") "/10 " (rnd-ave :painSensitivity "B") "/10") "")
"
_________________________________________________________________

SIDE EFFECTS
=================================================================
**Scored 0-10 (lower scores denote better outcomes)**

*Treatment* ---------------------------------------- *A*  *B*
Thinking problems: --------------------------------- 3/10 3/10

Constipation: -------------------------------------- " (rnd-ave :constipationPrompt "A") "/10 " (rnd-ave :constipationPrompt "B") "/10

Fatigue: ------------------------------------------- " (rnd-ave :fatiguePrompt "A") "/10 " (rnd-ave :fatiguePrompt "B") "/10

Drowsiness: ---------------------------------------- " (rnd-ave :drowsinessPrompt "A") "/10 " (rnd-ave :drowsinessPrompt "B") "/10

_________________________________________________________________

INTERFERENCE WITH ACTIVITIES OF DAILY LIVING
=================================================================
**Scored 0-10 (lower scores denote better outcomes)**

*Treatment* ---------------------------------------- *A*  *B*

Sleeping: ------------------------------------------ " (rnd-ave :sleepDisturbancePrompt "A") "/10 " (rnd-ave :sleepDisturbancePrompt "B") "/10

Interference with enjoyment of life (E): ----------- " (rnd-ave :enjoymentOfLife "A") "/10 " (rnd-ave :enjoymentOfLife "B") "/10

Interference with general activity (G): ------------ " (rnd-ave :generalActivity "A") "/10 " (rnd-ave :generalActivity "B") "/10
_________________________________________________________________

*SERIAL PEG SCORES – PEG is a 3-item Scale that assesses average
 pain intensity (P), interference with enjoyment of life (E),
 and interference with general activity (G). Lower scores denote
 better outcomes.*")))
