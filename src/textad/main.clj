(ns textad.main
  (:require [clojure.string :as s]
            [clojure.edn :as edn]
            [clojure.main :as m]
            [clojure.set :as st]
            [io.aviso.ansi :as aa]
            [clojure.test :refer [function?]]
            [clojure.core.incubator]
            [clojure.pprint])
  (:refer-clojure :exclude [second println name])
  (:gen-class :main true))


; TODO
; if more than one synonym match in scope - ask user to be more specific

;; =============================================================================
;; The story
;; =============================================================================

(def story (atom {}))
(def history (atom []))

(def ^:dynamic *story-file* nil)
(def ^:dynamic *print-width* 80) ; width should be configurable? not part of story, but maybe user can specify?

; To be bound before invoking interceptors
(def ^:dynamic self nil)
(def ^:dynamic actor nil)
(def ^:dynamic noun nil)
(def ^:dynamic second nil)
(def ^:dynamic location nil)

; Optionally bound before invoking interceptors
(def ^:dynamic keep-silent false)


(defn- text-to-width
  "returns the string with newlines to keep within the print-width without breaking words"
  [s print-width]
  (if (= -1 print-width)
    s
    (let [with-delimiter "((?<=%1$s)|(?=%1$s))" ; look ahead regular expression to keep the delimiters
          delimiters ["\\s" "\\t" "\\n" "\\r" "\\f"]
          re (re-pattern (s/join "|" (map #(format with-delimiter %) delimiters)))
          tokens (s/split s re)
          join-with-width (fn [tokens acc current-width]
            (if (empty? tokens)
              acc
              (let [token (first tokens)
                    exceeds-width (>= (+ current-width (count token)) print-width)
                    next-width (cond
                      exceeds-width (count token)
                      (= "\\n" token) 0
                      :else (+ current-width (count token)))
                    next-acc (s/join "" [acc (if exceeds-width (str "\n" (.trim token)) token)])]
                (recur (rest tokens) next-acc next-width))))]
      (join-with-width tokens "" 0))))

(defn println [& more]
  (if (not keep-silent)
     (clojure.core/println (text-to-width (s/join " " more) *print-width*))))

; http://howardlewisship.com/io.aviso/documentation/pretty/ansi.html
(defn- error [& msg]
  (println (aa/bold-red (s/join " " msg))))

(defn trace [& msg]
  (if (:trace @story)
    (binding [keep-silent false *print-width* -1]
      (println (aa/italic (aa/yellow (s/join " " msg)))))))

(defn replicate-str [times str]
  (s/join "" (replicate times str)))

(defn props [obj-ref]
  (let [template (get-in @story [obj-ref :template])
        template-has (if template (get-in @story [template :has]))
        obj-has (get-in @story [obj-ref :has])
        obj-hasnot (get-in @story [obj-ref :has-not])]
    (st/union obj-has (st/difference template-has obj-hasnot))))

(defn has [obj-ref & properties]
  (st/subset? (set properties) (props obj-ref)))

(defn has-any [obj-ref & properties]
  (not-empty (st/intersection (set properties) (props obj-ref))))

(defn has-none [obj-ref & properties]
  (not (apply has-any obj-ref properties)))

(defn parent-for [obj-ref]
  (get-in @story [obj-ref :where]))

(defn room [obj-ref]
  (loop [obj-ref obj-ref]
    (let [parent-ref (parent-for obj-ref)]
      (if (not parent-ref) obj-ref (recur parent-ref)))))

(defn is-room [obj-ref]
  (nil? (get-in @story [obj-ref :where])))

(defn- objects [] (keys @story))

(defn children
  "collect object where :where = parent-ref"
  [parent-ref]
  (set (filter #(= (get-in @story [% :where]) parent-ref) (objects))))

(defn is-light [obj-ref]
  (or (has obj-ref :light)
    (and (or (has-any obj-ref :transparent :open :supporter) (is-room obj-ref))
      (some #(has % :light) (children obj-ref)))))

(defn is [noun]
  (if (has noun :pluralname) " are " " is "))

(defn has-child [noun child]
  (some #{child} (children noun)))

(defn- evaluate [obj-ref prop]
  (let [val (get-in @story [obj-ref prop])]
    (if (function? val) (val) val)))

(defn- name [obj-ref]
  (evaluate obj-ref :name))

(defn the [obj-ref]
  (let [name (name obj-ref)
        prefix (if (has obj-ref :proper) "" "the ")]
    (str prefix name)))

(defn The [obj-ref]
  (let [the (the obj-ref)]
    (s/capitalize the)))

(defn a [obj-ref]
  (let [article (get-in @story [obj-ref :article])
        name (name obj-ref)
        prefix (if (has obj-ref :proper)
          ""
          (cond
            article (str article " ")
            (has obj-ref :pluralname) "some "
            (re-matches #"[aeiou].*" name) "an "
            :else "a "
          ))]
    (str prefix name)))

(defn list-a [obj-refs]
  (let [as (map a obj-refs)]
    (s/join " and " (filter not-empty (list (s/join ", " (drop-last 1 as)) (last as))))))

(defn end-game [type]
  (swap! story assoc :game-over true)

  (case type
    "victory" (println (aa/bold "\nCongratulations - you have finished the game!\n"))
    "death"   (println (aa/bold "\nGame Over!\n"))
    (println (aa/bold (str "\nGame Over: " type "\n"))))

  (if (not (:no-score @story))
    (println (str "In that game you scored " (:score @story)
      (if (:max-score @story) (str " out of a possible " (:max-score @story)))
       ", in " (:turn @story) " turns.\n")))
  (println "Type 'quit' to quit, 'restore' to restore a previous game, or 'restart' to start again.\n"))


(defn add-score [amount]
  (swap! story assoc :score (+ amount (:score @story)))
  (println "[Your score has just gone up by " amount " point.]\n"))

(defn print-or-evaluate
  "Evaluate func - which may return a string - in which case print, or true (literal, not truthy) in which case continue with any post steps
   else stop with any post steps"
  [func]
  (let [response (if (string? func) func (func))]
    (if (string? response)
      (println response "\n")
      (= true response))))

(defn descr [obj-ref]
  (let [description (binding [self obj-ref] (evaluate obj-ref :description))
        mentionable (filter #(and (has-none % :scenery) (not (= % :player))) (children obj-ref))]
    ; list non-scenery
    (str description "\n"
      (if (seq mentionable)
        (cond
          (is-room obj-ref) (str "\nYou can see " (list-a mentionable) " here.")
          (has obj-ref :container) (if (or (has-none obj-ref :openable) (has obj-ref :openable :open))
            (str "\nInside you see " (list-a mentionable) "."))
          (has obj-ref :supporter) (str "\nOn top you see " (list-a mentionable) "."))))))

(defn message [key & args]
  (let [room-messages (get-in @story [(room :player) :messages])
        message (get room-messages key (get (:messages @story) key))]
    (if message
      (apply format message args)
      (throw (Exception. (str "no message for key '" key "'"))))))

(defn look []
  (if (is-light (room :player))
    (let [parent (parent-for :player)
          inside (not (is-room parent))]
      (println (aa/bold (str (name (room :player)) (if inside (str " (in " (the parent) ")")))) "\n")
      (if (or (not inside) (has-any parent :transparent :open))
        (println (descr (room :player)))
        (println (get-in @story [parent :inside-description])))
      (println))
    (do
      (println (aa/bold (message :darkroom.name)))
      (println (message :darkroom.description))
      (println))))

(declare move-to)
(declare calculate-scope)
(defn- move-floating-objects [where]
  (let [found-in? (fn [x]
    (if-let [found-in (get-in @story [x :found-in])]
      (cond
        (function? found-in)(eval found-in)
        (set? found-in)(some #{where} found-in)
        :else found-in)))]
    (doseq [obj (filter found-in? (objects))]
      (move-to obj where)))

    (calculate-scope))

(defn- get-val [obj-ref key]
  (let [template (get-in @story [obj-ref :template])
        template-val (if template (get-in @story [template key]))
        obj-val (get-in @story [obj-ref val])]
    (if obj-val obj-val template-val)))

(defn- run-room-initial [room]
  (let [initial (get-val room :initial)
        appropriate? (fn [x] (and (not (= x :player)) (has-none x :moved)))]
    (if initial (print-or-evaluate initial))
    (doseq [child (filter appropriate? (children room))]
      (if-let [child-initial (get-val child :initial)]
        (print-or-evaluate child-initial)))))

(defn add-has [obj-ref prop]
  (swap! story assoc-in [obj-ref :has] (set (conj (get-in @story [obj-ref :has]) prop))))

(defn remove-has [obj-ref prop]
  (swap! story assoc-in [obj-ref :has] (set (remove #{prop} (get-in @story [obj-ref :has])))))

(defn dissoc-child [obj-ref]
  (swap! story clojure.core.incubator/dissoc-in [obj-ref :where]))

(defn move-to [who-ref where-ref]
  (trace "moving" who-ref "to" where-ref)
  (let [player-change-room (and (= :player who-ref) (is-room where-ref))]
    (swap! story assoc-in [who-ref :where] where-ref)
    (if player-change-room
      (do
        (binding [location where-ref] ; new binding applies from this point onwards, not just down stack - will this be a problem?
          (trace "room transition to" where-ref)
          (move-floating-objects where-ref)
          (if (or (:verbose @story) (has-none where-ref :visited))
            (look)
            (println (aa/bold (name where-ref)) "\n"))
          (run-room-initial where-ref)
          (if
            (and (has-none where-ref :visited) (has (where-ref @story) :scored))
            (add-score (:room-score @story)))
          (add-has where-ref :visited))))))

(defn turn []
  (:turn @story))


;; =============================================================================
;; Some routines
;; =============================================================================

(defn routine_trace []
  (if (:debug @story)
    (let [b (or (= noun "on") (= noun "true"))]
      (do
        (if (not b) (trace "trace turned off"))
        (swap! story assoc :trace b)
        (if b (trace "trace turned on"))))))

(defn invent [obj-ref]
  "invent - how the game object should be described in inventory"
  (let [invent (:invent @story)]
    (if invent
      (print-or-evaluate invent)
      (a obj-ref))))

(defn print-item [obj-ref depth]
  (let [children (children obj-ref)
        to-string (fn [attr] (cond
          (= attr :openable) (if (has obj-ref :open) "open" "closed")
          (= attr :light) "providing light"
          (= attr :worn) "being worn"
          (and (= attr :lockable) (= attr :locked)) "locked"
          (and (= attr :container) (= attr :open) (empty? children)) "empty"))
        attributes (filter identity (map to-string (get-in @story [obj-ref :has])))]
    (println
      (replicate-str depth " ")
      (invent obj-ref)
      (if (seq attributes)
        (str " (which" (is obj-ref) (s/join " and " attributes) ")")
        ""))
    (if (and (has obj-ref :container) (has obj-ref :open) (seq children))
      (do
        (println " containing")
        (doseq [child children] (print-item child (+ depth 1)))))))


(defn door-to [noun])

(defn get-direction [noun]
  (let [is-match (fn [direction-entry name] (some #{name} (val direction-entry)))
        res (first (filter #(is-match % noun) (seq (:compass @story))))]
    (if res (key res))))

(defn routine_go []
  (trace "go " noun)
  (if noun
    (let [location-ref (get-in @story [location noun])]
      (cond
        (string? location-ref) location-ref
        (function? location-ref) (print-or-evaluate location-ref)
        location-ref (if (has location-ref :door)
          (if (has location-ref :open)
            (let [door-to (evaluate location-ref :door-to)]
              (move-to :player door-to))
            (println (str (The location-ref) " blocks you\n")))
          (move-to :player location-ref))
        :else (message :cant-go)))
    (str "Where do you want to go?")))

(defn routine_inventory []
  (let [children (children :player)]
    (if (empty? children) "You are not carrying anything."
      (do
        (println "You are carrying:")
        (doseq [child children]
          (print-item child 1))
        (println)))))

(defn routine_quit []
  (println "Are you sure you want to quit? (y/n)\n")
  (case (read-line)
    "y" (do
          (println "Bye")
          (System/exit 0))
    "n" (println "")
    (recur)))

(defn routine_help []
  (println (s/join "\n" [
    "quit - to quit"
    "help - to display help"
    "inv - to display inventory"
    "score - to display score"
    "try things like: look, go"
  ])))

(defn routine_nomatch []
  (str "Sorry, I don't understand \"" noun "\""))

(defn save [filename]
  (let [file (str filename ".pos")]
    (spit file {:story (:name @story) :history @history})))

(defn routine_save []
  (let [filename (if noun noun "pos")]
    (save filename)
    (println (str "Your position has been saved as '" filename "'\n"))))

(defn- my-merge [left right] ; my-merge will handle vectors as well as maps
  (cond
    (vector? left) (concat right left)
    (map? left) (merge-with my-merge left right)
    :else right))

(defn- load-cp-script [filename]
  (clojure.lang.Compiler/load (clojure.java.io/reader (clojure.java.io/resource filename)) filename filename))

(defn- load-story []
  (println "loading" *story-file* "\n")
  ; load the base definitions, and merge in the story

  ; (m/load-script "@core.edn") returning nil?
  (reset! story (merge-with my-merge (load-cp-script "core.edn") (m/load-script *story-file*)))
;  (clojure.pprint/pprint @story)

  ; Add state to story which isn't defined in story definition:
  (swap! story assoc :turn 0)
  (swap! story assoc :visited #{})
  (swap! story assoc :score 0)
  (swap! story assoc :game-over false)
  (reset! history []))

(declare run execute-initial)
(defn restore [filename]
  (let [file (str filename ".pos")
        loaded (m/load-script file)]
    (if (= (:story loaded) (:name @story))
      (binding [keep-silent true]
        (load-story)
        (execute-initial)
        (reset! history (:history loaded))
        (run @history))
      (throw (Exception. (str "position was for game '" (:story loaded) "'"))))))

(defn routine_restore []
  (let [filename (if noun noun "pos")]
    (try
      (do
        (restore filename)
        (println (str "Position '" filename "' has been restored\n")))
      (catch Exception e (error "Could not load file" (.getMessage e))))))

(defn routine_restart []
  (let [restart (fn [] (load-story) (execute-initial))]
    (if (:game-over @story)
      (restart)
      (do
        (println "Are you sure you want to restart? (y/n)\n")
        (case (read-line)
          "y" (restart)
           "n" (println "")
          (recur))))))

(defn routine_score []
  (cond
    (:no-score @story) (str "No score available, in " (:turn @story) " turns.")
    (:max-score @story) (str "You have so far scored " (:score @story)
      " out of a possible " (:max-score @story) ", in " (:turn @story) " turns.")
    :else (str "You have so far scored " (:score @story) " in " (:turn @story) " turns.")))


;; =============================================================================
;; Helper functions
;; =============================================================================

(defn print-banner []
  (println "")
  (println (aa/bold (replicate-str 20 "-")))
  (println (aa/bold (:name @story)))
  (println (aa/bold (replicate-str 20 "-")))
  (println ""))

(defn- execute-initial []
  (trace "executing initial")
  (print-or-evaluate (:initial @story)))

(defn- execute-daemons []
  (doseq [obj-ref (objects)]
    (if (get-in @story [obj-ref :daemon-running])
      (binding [self obj-ref
                location (get-in @story [:player :where])]
        (print-or-evaluate (get-in @story [obj-ref :daemon]))))))

(defn start-daemon [obj-ref]
  (swap! story assoc-in [obj-ref :daemon-running] true))

(defn stop-daemon [obj-ref]
  (swap! story assoc-in [obj-ref :daemon-running] false))

(defn start-timer [obj-ref num-turns]
  (swap! story assoc-in [obj-ref :num-turns-to-timeout] num-turns))

(defn stop-timer [obj-ref]
  (swap! story assoc-in [obj-ref :num-turns-to-timeout] -1))

(defn- execute-timers []
  (doseq [obj-ref (objects)]
    (let [timeout (fn [] (binding [self obj-ref
                                   location (get-in @story [:player :where])]
                      (print-or-evaluate (get-in @story [obj-ref :timeout]))))
          update (fn [num-turns] (cond
                   (= 1 num-turns) (do (timeout) -1)
                   (> num-turns 1) (dec num-turns)))]
      (if (get-in @story [obj-ref :num-turns-to-timeout])
        (swap! story update-in [obj-ref :num-turns-to-timeout] update)))))

(defn- check-new-possessions []
  (doseq [new-possession (filter #(has-none % :moved) (children :player))]
    (if (and (not (:no-score @story)) (has new-possession :scored))
       (add-score (:object-score @story)))
    (add-has new-possession :moved)))

(defn- execute-each-turn []
  (trace "executing each turn")
  (doseq [obj-ref (:scope @story)]
    (if-let [each-turn (get-in @story [obj-ref :each-turn])]
      (binding [self obj-ref
                location (get-in @story [:player :where])]
        (print-or-evaluate each-turn)))))

(defn- execute-rules []
  (trace "executing rules")
  (let [rules (:rules @story)]
    (if rules (doseq [rule rules] (print-or-evaluate rule)))))

(defn- extract [result index]
  (if (number? index) (get result index) nil))

(defn- get-by-name-in [name in]
  (defn is-match [synonyms name]
    (some #{name} synonyms))
  (first (filter #(is-match (map s/lower-case (:synonyms (% @story))) (s/lower-case name)) in)))

;(defn- get-by-name-in [name in]
;  (let [is-match (fn [synonyms name] (some #{name} synonyms))
;        synonyms (fn [obj-ref] (map s/lower-case (:synonyms @story)))]
;    (first (filter #(is-match (synonyms %)) (s/lower-case name)) in)))

(defn- get-by-name [name]
  (get-by-name-in name (objects)))

(defn- get-by-name-in-scope [name]
  (get-by-name-in name (:scope @story)))

(defn- interceptors [obj-ref]
  (let [template (get-in @story [obj-ref :template])
        template-interceptors (if template (get-in @story [template :interceptors]))
        obj-interceptors (get-in @story [obj-ref :interceptors])]
    (merge-with merge template-interceptors obj-interceptors)))

(defn- invoke-obj-interceptors [interceptor-type routine obj-ref]
  (let [interceptors (get-in (interceptors obj-ref) [interceptor-type])
        is-match (fn [routine interceptor]
          (trace "invoke-obj-interceptors:is-match" routine interceptor)
          (if (some #{routine} (key interceptor)) (val interceptor)))
        interceptor (some #(is-match routine %) interceptors)]
    (trace "invoke-obj-interceptors" interceptor-type routine obj-ref " -> " interceptor)
      (if interceptor
        (binding [self obj-ref]
          (print-or-evaluate interceptor))
        ; if no interceptor, check for :defaut
        (if (not (= :default routine))
          (invoke-obj-interceptors interceptor-type :default obj-ref)
          true))))

(defn run-life [animate, routine]
  (if (has animate :animate)
    (invoke-obj-interceptors :life routine animate)
    (throw (Exception. (str "cannot call runLife on " animate ". It is not animate")))))

(defn run-before [target, routine]
  (invoke-obj-interceptors :before routine target))

(defn run-after [target, routine]
  (invoke-obj-interceptors :after routine target))

(defn- execute-interceptors [interceptor-type routine]
  (trace "execute-interceptors" interceptor-type routine)
  (if (invoke-obj-interceptors interceptor-type routine (room :player))
    (if (invoke-obj-interceptors interceptor-type routine noun)
        (if (invoke-obj-interceptors interceptor-type routine :library-messages)
          true))))

(defn- default-execute [routine-ref]
  (let [routine (if (keyword? routine-ref) (get-in @story [:routines routine-ref]) routine-ref)]
    (trace "executing" routine-ref)
    (if routine
      (let [response (routine)]
        (if (string? response)
          (println response "\n")
          response))
       (throw (Exception. (str "No routine defined for " routine-ref))))))

(defn- get-children-for-scope [obj-ref]
    (trace "get-children-for-scope" obj-ref)
;    (trace "children:" (children obj-ref) "\n\n")
    (let [f (fn [child] (cons child (if (has-any child :transparent :open :supporter) (get-children-for-scope child))))
          children-list (seq (children obj-ref))]
      (apply concat (map f children-list))))

(defn- to-noun [term]
  (let [noun (get-by-name-in-scope term)]
    (if noun noun {:error :cant-see :term term})))

(defn- to-held [term]
  (if (= :player actor)
    (let [noun (get-by-name-in-scope term)
          possessions (get-children-for-scope :player)]
      (if (some #{noun} possessions)
        noun
        {:error :not-held :term term}))
    ;we won't be so strict for other players
    (get-by-name term)))

(defn- to-creature [term]
  (let [noun (get-by-name-in-scope term)]
    (if noun
      (do
      (if (has noun :animate)
        noun
        {:error :not-animate :term term}))
       {:error :cant-see :term term})))

(defn- to-topic [term] term)

(defn- to-direction [term]
  (let [direction (get-direction term)]
    (if direction direction {:error :not-direction :term term})))

(defn- to-noun-or-topic [term]
 (let [noun (get-by-name-in-scope term)]
   (if noun noun term)))

(defn- to-number [term]
 (if (number? term) term {:error :bad-number :term val}))

(defn verb-match [instruction verb]
  (trace "is match " instruction verb "?")
  (let [grammar (key verb)
        keys (keys (val verb))
        verbString (str "(" (s/join "|" grammar) ")")
        exprs [
          "%{noun,noun}"   "%{noun,creature}"   "%{noun,topic}"   "%{noun,held}"   "%{noun,direction}"   "%{noun,nounOrTopic}"   "%{noun,number}"
          "%{second,noun}" "%{second,creature}" "%{second,topic}" "%{second,held}" "%{second,direction}" "%{second,nounOrTopic}" "%{second,number}"]
        replace-n (fn [string expr & more-expr]
                    (let [r (s/replace string expr "(\\S*)")]
                      (if (first more-expr) (recur r (first more-expr) (rest more-expr)) r)))
        match (fn [key]
                (let [string (if (empty? key) verbString (str verbString " " key))
                      routine (get (val verb) key)
                      regex (re-pattern (apply replace-n string exprs))
                      result (re-matches regex instruction)
                      extract (fn [placeholder key]
                        (let [i (.indexOf key placeholder)]
                          (if (>= i 0)
                            (let [prefix (.substring key 0 i)
                                  groupPosition (+ 2 (count (filter #{\(} prefix)) (count (filter #{\%} prefix)))
                                  match (re-matches (re-pattern "%\\{(\\w*),(\\w*)}") placeholder)]
                                [groupPosition (nth match 1) (nth match 2)]))))
                      transform (fn [[groupPosition groupResult groupType]]
                        (let [val (nth result groupPosition)
                          transform (case groupType
                            ("noun") to-noun
                            ("creature") to-creature
                            ("topic") to-topic
                            ("held") to-held
                            ("direction") to-direction
                            ("nounOrTopic") to-noun-or-topic
                            ("number") to-number)]
                      {(keyword groupResult) (transform val)}))]
;                  (println "result" result)
                  (trace "verbString " verbString)
                  (if result
                    (let [xs (filter identity (map #(extract % key) exprs))
                          transformed (apply merge {:routine routine} (map transform xs))]
;                      (println "transformed" transformed)
                      transformed))))]
    (some #(match %) keys)))

(defn- error? [match]
  (let [noun-error (get-in match [:noun :error])
        second-error (get-in match [:second :error])]
    (if noun-error
      noun-error
      (if second-error
        second-error
        nil))))

(defn execute [routine]
  (if (invoke-obj-interceptors :orders routine actor)
    (if (invoke-obj-interceptors :before :order actor)
      (if (execute-interceptors :before routine)
        (if (default-execute routine)
          (if (execute-interceptors :after routine)
          false))))))

; TODO tidy this up - e.g. checking error for actor independently for noun/second
(defn- execute-instruction [instruction]
  (trace "in execute-instruction" instruction)
  (let [order-match (re-find #"([^,]*),(.*)" instruction)
        actor-match (nth order-match 1)
        actor (if actor-match (to-noun actor-match))
        command-match (nth order-match 2)
        command (if command-match (.trim command-match) instruction)
        actor-error (:error actor)]
    (if actor-error
      (println (message actor-error) "\n")
      (binding [actor (if actor actor :player) ; binding required for parsing (verb-match .e.g if held) as well as default-execute
                location (get-in @story [:player :where])]
        (let [routine-match (some #(verb-match command %) (:verbs @story))
              direction (get-direction command)
              match (cond
                  routine-match routine-match
                  direction {:routine :go :noun direction}
                  :else {:routine routine_nomatch :noun command})
              error (error? match)]
          (trace "match" match)
          (trace "error" error)
          (if (and (get-in @story [:game-over])
                    ; these are the only permitted routines when game is over.
                    ; Note they are currently routines and not keywords since group 1 routines are not interceptable
                   (not (st/subset? #{(:routine match)} #{routine_score routine_help routine_quit routine_restore routine_restart routine_trace})))
            (println "Game is over - you can only quit, restart, restore or score.\n")
            (if error
              (println (message error) "\n")
              (binding [noun (:noun match) second (:second match)]
                (let [routine (:routine match)]
                  (execute routine))))))))))

(defn- calculate-scope []
  "Populates the scope with all 'visible' gameObjects - i.e. gameObjects which can be interacted with.
  This is defined by all objects in the player's room/immediate container, their possessions, and any children objects of transparent/open/supporter objects."
  (trace "in calculateScope")
  (let [scope
         (concat
           (list (room :player)) ; room
           (list (parent-for :player)) ; immediate parent
           (list :player) ; player
           (get-children-for-scope :player) ; player's possessions
           (if (is-light (room :player)) (get-children-for-scope (room :player))) ; in light, we can see all room objects
         )]
    (trace "calculated scope:" (seq scope))
    (swap! story assoc :scope scope)))

(defn place-in-scope [obj-ref]
  (swap! story assoc-in [:scope] (conj (:scope @story) obj-ref)))

(defn- command-line-instructions []
  "returns a lazy sequence of commands which come from the command-line
   (and are stored on history stack)"
  (let [read-instruction (fn []
    (print "> ")
    (flush)
    (let [res (.trim (read-line))]
      (println)
      (swap! history conj res)
      res))]
  (cons (read-instruction) (lazy-seq (command-line-instructions)))))

(defn- run [instructions]
  (trace "in run")
  ; pre-execution actions
  (when (not (:game-over @story))
    (calculate-scope) ; calcuate scope before execute rules, since they may add to the scope
    (execute-rules))
  ; can't use [[instruction & rest] (instructions) since it evaluates the first element of rest too)
  (let [instruction (first instructions)
        rest (rest instructions)]
    (execute-instruction instruction)
    ; post-execution actions
    (when (not (:game-over @story))
      (execute-daemons)
      (execute-timers)
      (check-new-possessions)
      (execute-each-turn)
      (swap! story assoc :turn (inc (:turn @story))))
  (if (not-empty rest) (recur rest)))) ; could be empty when running history

;; =============================================================================
;; Main
;; =============================================================================

(defn -main [& args]
  (if args
    (let [file (first args)]
      (binding [*story-file* file]
        (load-story)
        (print-banner)
        (execute-initial)
        (run (command-line-instructions))))
    (println "usage: story-file")))
