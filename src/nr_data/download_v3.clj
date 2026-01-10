(ns nr-data.download-v3
  (:require
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [clojure.set]
   [clojure.string :as str]
   [nr-data.text :refer [add-stripped-card-text]]
   [nr-data.utils :refer [cards->map slugify]]
   [org.httpkit.client :as http]
   [zprint.core :as zp]))

(defn strip-typesetting-chars
  ;; strip out any typesetting characters from card titles
  [target-str]
  (when target-str
    (str/replace target-str #"[ʼ’“”]" {"’" "'"
                                       "ʼ" "'"
                                       "“" "\""
                                       "”" "\""})))

(defn underscore->hyphen
  "Convert underscores to hyphens in v3 API IDs"
  [s]
  (when s (str/replace s "_" "-")))

(defn parse-response
  [body]
  (json/parse-string body true))

(defn parse-v3-response
  "Parse v3 JSON:API response, extracting data and merging id into attributes"
  [body]
  (let [parsed (json/parse-string body true)]
    (->> (:data parsed)
         (map (fn [item]
                (assoc (:attributes item) :id (:id item)))))))

(def large-endpoints
  "Endpoints that need pagination (>1000 items)"
  #{"cards" "printings"})

(defn download-nrdb-data
  "Download data from NRDB v3 API. Adds page[size] for large endpoints."
  [path]
  (let [needs-pagination (some #(str/starts-with? path %) large-endpoints)
        url (str "https://api-preview.netrunnerdb.com/api/v3/public/" path
                 (when needs-pagination
                   (str (if (str/includes? path "?") "&" "?")
                        "page[size]=2500")))
        data (http/get url)
        {:keys [status body error]} @data]
    (cond
      error (throw (Exception. (str "Failed to download file " error)))
      (= 200 status) (parse-v3-response body)
      :else (throw (Exception. (str "Failed to download file " url ", status " status))))))

(defn read-json-file
  [file-path]
  ((comp parse-response slurp) file-path))

(defn read-local-data
  [base-path filename]
  (read-json-file (str base-path "/" filename ".json")))

(defn read-card-dir
  [base-path]
  (->> (str base-path "/pack")
       (io/file)
       (file-seq)
       (filter #(and (.isFile %)
                     (str/ends-with? % ".json")))
       (map read-json-file)
       (flatten)
       (parse-response)))

(defn translate-fields
  "Modify NRDB json data to our schema"
  [fields data]
  (reduce-kv (fn [m k v]
               (if (contains? fields k)
                 (let [[new-k new-v] ((get fields k) [k v])]
                   (assoc m new-k new-v))
                 m))
             {} data))

(defmacro rename
  "Rename a card field"
  ([new-name]
   `(fn [[k# v#]] [~new-name v#]))
  ([new-name f]
   `(fn [[k# v#]] [~new-name (~f v#)])))

(defn convert-cycle
  [v]
  (case v
    ;; name conversions (for cycle names)
    "Core Set" "Core"
    "Revised Core Set" "Revised Core"
    ;; id conversions (for v3 card_cycle_id after underscore->hyphen)
    "core-set" "core"
    "revised-core-set" "revised-core"
    ;; legacy v2 code conversions
    "core2" "revised-core"
    "napd" "napd-multiplayer"
    "sc19" "system-core-2019"
    v))

(def cycle-fields
  {:name (rename :name convert-cycle)
   :position identity
   :card_set_ids identity})

(defn add-cycle-fields
  [active-standard-cycle-ids cy]
  (let [cycle-id (slugify (:name cy))
        size (count (:card_set_ids cy))
        ;; draft was never in standard, so it's not "rotated out"
        ;; cycles in active standard snapshot are not rotated
        rotated (if (= "draft" cycle-id)
                  false
                  (not (contains? active-standard-cycle-ids cycle-id)))]
    (-> cy
        (assoc :id cycle-id
               :size size
               :rotated rotated)
        (dissoc :card_set_ids))))

(defn convert-cycle-id
  "Convert v3 card_cycle_id (with underscores) to our cycle-id format"
  [v]
  (-> v
      underscore->hyphen
      convert-cycle))

(def set-fields
  {:legacy_code (rename :code)
   :card_cycle_id (rename :cycle-id convert-cycle-id)
   :date_release (rename :date-release)
   :name identity
   :position identity
   :size identity})

(defn deluxe-set?
  [s]
  (case (:cycle-id s)
    ("core" "revised-core" "system-core-2019"
     "creation-and-control" "honor-and-profit" "order-and-chaos" "data-and-destiny"
     "terminal-directive" "reign-and-reverie") true
    ;; else
    false))

(defn set-type?
  [s]
  (case (slugify (:name s))
    ("core-set" "revised-core-set" "system-core-2019"
     "system-gateway" "system-update-2021") :core
    ("creation-and-control" "honor-and-profit"
     "order-and-chaos" "data-and-destiny"
     "reign-and-reverie") :deluxe
    ("magnum-opus" "magnum-opus-reprint" "uprising-booster-pack") :expansion
    "draft" :draft
    "napd-multiplayer" :promo
    ("terminal-directive" "terminal-directive-campaign") :campaign
    ;; else
    :data-pack))

(defn add-set-fields
  [s]
  (-> s
      (assoc :id (slugify (:name s))
             :deluxe (deluxe-set? s)
             :set-type (set-type? s))))

(defn convert-subtypes
  [subtype]
  (when (seq subtype)
    (->> (str/split subtype #" - ")
         (map slugify)
         (map keyword)
         (into []))))

(defn v3-id->keyword
  "Convert v3 API id (with underscores) to keyword (with hyphens)"
  [id]
  (when id
    (-> id underscore->hyphen keyword)))

(defn convert-card-type
  "Convert v3 card type to v2 format (corp-identity/runner-identity -> identity)"
  [type-id]
  (case type-id
    ("corp_identity" "runner_identity") :identity
    (v3-id->keyword type-id)))

(defn parse-int
  "Parse string to int, return nil if not a valid integer string"
  [s]
  (when (and s (string? s))
    (try (Integer/parseInt s)
         (catch NumberFormatException _ nil))))

(defn process-face
  "Process a face map with consistent field naming"
  [face]
  (let [processed (-> face
                      (dissoc :card_subtype_ids :images)
                      (clojure.set/rename-keys {:display_subtypes :subtype
                                                :stripped_text :stripped-text
                                                :stripped_title :stripped-title
                                                :base_link :base-link})
                      (update :subtype convert-subtypes))]
    (if (:base-link processed)
      (update processed :base-link parse-int)
      processed)))

(defn process-faces
  "Process all faces in a card"
  [faces]
  (when (seq faces)
    (mapv process-face faces)))

(def card-fields
  {
   :advancement_requirement (rename :advancement-requirement)
   :agenda_points (rename :agenda-points)
   :base_link (rename :base-link)
   :cost identity
   :deck_limit (rename :deck-limit)
   :faces (rename :faces process-faces)
   :faction_id (rename :faction v3-id->keyword)
   :influence_cost (rename :influence-cost)
   :influence_limit (rename :influence-limit)
   :display_subtypes (rename :subtype convert-subtypes)
   :memory_cost (rename :memory-cost)
   :minimum_deck_size (rename :minimum-deck-size)
   :num_extra_faces (rename :num-extra-faces)
   :side_id (rename :side v3-id->keyword)
   :strength identity
   :text identity
   :title identity
   :trash_cost (rename :trash-cost)
   :card_type_id (rename :type convert-card-type)
   :is_unique (rename :uniqueness)
   })

(def numeric-card-fields
  "Card fields that should be integers"
  #{:advancement-requirement :agenda-points :base-link :cost :deck-limit
    :influence-cost :influence-limit :memory-cost :minimum-deck-size
    :strength :trash-cost})

(defn add-card-fields
  [card]
  (let [;; Convert string numbers to integers for numeric fields
        card (reduce (fn [c k]
                       (if-let [v (get c k)]
                         (if (string? v)
                           (assoc c k (parse-int v))
                           c)
                         c))
                     card
                     numeric-card-fields)
        ;; Remove nil values
        card (into {} (remove (fn [[_ v]] (nil? v)) card))
        ;; Add back nil for fields that should be nil based on card type
        ;; :cost nil for any card without a cost (X-cost cards)
        card (if (and (not (contains? card :cost))
                      (contains? #{:operation :event :hardware :resource :program} (:type card)))
               (assoc card :cost nil)
               card)
        ;; :strength -1 in v3 means variable strength, convert to nil
        card (if (= -1 (:strength card))
               (assoc card :strength nil)
               card)
        ;; :advancement-requirement nil for agendas without fixed advancement
        card (if (and (= :agenda (:type card))
                      (not (contains? card :advancement-requirement)))
               (assoc card :advancement-requirement nil)
               card)
        ;; :influence-limit nil for identities without fixed influence limit
        card (if (and (= :identity (:type card))
                      (not (contains? card :influence-limit)))
               (assoc card :influence-limit nil)
               card)
        ;; Remove :num-extra-faces and :faces if num-extra-faces is 0
        card (if (or (nil? (:num-extra-faces card))
                     (zero? (:num-extra-faces card)))
               (dissoc card :num-extra-faces :faces)
               card)]
    (-> card
        (assoc :id (slugify (:title card)))
        (dissoc (when (or (and (= :agenda (:type card))
                               (not (or (= :neutral-corp (:faction card))
                                        (= :neutral-runner (:faction card)))))
                          (= :identity (:type card)))
                  :influence-cost)))))

(defn convert-set-id
  "Convert v3 card_set_id (with underscores) to our set-id format"
  [v]
  (-> v
      underscore->hyphen
      convert-cycle))

(defn process-printing-face
  "Process a printing face map, keeping only relevant fields"
  [face]
  (-> face
      (select-keys [:index :copy_quantity :flavor])
      (clojure.set/rename-keys {:copy_quantity :copy-quantity})))

(defn process-printing-faces
  "Process all faces in a printing"
  [faces]
  (when (seq faces)
    (mapv process-printing-face faces)))

(def set-card-fields
  {
   :id (rename :code)
   :attribution identity
   :card_id (rename :card-id underscore->hyphen)
   :card_set_id (rename :set-id convert-set-id)
   :display_illustrators (rename :illustrator)
   :faces (rename :faces process-printing-faces)
   :flavor identity
   :num_extra_faces (rename :num-extra-faces)
   :position identity
   :quantity identity
   })

(def mwl-fields
  {:date_start (rename :date-start)
   :format_id (rename :format underscore->hyphen)
   :name identity
   :point_limit (rename :point-limit)
   :verdicts identity})

(defn convert-verdicts
  "Convert v3 verdicts structure to v2 cards map"
  [verdicts]
  (let [banned (for [card-id (:banned verdicts)]
                 [(underscore->hyphen card-id) {:deck-limit 0}])
        restricted (for [card-id (:restricted verdicts)]
                     [(underscore->hyphen card-id) {:is-restricted 1}])
        universal-fc (for [[card-id cost] (:universal_faction_cost verdicts)]
                       [(underscore->hyphen (name card-id)) {:universal-faction-cost cost}])
        global-penalty (for [card-id (:global_penalty verdicts)]
                         [(underscore->hyphen card-id) {:global-penalty 1}])
        points (for [[card-id pts] (:points verdicts)]
                 [(underscore->hyphen (name card-id)) {:points pts}])]
    (into {} (concat banned restricted universal-fc global-penalty points))))

(defn convert-mwl
  [mwl]
  (-> mwl
      (assoc :cards (convert-verdicts (:verdicts mwl))
             :id (slugify (:name mwl)))
      (dissoc :verdicts)
      (->> (remove (fn [[_ v]] (nil? v)))
           (into {}))))

(defn sort-and-group-set-cards
  [set-cards]
  (->> set-cards
       (sort-by :position)
       (group-by :set-id)))

(defn fetch-data
  "Read NRDB json data. Modify function is mapped to all elements in the data collection."
  ([download-fn m] (fetch-data download-fn m identity))
  ([download-fn {:keys [path fields]} add-fields-function]
   (->> (download-fn path)
        (map (partial translate-fields fields))
        (map add-fields-function))))

(def tables
  {:cycle {:path "card_cycles" :fields cycle-fields}
   :set {:path "card_sets" :fields set-fields}
   :card {:path "cards" :fields card-fields}
   :set-card {:path "printings" :fields set-card-fields}
   :mwl {:path "restrictions" :fields mwl-fields}
   })

(defn snapshot-handler
  "Download snapshots and return the active standard cycle IDs"
  [line-ending download-fn]
  (print "Downloading and processing snapshots... ")
  (let [snapshots (download-fn "snapshots")
        ;; Find the active standard format snapshot
        active-standard (->> snapshots
                             (filter #(and (:active %)
                                           (= "standard" (:format_id %))))
                             first)
        ;; Get the cycle IDs and convert underscores to hyphens
        active-cycle-ids (->> (:card_cycle_ids active-standard)
                              (map underscore->hyphen)
                              set)
        path "edn/snapshots.edn"]
    (io/make-parents path)
    (println "Saving" path)
    (spit path (str (zp/zprint-str (into [] snapshots)) line-ending))
    active-cycle-ids))

(defn cycle-handler
  [line-ending download-fn active-cycle-ids]
  (print "Downloading and processing cycles... ")
  (let [cycles (->> (fetch-data download-fn (:cycle tables) (partial add-cycle-fields active-cycle-ids))
                    (sort-by :position)
                    (into []))
        path (str "edn/cycles.edn")]
    (io/make-parents path)
    (println "Saving" path)
    (spit path (str (zp/zprint-str cycles) line-ending))
    cycles))

(defn set-handler
  [line-ending download-fn]
  (print "Downloading and processing sets... ")
  (let [sets (->> (fetch-data download-fn (:set tables) add-set-fields)
                  (sort-by :date-release)
                  (into []))
        path (str "edn/sets.edn")]
    (io/make-parents path)
    (println "Saving" path)
    (spit path (str (zp/zprint-str sets) line-ending))
    sets))

(defn card-handler
  [line-ending download-fn]
  (let [raw-cards (download-fn (-> tables :card :path))
        raw-cards (mapv #(update % :title strip-typesetting-chars) raw-cards)
        card-stub (fn [_] raw-cards)
        cards (->> (fetch-data card-stub (:card tables) add-card-fields)
                   (add-stripped-card-text)
                   (cards->map :id))]
    (println "Saving edn/cards")
    (doseq [[path card] cards
            :let [path (str "edn/cards/" path ".edn")]]
      (io/make-parents path)
      (spit path (str (zp/zprint-str card) line-ending)))
    cards))

(defn remove-nil-values
  "Remove keys with nil values from a map"
  [m]
  (into {} (remove (fn [[_ v]] (nil? v)) m)))

(defn process-printing
  "Post-process a printing, removing nil values and handling faces"
  [printing]
  (let [printing (remove-nil-values printing)]
    (if (or (nil? (:num-extra-faces printing))
            (zero? (:num-extra-faces printing)))
      (dissoc printing :num-extra-faces :faces)
      printing)))

(defn printings-handler
  [line-ending download-fn]
  (print "Downloading and processing printings... ")
  (let [raw-set-cards (->> (fetch-data download-fn (:set-card tables))
                           (map process-printing))]
    (println "done")
    raw-set-cards))

(defn set-cards-handler
  [line-ending raw-set-cards]
  (let [set-cards (sort-and-group-set-cards raw-set-cards)]
    (println "Saving edn/set-cards")
    (doseq [[path set-card] set-cards
            :let [path (str "edn/set-cards/" path ".edn")]]
      (io/make-parents path)
      (spit path (str (zp/zprint-str set-card) line-ending)))
    set-cards))

(defn mwl-handler
  [line-ending download-fn]
  (print "Downloading and processing mwls... ")
  (let [mwls (fetch-data download-fn (:mwl tables) convert-mwl)
        path "edn/mwls.edn"]
    (io/make-parents path)
    (println "Saving" path)
    (spit path (str (zp/zprint-str (into [] mwls)) line-ending))
    mwls))

(defn download-from-nrdb
  [& args]
  (let [line-ending "\n"
        use-local (some #{"--local"} args)
        localpath (first (remove #(and % (str/starts-with? % "--")) args))
        download-fn (if use-local
                      (partial read-local-data localpath)
                      download-nrdb-data)

        active-cycle-ids (snapshot-handler line-ending download-fn)

        _cycles (cycle-handler line-ending download-fn active-cycle-ids)

        _sets (set-handler line-ending download-fn)

        _ (print "Downloading and processing cards... ")
        ;; In v3, cards and printings are separate endpoints
        ;; Cards endpoint has card data, printings has set-card info
        card-download-fn (if use-local
                           (partial read-card-dir localpath)
                           download-nrdb-data)

        _cards (card-handler line-ending card-download-fn)

        raw-set-cards (printings-handler line-ending download-fn)

        _set-cards (set-cards-handler line-ending raw-set-cards)

        _mwls (mwl-handler line-ending download-fn)]

    (println "Done!")))

(comment
  (download-from-nrdb)
  ,)
