(ns metabase.api.search
  (:require [clojure.string :as str]
            [compojure.core :refer [GET]]
            [honeysql.helpers :as h]
            [metabase.api.common :refer [defendpoint define-routes]]
            [metabase.models
             [card :refer [Card]]
             [collection :refer [Collection]]
             [dashboard :refer [Dashboard]]
             [metric :refer [Metric]]
             [pulse :refer [Pulse]]
             [segment :refer [Segment]]]
            [metabase.util
             [honeysql-extensions :as hx]
             [schema :as su]]
            [schema.core :as s]
            [toucan.db :as db]))

(def ^:private search-columns-without-type
  "The columns found in search query clauses except type. Type is added automatically"
  [:name :description :id])

(defn- merge-search-select
  "The search query uses a `union-all` which requires that there be the same number of columns in each of the segments
  of the query. This function will take `entity-columns` and will inject constant `nil` values for any column missing
  from `entity-columns` but found in `search-columns`"
  ([entity-type entity-columns]
   (merge-search-select {} entity-type entity-columns))
  ([query-map entity-type entity-columns]
   (let [entity-column-set (set entity-columns)
         cols-or-nils      (for [search-col search-columns-without-type]
                             (if (contains? entity-column-set search-col)
                               search-col
                               [nil search-col]))]
     (apply h/merge-select query-map (concat cols-or-nils [[(hx/literal entity-type) :type]])))))

(defn- merge-name-search
  "Add case-insensitive name query criteria to `query-map`"
  [query-map search-string]
  (h/merge-where query-map [:like :%lower.name search-string]))

(defn- merge-name-and-archived-search
  "Add name and archived query criteria to `query-map`"
  [query-map search-string archived?]
  (-> query-map
      (merge-name-search search-string)
      (h/merge-where [:= :archived archived?])))

(defn- merge-name-and-active-search
  [query-map search-string archived?]
  (-> query-map
      (merge-name-search search-string)
      ;; archived? = true is the same as is_active = false, so flip it here
      (h/merge-where [:= :is_active (not archived?)])))

(defn- maybe-just-collection
  "If `collection-id` is non-nil, include it in the query criteria"
  [query-map collection-id]
  (if collection-id
    (h/merge-where query-map [:= :collection_id collection-id])
    query-map))

(defmulti ^:private create-search-query (fn [entity search-string archived? collection] entity))

(defmethod ^:private create-search-query :card
  [_ search-string archived? collection]
  (-> (merge-search-select "card" [:name :description :id])
      (h/merge-from Card)
      (merge-name-and-archived-search search-string archived?)
      (maybe-just-collection collection)))

(defmethod ^:private create-search-query :collection
  [_ search-string archived? collection]
  ;; If we have a collection, no need to search collections
  (when-not collection
    (-> (merge-search-select "collection" [:name :description :id])
        (h/merge-from Collection)
        (merge-name-and-archived-search search-string archived?))))

(defmethod ^:private create-search-query :dashboard
  [_ search-string archived? collection]
  (-> (merge-search-select "dashboard" [:name :description :id])
      (h/merge-from Dashboard)
      (merge-name-and-archived-search search-string archived?)
      (maybe-just-collection collection)))

(defmethod ^:private create-search-query :pulse
  [_ search-string archived? collection]
  ;; Pulses don't currently support being archived, omit if archived is true
  (when-not archived?
    (-> (merge-search-select "pulse" [:name :id])
        (h/merge-from Pulse)
        (merge-name-search search-string)
        (maybe-just-collection collection))))

(defmethod ^:private create-search-query :metric
  [_ search-string archived? collection]
  (when-not collection
    (-> (merge-search-select "metric" [:name :description :id])
        (h/merge-from Metric)
        (merge-name-and-active-search search-string archived?))))

(defmethod ^:private create-search-query :segment
  [_ search-string archived? collection]
  (when-not collection
    (-> (merge-search-select "segment" [:name :description :id])
        (h/merge-from Segment)
        (merge-name-and-active-search search-string archived?))))

(defn- search [search-string archived? collection]
  (db/query {:union-all (for [entity [:card :collection :dashboard :pulse :segment :metric]
                              :let [query-map (create-search-query entity search-string archived? collection)]
                              :when query-map]
                          query-map)}))

(defendpoint GET "/"
  "Search Cards, Dashboards, Collections and Pulses for the substring `q`."
  [q archived collection_id]
  {q             su/NonBlankString
   archived      (s/maybe su/BooleanString)
   collection_id (s/maybe s/Any)}
  (let [search-string (str "%" (str/lower-case q) "%")
        archived?     (Boolean/parseBoolean archived)]
    (search search-string archived? collection_id)))

(define-routes)
