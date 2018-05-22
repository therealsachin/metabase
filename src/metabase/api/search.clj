(ns metabase.api.search
  (:require [compojure.core :refer [DELETE GET POST PUT]]
            [honeysql.helpers :as h]
            [metabase.util.honeysql-extensions :as hx]
            [metabase.api.common :refer [defendpoint define-routes]]
            [metabase.models
             [dashboard :refer [Dashboard]]
             [card :refer [Card]]
             [collection :refer [Collection]]
             [pulse :refer [Pulse]]]
            [metabase.util.schema :as su]
            [schema.core :as s]
            [toucan.db :as db]))

(defn- create-search-query [entity entity-name search-string archived?]
  {:select [:name :description :id [(hx/literal entity-name) :type]]
   :from   [entity]
   :where  [:and
            [:like :name search-string]
            [:= :archived archived?]]})

(defn- collections-query [search-string archived?]
  (create-search-query Collection "collection" search-string archived?))

(defn- card-query [search-string archived?]
  (create-search-query Card "card" search-string archived?))

(defn- dashboard-query [search-string archived?]
  (create-search-query Dashboard "dashboard" search-string archived?))

(defn- pulse-query [search-string archived?]
  (when-not archived?
    {:select [:name [nil :description] :id [(hx/literal "pulse") :type]]
     :from   [Pulse]
     :where  [:like :name search-string]}))

(defn- search-all [search-string archived?]
  (db/query {:union-all (remove nil? (map (fn [search-query-fn]
                                            (search-query-fn search-string archived?))
                                          [collections-query card-query dashboard-query pulse-query]))}))

(defendpoint GET "/"
  "Search Cards, Dashboards, Collections and Pulses for the substring `q`."
  [q archived types]
  {q          su/NonBlankString
   archived   (s/maybe su/BooleanString)}
  (search-all (str "%" q "%") (Boolean/parseBoolean archived)))

(define-routes)
