(ns metabase.api.search-test
  (:require [expectations :refer :all]
            [metabase.api.search :refer :all]
            [metabase.models
             [card :refer [Card]]
             [collection :refer [Collection]]
             [dashboard :refer [Dashboard]]
             [pulse :refer [Pulse]]]
            [metabase.test.data.users :refer :all]
            [metabase.test.util :as tu]
            [toucan.util.test :as tt]))

(def ^:private default-search-results
  #{{:name "dashboard foo dashboard", :description nil, :id true, :type "dashboard"}
    {:name "collection foo collection", :description nil, :id true, :type "collection"}
    {:name "card foo card", :description nil, :id true, :type "card"}
    {:name "pulse foo pulse", :description nil, :id true, :type "pulse"}})

;; Basic search, should find 1 of each entity type
(expect
  default-search-results
  (tt/with-temp* [Card       [_ {:name "card foo card"}]
                  Dashboard  [_ {:name "dashboard foo dashboard"}]
                  Collection [_ {:name "collection foo collection"}]
                  Pulse      [_ {:name "pulse foo pulse"}]]
    (tu/boolean-ids-and-timestamps (set ((user->client :crowberto) :get 200 "search", :q "foo")))))

;; Basic search should only return substring matches
(expect
  default-search-results
  (tt/with-temp* [Card       [_ {:name "card foo card"}]
                  Card       [_ {:name "card bar card"}]
                  Dashboard  [_ {:name "dashboard foo dashboard"}]
                  Dashboard  [_ {:name "dashboard bar dashboard"}]
                  Collection [_ {:name "collection foo collection"}]
                  Collection [_ {:name "collection bar collection"}]
                  Pulse      [_ {:name "pulse foo pulse"}]
                  Pulse      [_ {:name "pulse bar pulse"}]]
    (tu/boolean-ids-and-timestamps (set ((user->client :crowberto) :get 200 "search", :q "foo")))))

(defn- archived [m]
  (assoc m :archived true))

;; Should return unarchived results by default
(expect
  default-search-results
  (tt/with-temp* [Card       [_ {:name "card foo card"}]
                  Card       [_ (archived {:name "card foo card2"})]
                  Dashboard  [_ {:name "dashboard foo dashboard"}]
                  Dashboard  [_ (archived {:name "dashboard foo dashboard2"})]
                  Collection [_ {:name "collection foo collection"}]
                  Collection [_ (archived  {:name "collection foo collection2"})]
                  Pulse      [_ {:name "pulse foo pulse"}]]
    (tu/boolean-ids-and-timestamps (set ((user->client :crowberto) :get 200 "search", :q "foo")))))

;; Should return archived results when specified
(expect
  (set (remove #(= "pulse" (:type %)) default-search-results))
  (tt/with-temp* [Card       [_ (archived {:name "card foo card"})]
                  Card       [_ {:name "card foo card2"}]
                  Dashboard  [_ (archived {:name "dashboard foo dashboard"})]
                  Dashboard  [_ {:name "dashboard foo dashboard2"}]
                  Collection [_ (archived {:name "collection foo collection"})]
                  Collection [_ {:name "collection foo collection2"}]
                  Pulse      [_ {:name "pulse foo pulse"}]]
    (tu/boolean-ids-and-timestamps (set ((user->client :crowberto) :get 200 "search",
                                         :q "foo", :archived "true")))))

;; Search within a collection will omit the collection, only return cards/dashboards/pulses in the collection
(expect
  (set (remove #(= "collection" (:type %)) default-search-results))
  (tt/with-temp* [Collection [{coll-id :id} {:name "collection foo collection"}]
                  Card       [_ {:name "card foo card", :collection_id coll-id}]
                  Card       [_ {:name "card foo card2"}]
                  Dashboard  [_ {:name "dashboard foo dashboard", :collection_id coll-id}]
                  Dashboard  [_ {:name "dashboard bar dashboard2"}]
                  Pulse      [_ {:name "pulse foo pulse", :collection_id coll-id}]
                  Pulse      [_ {:name "pulse foo pulse2"}]]
    (tu/boolean-ids-and-timestamps (set ((user->client :crowberto) :get 200 "search", :q "foo", :collection_id coll-id)))))
