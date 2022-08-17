(ns graph-traversal.core
  (:gen-class))


(def ^:private inf (Integer/MAX_VALUE))
(require '[clojure.data.priority-map :refer [priority-map]])

(defn create-nodes [n]
  (into [] (range 1 (+ 1 n))))

;;Should I change this (and all the other ones to a reduction function rather than a loop?)
;;Creates a spanning tree to ensure the graph is connected
(defn create-connected-graph
  [nodes]
  (let [start-node (rand-nth nodes)]
    (loop [v start-node explored #{start-node} remaining (remove #{start-node} (set nodes)) graph {}]
      (if (empty? remaining)
        (into graph {v {}})
        (let [rand (rand-nth remaining)]
          (recur
           rand
           (conj explored rand)
           (remove #{rand} remaining)
           (into graph (assoc {} v (assoc {} rand (+ (rand-int 9) 1))))))))))



;;I think this is bugged and sometimes it produces a few less edge than its supposed to havent figured out why yet
(defn add-random-edge
  "Add a random edge to a specified graph"
  [graph]
  (let [key (rand-nth (keys graph))]
    (assoc-in graph [key] (conj (get-in graph [key]) (assoc {} (rand-nth (keys (dissoc graph key))) (+ (rand-int 9) 1))))))

(defn make-graph [n s]
  (cond
    (<  s (- n 1))  "Not enough edges"
    (> s (* n (- n 1))) "Too many edges"
    :else
    (let [graph (create-connected-graph (create-nodes n))]
      (loop [count (- s 9) g graph]
        (if (= (int count) 0)
          g
          (recur (dec count)
                 (add-random-edge g)))))))


(defn neighbours
  ([g n] (get g n {}))
  ([g n uv] (select-keys (neighbours g n) uv)))


;;Update the cost of neighbours and keep track of parent nodes to backtrack path 
(defn update-costs
  [graph costs unvisited curr children]
  (let [curr-cost (get costs curr)]
    (reduce-kv
     (fn [[c cx] nbr nbr-cost]
       (if (unvisited nbr)
         [(update-in c [nbr] min (+ curr-cost nbr-cost)) (assoc cx nbr curr)]
         [c cx]))
     [costs children]
     (neighbours graph curr unvisited))))



;;come up with a better name
(defn dijkstra-loop
  [graph start end]
  (loop [costs (conj (priority-map) (assoc (zipmap (keys graph) (repeat inf)) start 0))
         current (int start)
         unvisited (disj (apply hash-set (keys graph)) start)
         visitied [start]
         path (assoc (zipmap (keys graph) (repeat 0)) start 0)]
    (if (= current end)
      [costs path]
      (let [[new-costs children] (update-costs graph costs unvisited current path)
            nextnode (key (first (apply dissoc new-costs visitied)))]
        (recur
         (conj (priority-map) new-costs)
         (int nextnode)
         (disj unvisited current)
         (conj visitied nextnode)
         children)))))

(defn get-path
  [nodes end]
  (loop [currentNode (get nodes end) path (list end)]
    (if (= currentNode 0)
      path
      (recur
       (get nodes currentNode)
       (conj path currentNode)))))


(defn dijkstra
  [graph start end]
  (let [[costs nodes] (dijkstra-loop graph start end)
        traversals (into (priority-map) (filter #(not= (second %) inf)
                                                costs))]
    (if-not (contains? traversals end)
      (assoc {} :path () :total-distance (get traversals end))
      (assoc {} :path (get-path nodes end)
             :total-distance (get traversals end)))))

(defn get-eccentricity
  [graph s]
  (assoc (sorted-map) s (into ()
                              (map (fn [[k v]]
                                     (get (dijkstra graph s k) :total-distance))
                                   (dissoc graph s)))))

(defn eccentricity
  [g s]
  (apply max (remove nil? (get (get-eccentricity g s) s))))

(defn get-distances
  [graph f]
  (f (sort (remove nil? (flatten  (map (fn [[k v]]
                                         (get (get-eccentricity graph k) k)) graph))))))
(defn radius
  [graph]
  (get-distances graph first))
(defn diameter
  [graph]
  (get-distances graph last))

(def random-graph (make-graph 10 10))
(dijkstra random-graph 1 10)
(eccentricity random-graph (first (keys random-graph)))
(radius random-graph)
(diameter random-graph)

(defn -main

  [& args]

  (println "Hello, World!"))


