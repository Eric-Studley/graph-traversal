(ns graph-traversal.core
  (:gen-class))

(defn create-nodes [n]
  (into [] (range 1 (+ 1 n))))

;;Should I change this (and all the other ones to a reduction function rather than a loop?)
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
           ;;(into graph {v {rand (+ (rand-int 9) 1)}})))))))

(assoc {} (keyword (str 1))  (assoc {} (rand-int 9) (+ (rand-int 9) 1)))

;;I think this is bugged and sometimes it produces a few less edge than its supposed to havent figured out why yet
(defn add-random-edge
  "Add a random edge to a specified graph"
  [graph]
  (let [key (rand-nth (keys graph))]
    (assoc-in graph [key] (conj (get-in graph [key]) (assoc {} (+ (rand-int 9) 1) (+ (rand-int 9) 1))))))

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

(def ^:private inf (Integer/MAX_VALUE))
(require '[clojure.data.priority-map :refer [priority-map]])

(defn neighbors
  "Returns n's neighbors, optionally filtered if unvisited"
  ([g n] (get g n {}))
  ([g n uv] (select-keys (neighbors g n) uv)))

(defn update-children
  [graph current unvisited children]
  (merge children
         (conj children (zipmap (keys (neighbors graph current unvisited)) (repeat current)))))

;;somehow update the tentative cost of all the neighbours
(defn update-costs
  [graph costs unvisited curr]
  (let [curr-cost (get costs curr)]
    (reduce-kv
     (fn [c nbr nbr-cost]
       (if (unvisited nbr)
         (update-in c [nbr] min (+ curr-cost nbr-cost))
         c))
     costs
     (neighbors graph curr unvisited))))


(use 'clojure.data)
;;come up with a better name
(defn dijkstra-loop
  [graph start end]
  (println graph)
  (loop [costs (conj (priority-map) (assoc (zipmap (keys graph) (repeat inf)) start 0))
         current (int start)
         unvisited (disj (apply hash-set (keys graph)) start)
         visitied [start]
         path (assoc (zipmap (keys graph) (repeat 0)) start 0)]

    (if (= current end)
      costs
      (let [new-costs (update-costs graph costs unvisited current)
            nextnode (key (first (apply dissoc new-costs visitied)))
            [x y z] (diff new-costs costs)
            children (zipmap (keys x) (repeat current))]
        (recur
         (conj (priority-map) new-costs)
         (int nextnode)
         (disj unvisited current)
         (conj visitied nextnode)
         (merge path children))))))


(defn dijkstra
  [graph start end]
  ;;(println graph)
  (let [costs (dijkstra-loop graph start end)
        traversals (into {} (filter #(not= (second %) inf)
                                    costs))]
    (println costs)
    (if-not (contains? traversals end)
      (println "No Path between " start " and " end)

      ;; (println (last (into [] (filter #(not= (second %) inf)
      ;;                               costs)))
      (assoc {} :path (keys traversals)
             :total-distance (get (into {} (filter #(not= (second %) inf)
                                                   costs)) end))
    ;;  (println (last (into {} (filter #(not= (second %) inf)
                                    ;;  costs)))))
      )))

(def g (make-graph 10 20))
(dijkstra g 1 10)


(defn -main

  "I don't do a whole lot ... yet."

  [& args]

  (println "Hello, World!"))


