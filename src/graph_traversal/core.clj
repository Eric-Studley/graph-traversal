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
     (get graph curr))))

(defn dijkstra
  [graph start end]
  (loop [costs (conj (priority-map) (assoc (zipmap (keys graph) (repeat inf)) start 0))
         current (int start)
         unvisited (disj (apply hash-set (keys graph)) start)
         visitied [start]]
    (if (= current end)
      costs
      (let [updated-costs (update-costs graph costs unvisited current)
            nextnode (key (first (apply dissoc updated-costs visitied)))]
        (recur
         (conj (priority-map) updated-costs)
         (int nextnode)
         (disj unvisited current)
         (conj visitied nextnode))))))



(dijkstra (make-graph 10 20) 1 3)

(def mapw {:a 1 :b 4 :c 3})
(println mapw)
(keys {:keys :and, :some :values})
(def xxx (conj (priority-map) mapw))
(apply dissoc xxx #{:a})

(defn -main

  "I don't do a whole lot ... yet."

  [& args]

  (println "Hello, World!"))


