(ns graph-traversal.core
  (:gen-class))

(defn create-nodes [n]
  (into [] (range 1 (+ 1 n))))

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
           (into graph {v {rand (+ (rand-int 9) 1)}})))))))

(defn add-random-edge
  "Add a random edge to a specified graph"
  [graph]
  (let [key (rand-nth (keys graph))]
    (assoc-in graph [key] (conj (get-in graph [key]) {(+ (rand-int 9) 1) (+ (rand-int 9) 1)}))))

(defn make-graph [n s]
  (let [graph (into (sorted-map) (create-connected-graph (create-nodes n)))]
    (loop [count (- s 9) g graph]
      (if (= (int count) 0)
        g
        (recur (dec count)
               (add-random-edge g))))))



(make-graph 10 20)



(defn -main

  "I don't do a whole lot ... yet."

  [& args]

  (println "Hello, World!"))


