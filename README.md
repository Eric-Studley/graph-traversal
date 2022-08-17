Eric Studley

## Coding challenge for Audience republic 



Lein repl and the following commands should be enough to verify everything works.

- (def random-graph (make-graph 10 10))
- (dijkstra random-graph (first (keys random-graph)) (last (keys random-graph)))
- (eccentricity random-graph (first (keys random-graph)))
- (radius random-graph)
- (diameter random-graph)



###### Notes:

Even though I have never touched clojure or any functional programming language before ill admit this took me a really long time. ~3-4 hours a day for 7 days. Although a lot of that was reading on braveclojure.com. It took me a very long time to stop thinking in terms of OOP and in the end I'm not overly happy with the result.

The code does work and you get the right ouputs but Im positive the code could be improved dramaticaly with more in depth knowledge of clojure. 

I was going to add tests but after a week I feel like my time is up.

I do plan to come back to this in the future and improve upon it once I understand more.
