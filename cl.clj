(ns cl)
(defn encrypt [message m]
  (let [phase-count (* 2 (dec m))
        phases (map
                 (fn [first-index]
                   (take-nth phase-count
                             (drop first-index (range))))
                 (range phase-count))
        indexs (take
                (count message)
                (loop [gs (rest phases)
                       indexs (take-while
                               #(> (count message) %)
                               (first phases))]
                  (if
                    (= 1 (count gs))
                    (concat indexs (last gs))
                    (recur
                      (rest (drop-last gs))
                      (concat
                        indexs
                        (take-while
                          #(> (count message) %)
                          (interleave (first gs) (last gs))))))))]
    (apply str indexs)))
(defn decrypt [message m]
  (let [phase-count (* 2 (dec m))
        phases (map
                 (fn [first-index]
                   (take-nth phase-count
                             (drop first-index (range))))
                 (range phase-count))
        indexs (take
                (count message)
                (loop [gs (rest phases)
                       indexs (take-while
                               #(> (count message) %)
                               (first phases))]
                  (if
                    (= 1 (count gs))
                    (concat indexs (last gs))
                    (recur
                      (rest (drop-last gs))
                      (concat
                        indexs
                        (take-while
                          #(> (count message) %)
                          (interleave (first gs) (last gs))))))))
        inv-indexs (->> indexs
                       (map-indexed vector)
                       (sort-by second)
                       (map first))]
    (apply str (map #(nth (clojure.string/replace message "_" " ") %) inv-indexs))))