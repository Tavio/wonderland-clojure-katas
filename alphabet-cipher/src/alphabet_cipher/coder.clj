(ns alphabet-cipher.coder)

(def max-num 26)

(defn num-to-char [num]
  (char (+ 97 num)))

(defn char-to-num [c]
  (- (int (first  (clojure.string/lower-case (str c)))) 97))

(defn generate-column [start-num]
  (reduce (fn [column num]
            (conj column (str (clojure.string/lower-case
                               (num-to-char (mod (+ start-num num) max-num))))))
          []
          (range max-num)))

(def table
  (reduce (fn [table num]
            (assoc table
                   num
                   (generate-column num)))
            {}
            (range max-num)))

(defn encode-char [column row]
  (get (get table (char-to-num column)) (char-to-num row)))

(defn index-of [coll item]
  (let [pos (count (take-while #(not= item %) coll))]
    (if (< pos (count coll))
      pos
      nil)))

(defn decode-char [column encoded-char]
  (num-to-char (index-of (get table (char-to-num column))
                         (str encoded-char))))

(defn expand-keyword [keyword]
  (cons (first keyword)
        (lazy-seq (expand-keyword (str (clojure.string/join (rest keyword))
                                      (first keyword))))))

(defn encode [keyword message]
  (let [expanded-keyword (take (count message) (expand-keyword keyword))]
    (loop [remaining-keyword expanded-keyword
           remaining-message message
           encoded-message ""]
      (if (empty? remaining-message)
        encoded-message
        (recur (rest remaining-keyword)
               (rest remaining-message)
               (str encoded-message
                    (encode-char (first remaining-keyword) (first remaining-message))))))))

(defn decode [keyword message]
  (let [expanded-keyword (take (count message) (expand-keyword keyword))]
    (loop [remaining-keyword expanded-keyword
           remaining-message message
           decoded-message ""]
      (if (empty? remaining-message)
        decoded-message
        (recur (rest remaining-keyword)
               (rest remaining-message)
               (str decoded-message
                    (decode-char (first remaining-keyword) (first remaining-message))))))))

(defn decipher [cipher message]
  "decypherme")
