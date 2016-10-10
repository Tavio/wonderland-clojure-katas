(ns alphabet-cipher.coder)

(defn num-to-char [num]
  (char (+ 97 num)))

(defn char-to-num [c]
  (- (int (first  (clojure.string/lower-case (str c)))) 97))

(defn generate-column [start-num]
  (cons (str (num-to-char start-num))
        (lazy-seq (generate-column (mod (inc start-num) 26)))))

(defn encode-char [keyword-char message-char]
  (first
   (drop (char-to-num keyword-char)
         (generate-column (char-to-num message-char)))))

(defn decode-char [keyword-char message-char]
  (num-to-char (count
                (take-while (fn [encoded-char] (not= (str message-char) encoded-char))
                            (generate-column (char-to-num keyword-char))))))

(defn expand-keyword [keyword]
  (cons (first keyword)
        (lazy-seq (expand-keyword (str (clojure.string/join (rest keyword))
                                      (first keyword))))))

(defn encode [keyword message]
  (apply str (map
              (fn [keyword-char message-char]
                (encode-char keyword-char message-char))
              (take (count message) (expand-keyword keyword))
              message)))

(defn decode [keyword message]
  (apply str (map
              (fn [keyword-char message-char]
                (decode-char keyword-char message-char))
              (take (count message) (expand-keyword keyword))
              message)))

(defn decypher [original-message encoded-message]
  (first (drop-while (fn [candidate-letter]
                       (not= (str encoded-char)
                             (encode-char candidate-letter
                                          original-char)))
                     (map num-to-char (range 26)))))

(encode "scones" "meetmebythetree")

(decode "scones" "egsgqwtahuiljgs")

(decypher "meetmebythetree" "egsgqwtahuiljgs")
