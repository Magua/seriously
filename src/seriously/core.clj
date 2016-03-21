(ns seriously.core
  (:use [clojure.test :only (is run-tests function?)]
        [clojure.repl :only (doc)]
        [test.core :only [is=]]))

(defn get-hash [type data]
  (.digest (java.security.MessageDigest/getInstance type) (.getBytes data)))

(defn sha1-hash [data]
  (get-hash "sha1" data))

(defn
  ^{:doc  "sha 1 hasher from stack overflow gist: https://gist.github.com/prasincs/827272"
    :test (fn []
            (is (= (get-hash-str (sha1-hash "dallas")) "23f2916e01209d6282f226be9677affaec44a8d6"))
            )}
  get-hash-str [data-bytes]
  (apply str
         (map
           #(.substring
             (Integer/toString
               (+ (bit-and % 0xff) 0x100) 16) 1)
           data-bytes)
         ))

(defn sha1 [string]
  (get-hash-str (sha1-hash string)))

(defn
  ^{:doc  "next permutaion calculator ported from Scala

  def calcNextSeq(currentSeq: Seq[Int], max: Int): Seq[Int] = {

      def _cns(nextSeq: Seq[Int]): Seq[Int] = nextSeq match {
        case Nil                              => Seq(1)
        case allButLast :+ last if last < max => allButLast :+ last + 1
        case allButLast :+ last               => _cns(allButLast) :+ 1
      }

      _cns(currentSeq)
  }"
    :test (fn []
            (is (= (calc-next-permutation [] 1) [1]))
            (is (= (calc-next-permutation [3] 3) [1 1]))
            (is (= (calc-next-permutation [1 2 3 3] 3) [1 3 1 1]))
            (is (= (calc-next-permutation [1 2 3 4 5] 5) [1 2 3 5 1]))
            (is (= (calc-next-permutation [2 2 3] 3) [2 3 1]))
            (is (= (calc-next-permutation [65 65 65] 65) [1 1 1 1]))
            )}
  calc-next-permutation [currentSeq len]

  (let [butlast (vec (butlast currentSeq))
        last (last currentSeq)]
    (cond
      (empty? currentSeq) [1]
      (< last len) (conj butlast (inc last))
      :else
      (conj (calc-next-permutation butlast len) 1))))

(defn
  ^{:doc  "Lazy permutaion seq"
    :test (fn []
            (is (= (take 2 (lazy-permutation-seq 3)) [[1] [2]]))
            (is (= (nth (lazy-permutation-seq 3) 119) [3 3 3 3]))
            (is (= (nth (lazy-permutation-seq 1) 0) [1]))
            )
    }
  lazy-permutation-seq
  ([len]
   (lazy-permutation-seq (calc-next-permutation [] len) len))
  ([previousSeq len]
   (lazy-seq (cons previousSeq (lazy-permutation-seq (calc-next-permutation previousSeq len), len)))))


(defn
  ^{:doc  "permutation seq to string ported from scala:

  def numToString(num: Seq[Int], characters: String): String = num.map(c => characters.charAt((c + \"\").toInt - 1)).mkString

  "
    :test (fn []
            (is (= (permutation-seq->string "adsl" [1]) "a"))
            (is (= (permutation-seq->string "adsl" [2]) "d"))
            (is (= (permutation-seq->string "adsl" [3]) "s"))
            (is (= (permutation-seq->string "adsl" [4]) "l"))
            (is (= (permutation-seq->string "adsl" [2 1 4 4 1 3]) "dallas"))
            )
    }
  permutation-seq->string [characters numSeq]
  (apply str (map (fn [i] (.charAt characters (dec i))) numSeq))
  )

(defn
  ^{:doc  ""
    :test (fn []
            (is (= (match-password? "dallas" "23f2916e01209d6282f226be9677affaec44a8d6") true))
            )
    }
  match-password? [password hash]
  (= (sha1 password) hash)
  )

(defn
  ^{:doc  "find password from hash"
    :test (fn []
            (is (= (hash->password "23f2916e01209d6282f226be9677affaec44a8d6" "adsl") "dallas"))
            )
    }
  hash->password [hash characters]
  (let [permutation-seq->string-partial (partial permutation-seq->string characters)]
    (->> (lazy-permutation-seq (.length characters))
         (map permutation-seq->string-partial)
         (filter #(= hash (sha1 %)))
         (first))))