(ns seriously.core
  (:use [clojure.test :only (is run-tests function?)]
        [clojure.repl :only (doc)]
        [test.core :only [is=]]))

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
            (is (= (calc-next-permutation [1 3 3] 3) [2 1 1]))
            (is (= (calc-next-permutation [65 65 65] 65) [1 1 1 1]))
            )}
  calc-next-permutation [currentSeq len]
  ; todo get allButLast and last without double reverse
  (let [last (last currentSeq)
        r (vec (reverse (rest (rseq currentSeq))))]
    (cond
      (empty? currentSeq) [1]
      (< last len) (conj r (inc last))
      :else
      (conj (calc-next-permutation r len) 1))
    )
  )

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
            (is (= (permutation-seq->string [1] "adsl") "a"))
            (is (= (permutation-seq->string [2] "adsl") "d"))
            (is (= (permutation-seq->string [3] "adsl") "s"))
            (is (= (permutation-seq->string [4] "adsl") "l"))
            (is (= (permutation-seq->string [2 1 4 4 1 3] "adsl") "dallas"))
            )
    }
  permutation-seq->string [numSeq characters]
  (apply str (map (fn [i] (.charAt characters (dec i))) numSeq))
  )