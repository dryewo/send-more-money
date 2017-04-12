(ns me.dryewo.send-more-money
  (:require [midje.sweet :refer :all]
            [loco.core :refer [solutions solution]]
            [loco.constraints :refer :all]
            [instaparse.core :as insta]
            [instaparse.transform :as instatr])
  (:gen-class))

(defn -main [& args]
  (println "Please use REPL-driven workflow"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Intro

(comment

  ;; SEND + MORE = MONEY
  ;; 9567 + 1085 = 10652

  )

(comment

  ;; 2 parts:
  ;; - parse the string "SEND+MORE=MONEY"   <- aengelberg/loco
  ;; - solve the problem                    <- Engelberg/instaparse

  )

;; Loco
(comment

  ;; X in [1, 6]
  ;; Y in [3, 7]
  ;; X + Y = 10

  ;; Such declarative
  (def model
    [($in :x 1 6)                                           ; x is in the domain ranging from 1 to 6, inclusive
     ($in :y 3 7)                                           ; y is in the domain ranging from 3 to 7, inclusive
     ($= ($+ :x :y) 10)])

  (solution model)
  (solutions model)

  )

;; Instaparse
(comment

  ;; Much declarative
  (def simple-parser (insta/parser "
    Equals = Expr '=' Expr
    Expr = #'\\w+'
    "))

  (simple-parser "A=B")

  ;; Can transform
  (instatr/transform
    {:Expr (fn [x] x)}
    (simple-parser "A=B"))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

(defn format-solution [text sol]
  (apply str (for [t text]
               (if-let [n (->> t str keyword (get sol))]
                 n
                 t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific solution

(defn send-more-money []
  (solution (concat
              (for [v [:E :N :D :O :R :Y]]
                ($in v 0 9))
              (for [v [:S :M]]
                ($in v 1 9))
              [($distinct [:S :E :N :D :M :O :R :Y])
               ($= ($+ ($scalar [:S :E :N :D] [1000 100 10 1])
                       ($scalar [:M :O :R :E] [1000 100 10 1]))
                   ($scalar [:M :O :N :E :Y] [10000 1000 100 10 1]))])))

(defn solve1 []
  (format-solution "SEND+MORE=MONEY" (send-more-money)))

(facts ""
  (solve1) => "9567+1085=10652")

(comment
  (Character/toLowerCase \v)
  (->> \v (#(Character/toLowerCase ^Character %)) str keyword)
  (set (map (comp keyword str) "sendmoremoney"))

  (for [sol (send-more-money)]
    (format-solution "SEND+MORE=MONEY" sol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic solution

(def parser (insta/parser "
Equals = Expr <'='> Expr
<Expr> = Sum | Diff | Node
Sum = Node <'+'> Node
Diff = Node <'-'> Node
Node = #'\\w+'
"))

(defn ->kw-vec [text]
  (mapv (comp keyword str) text))

(defn parse-tree [text]
  (let [raw-tree (parser text)]
    (when (insta/failure? raw-tree)
      (throw (ex-info "Error parsing" {:text text :result raw-tree})))
    (instatr/transform {:Node (fn [node] [:Node (->kw-vec node)])} raw-tree)))

(defn branch? [x]
  (and (sequential? x) (not= :Node (first x))))

(defn nodes [tree]
  (map second (remove branch? (tree-seq branch? next tree))))

(comment
  (tree-seq branch? next (parse-tree "AA+BB=CC"))
  (instatr/transform {:Equals #(vector %1 %2)
                      :Sum    #(vector %1 %2)
                      :Diff   #(vector %1 %2)
                      :Node   identity}
                     (parse-tree "AA+BB=CC"))
  )

(defn var-domains [var-vecs]
  (let [first-chars (set (map first var-vecs))
        all-chars   (set (apply concat var-vecs))]
    (concat (for [v first-chars]
              ($in v 1 9))
            (for [v (clojure.set/difference all-chars first-chars)]
              ($in v 0 9)))))

(defn expr-constraint [tree]
  (instatr/transform
    {:Equals #($= %1 %2)
     :Sum    #($+ %1 %2)
     :Diff   #($- %1 %2)
     :Node   (fn [vs]
               ($scalar (reverse vs)
                        (take (count vs) (iterate #(* 10 %) 1))))}
    tree))

;; Main part, problem statement
(defn build-problem-statement [text]
  (let [expr-tree (parse-tree text)
        var-vecs  (nodes expr-tree)
        domains   (var-domains var-vecs)
        all-vars  (set (apply concat var-vecs))
        constr    (expr-constraint expr-tree)]
    (concat domains
            [($distinct all-vars)]
            [constr])))

(comment

  (parser "SEND+MORE=MONEY")
  (parse-tree "SEND+MORE=MONEY")
  (nodes (parse-tree "SEND+MORE=MONEY"))
  (var-domains (nodes (parse-tree "SEND+MORE=MONEY")))
  (set (apply concat (nodes (parse-tree "SEND+MORE=MONEY"))))
  (expr-constraint (parse-tree "SEND+MORE=MONEY"))

  (time (solve2 "SEND+MORE=MONEY"))

  )

(defn solve2 [text]
  (for [sol (solutions (build-problem-statement text))]
    (format-solution text sol)))

;; TESTS

(facts "about solve-and-format"
  (solve2 "SEND+MORE=MONEY") => ["9567+1085=10652"]
  (solve2 "SEND=MONEY-MORE") => ["9567=10652-1085"])

(facts "about to-kw-vec"
  (->kw-vec "Ab") => [:A :b])

(facts "about parse-tree"
  (parse-tree "A+B=C") => [:Equals [:Sum [:Node [:A]] [:Node [:B]]] [:Node [:C]]])

(facts "about nodes"
  (nodes (parse-tree "AA+BB=CC")) => [[:A :A] [:B :B] [:C :C]])

(facts "about var-domains"
  (var-domains [[:S :M :S]]) => [($in :S 1 9) ($in :M 0 9)])

(comment
  (solutions (build-problem-statement "SEND+MORE=MONEY"))
  (solve2 "SEND+MOST=MONEY")
  (time (count (solution (build-problem-statement "SEND+MORE=MONEY"))))
  (let [statement (build-problem-statement "SEND+MORE=MONEY")]
    (time (count (solution statement))))
  (time (count (solutions (build-problem-statement "AAA+BBB=CCDD"))))
  (solve2 "AAA+BBB=CCDD")
  ($in :S 0 1)
  (var-domains ["SMS"])
  (re-seq #"w" "a")
  (instatr/transform {:Assertion (fn [l r]
                                   ($= l r))
                      :Sum       (fn [a b] ($+ a b))
                      :Node      (fn [vs] ($scalar (reverse vs) (take (count vs) (iterate #(* 10 %) 1))))}
                     (parse-tree "SEND+MORE=MONEY"))
  (solution [($in :a 0 9)
             ($in :b 0 9)
             ($= 15 ($scalar [:b :a] (iterate #(* 10 %) 1)))])
  (instatr/transform {:Node ->kw-vec} (parser "SEND+MORE=MONEY"))
  (remove sequential? (tree-seq sequential? next [:Assertion [:Sum "SEND" "MORE"] "MONEY"])))
