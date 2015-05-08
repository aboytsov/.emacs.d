;; -- general
;; 80 character warning sign ---------------------------------------------- hereOVER LIMIT
;; Comment
;; TODO: to do
;; FIXME: fix me
;; BUG: bug

((parens-are-dimmed-a-little))

"A string is a sequence of characters"
#"regexp"

[]
{}
#{}
[] {} #{}
[data] {data} #{data}

true false nil    ;; specials
:keyword1 :keyword2

% %1 %2           ;; lambda params (TODO: prettify)
@deref
#'var

;; TODO: comments including
(comment
  (defn
    (code)
    (code)))

;; -- macro-writing
~data
'data
`data
'~data
~'data
`~data
data#
(eval data)

;; -- definitions
(let let-form)
[a b a b a b ]
(let [n 1 n 2] a)
(let [name1 1
    name2 2
    hello
    {:keys [a b] :as f} destructured]
  code)
(def var-name data)
(def+ var-name data)

(defn xxoxoxox)

(defn aa)
(def a)

#("q" 0 1
  (face
   (:foreground "green")
   help-echo "help-text" intangible def-decorator))

( sdfsdf)
(def)
(def  )
(defnjkk    123)
(defnsadfsaklfj)
(defn 123)
(fn 1)
(defn 123)
(fn [arg1 arg2 & argn])
(defn+ )
(defmacro )
(de func-name [arg1 arg2 & argn])
(defmacro macro-name [arg1 arg2 & argn])
(fn+ [arg1 arg2 argn])
(defn+ func-name [arg1 arg2 argn])
(qweqw def-decorator ew :name [arg1 arg2 argn])

;; -- java
JavaClass
(.methodCall "param")
(JavaClass/static-method)

;; -- debugging
TODO:

;; -- prettification
nil
(into [] empty-vector)
(into {} empty-map)
(into #{} empty-set)

(def var-name data)
(def+ var-name data)
(defn func-name [two-unnamed-vars _ _ & argn])
(defmacro macro-name [arg1 arg2 & argn])
(fn [arg1 arg2 & argn])
(fn+ [arg1 arg2 argn])
(defn+ func-name [arg1 arg2 argn])
(def-decorator :name [arg1 arg2 argn])
#(shortcut-lambda %1 %2)




(>= 2 1)
(<= 1 2)
(= 4 5)

(-> f (inc))
(->> f (dec))

(map f t)
(mapv x f)
(vec f)
(vector x)

(| func1 func2)       ;; composition
(& juxt1 juxt2)       ;; juxtaposition
(*> func param)       ;; currying
(*< func param)

(apply sum 1 2 3)       ;; sum
(count f)             ;; count

;; should not be prettified
(partial 1 2)
(comp 1 2)

;; -- indentation
;; TODO: indentation in let and in maps as below
(let [variable-name
      (very-log-value)]
  {:key-1
     (a-very-long-function-call arg1 arg2)
   :key-1
     (a-very-long-function-call arg1 arg2)})

(just-some-function f
                    d
                    [a b
                     c d
                     e]
                    (fn-in-fn-call
                      f a
                      e g h j))

(arguments-on-next-line
  1 2 3
  4 5)

(nil 1
   2
   3)

(nil
  1
  2
  3)

(nil [1 2
    3 4]
   5)

(nil f [1 2
      3 4]
   5)

(func1 (func2
         [1 2 3]))

(nil (func2
     [1 2 3]))

(nil (nil
     [1 2 3]
     1
     2))

(nil (nil 4
      [1 2 3]
      1
      2))

(nil a (nil
       [1 2 3]
       1
       2))

(nil a (nil b
        [1 2 3]
        1
        2))

(nil (func
     [1 2 3]
     1
     2))

(func (nil
        [1
         2 3
         (nil (nil {:a 2
                :b 3
                :c (nil
                     1
                     2)}))]
        1
        2))

(nil [a
    b]
   4)

(fn [a
    b]
  code)

(defn name [a
          b]
  (code)
  (code))

(nil (defn name
     [1 2 3]
     1
     2))

(display-call-inside f
                     d
                     [a b
                      c d
                      e]
                     (nil 1
                        2
                        3))

(defn [d e
     f g]
  code
  code)

(defn [d e f g]
  code
  code)

(if [a b]
  one
  two)

(if-empty-let [a b]
  one
  two)

(if
    (very-long-expression)
  one
  two)

(if (nil 1
       2
       3)
  one
  two)

(if
    (nil 1
       2
       3)
  one
  two)

(defmacro def+
  "def which binds like let, e.g.:
  (def-let [{:keys [a b d]} {:a 1 :b 2 :d 3}])"      ;; WRONG
  [bindings]
  (let [let-expr (macroexpand `(let ~bindings))
    vars (filter #(not (.contains (str %) "__"))
                 (map first (partition 2 (second let-expr))))
    def-vars (map (fn [v] `(def ~v ~v)) vars)]
    (concat let-expr def-vars)))

;; namespaces here!
(defmacro yop
  `(ns (:require [clojure.string :as s])
     (:gen-class)))
