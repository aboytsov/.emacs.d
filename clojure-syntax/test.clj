(let [a b c d e f g h])
;; -- general
;; 80 character warning sign ---------------------------------------------- hereOVER LIMIT
;; Comment
;; TODO: to do
;; FIXME: fix me
;; BUG: bug

(a b c d)

(    let (a b c d e f))

(a)

(a (
    a v))

(let [a b c d e f g h])

((parens-are-dimmed-a-little)
 a
 b
 c
 )

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

;; TODO: comments including this one
(comment
  (defn
    (code)
    (code)))

;; -- macro-writing
~data
~(data)
'data
`data
'~data
~'data
`~data
~@data
~@(map identity data)
data#
(eval data)

;; -- definitions
(let let-form)
[a b a b a b ]
(let [n 1 n 2] a)
(let [name1 1
    name2 2
    hello (func-call 1)
    {:keys [a b] :as f} destructured]
  code)
(def var-name data)
(def+ var-name data)
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

(first a)
(last a)
(rest a)
(butlast a)

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

(nil (nil 1
      2
      3))

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

(first-argument-is-symbol nil
                          1 2)

(first-argument-is-symbol nil
                          nil nil)

(first-argument-is-symbol [nil
                           1 2
                           nil]
                          [nil
                           1 2 [1
                                nil
                                [nil
                                 2]]])

(fn [a
    b]
  (code))

(defn name [a
          b]
  (code)
  (code))

(defn name [nil nil nil b]
  (code)
  (code))

(nil (defn name
     [a b c]
     (code)
     (code)))

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
  (code)
  (code))

(defn [d e f g]
  (code)
  (code))

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

(if
    (nil (complement a)
       2
       [3
        (complement b)
        4])
  one
  two)

(do (func-1)
    (func-2))

(do (complement string)
    (func-2))

;; namespaces here!
(defmacro yop
  `(ns (:require [clojure.string :as s])
     (:gen-class)))

;; new
;;; font locking
(ns clojure-mode.demo
  (:require
    [clojure.something]
    [something.s]))

(comment ;; for indentation
  (with-hi heya
    somebuddy)

  (deftoggle cap
    gabba)

  (couch/with-db hi
    your-db)

  (clo/defguppy gurgle
    minnow))

;; character literals
[\a \newline \u0032 \/ \+ \,, \;]

;; namespaced/static calls/references
(core.foo-baz/bar)
@foo-bar/bar
(FooBar/bar)
(some.package.FooBar/baz)

;; cljx
(  let [ ;; agfadg
     ;; adgfafg
      buf +clj (StringBuilder.) +cljs (gstring/StringBuffer.) a

    ])


(defn x-to-string
  [x]
  (let [buf #+clj (StringBuilder.) #+cljs (gstring/StringBuffer.)]
    (.append buf "x is: ")
    (.append buf (str x))))

;; metadata doesn't break docstrings
(defn max
  "Returns the greatest of the nums."
  {:added "1.0"
   :inline-arities >1?
   :inline (nary-inline 'max)}
  ([x] x)
  ([x y] (. clojure.lang.Numbers (max x y)))
  ([x y & more]
     (reduce1 max (max x y) more)))

(defn ^String reverse
  "Returns s with its characters reversed."
  {:added "1.2"}
  [^CharSequence s]
  (.toString (.reverse (StringBuilder. s))))

;; useful for testing docstring filling
(defn say-hello
  "This is a long doc string to test clojure-fill-docstring. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus sed nunc luctus leo ultricies semper. Nullam id tempor mi. Cras adipiscing scelerisque purus, at semper magna tincidunt ut. Sed eget dolor vitae enim feugiat porttitor. Etiam vulputate pulvinar lacinia. Nam vitae nisl sit amet libero pulvinar pretium nec a dui. Ut luctus elit eu nulla posuere nec feugiat ipsum vehicula. Quisque eu pulvinar neque. Fusce fermentum adipiscing mauris, sit amet accumsan ante dignissim ac. Pellentesque molestie mollis condimentum.

Etiam commodo nulla id risus convallis pharetra. Integer dapibus, eros vitae vehicula rhoncus, nisl lorem ornare magna, eu vehicula justo nunc ac nunc. In dolor sem, vulputate eget vulputate id, euismod eu ligula. Nullam elit augue, ultrices ut pretium vel, bibendum sit amet est. Curabitur vulputate arcu vitae neque adipiscing vel commodo ante faucibus. Cras tempor placerat erat. Sed ultrices faucibus sodales. Vestibulum sollicitudin consectetur mauris, nec mollis quam accumsan ultrices. Vestibulum tincidunt libero a lectus condimentum et fermentum diam eleifend. Nam accumsan interdum neque nec aliquet. Praesent feugiat dui at est rhoncus lacinia."
  []
  (println "Hello, World!"))
