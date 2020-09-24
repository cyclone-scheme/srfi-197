(import (scheme base)
        (scheme process-context)
        (scheme write)
        (cyclone test)
        (srfi 197)
        (srfi 2))

(define (exclamation x) (string-append x "!"))

(define (foo+bar x) (values (string-append x "foo") (string-append x "bar")))

(test-group "Pipeline Operators"

(test "chain" "bazbarfoo!"
  (chain ""
         (string-append "foo" _)
         (string-append "bar" _)
         (string-append "baz" _)
         (exclamation _)))

(test "chain with mixed _ position" "barfoobaz"
  (chain ""
         (string-append _ "foo")
         (string-append "bar" _)
         (string-append _ "baz")))

(test "chain with _ in operator position" 3
  (chain +
         (_ 1 2)))

(test "chain without _" "barbazqux"
  (chain ""
         (string-append _ "foo")
         (string-append "bar" "baz")
         (string-append _ "qux")))

(test "chain multiple _" "quxfoo/quxbar"
  (chain "qux"
         (foo+bar _)
         (string-append _ "/" _)))

(test "chain _ ..." "bazquxfooquxbar"
  (chain "qux"
         (foo+bar _)
         (string-append "baz" _ ...)))

(test "chain _ _ ..." "quxfoobazquxbar"
  (chain "qux"
         (foo+bar _)
         (string-append _ "baz" _ ...)))

(test "chain with custom _" "bazbarfoo!"
  (chain "" <>
         (string-append "foo" <>)
         (string-append "bar" <>)
         (string-append "baz" <>)
         (exclamation <>)))

(test "chain with custom ..." "bazquxfooquxbar"
  (chain "qux" - ---
         (foo+bar -)
         (string-append "baz" - ---)))

(test "chain-and" "bazbarfoo!"
  (chain-and ""
             (string-append "foo" _)
             (string-append "bar" _)
             (string-append "baz" _)
             (exclamation _)))

(test "chain-and with mixed _ position" "barfoobaz"
  (chain-and ""
             (string-append _ "foo")
             (string-append "bar" _)
             (string-append _ "baz")))

(test "chain-and without _" "barbazqux"
  (chain-and ""
             (string-append "foo" _)
             (string-append "bar" "baz")
             (string-append _ "qux")))

(test "chain-and short-circuit" #f
  (chain-and ""
             (string-append "foo" _)
             (equal? _ "bar")
             (string-append "baz" _)
             (exclamation _)))

(test "chain-and short-circuit first" #f
  (chain-and #f
             (not _)))

(test "chain-and with custom _" "bazbarfoo!"
  (chain-and "" <>
             (string-append "foo" <>)
             (string-append "bar" <>)
             (string-append "baz" <>)
             (exclamation <>)))

(test "chain-when" "bazfoo"
  (chain-when ""
              ((= (+ 2 2) 4) (string-append "foo" _))
              ((= (+ 2 2) 5) (string-append "bar" _))
              (#t (string-append "baz" _))))

(test "chain-when with mixed _ position" "barfooqux"
  (chain-when ""
              (#t (string-append _ "foo"))
              (#t (string-append "bar" _))
              (#f (string-append _ "baz"))
              (#t (string-append _ "qux"))))

(test "chain-when without _" "barqux"
  (chain-when ""
              (#t (string-append _ "foo"))
              (#t (string-append "bar"))
              (#f (string-append _ "baz"))
              (#t (string-append _ "qux"))))

(test "chain-when with custom _" "bazfoo"
  (chain-when "" <>
              ((= (+ 2 2) 4) (string-append "foo" <>))
              ((= (+ 2 2) 5) (string-append "bar" <>))
              (#t (string-append "baz" <>))))

(test "chain-lambda" "bazbarfoo!"
  ((chain-lambda (string-append "foo" _)
                 (string-append "bar" _)
                 (string-append "baz" _)
                 (exclamation _))
   ""))

(test "chain-lambda one step" "foobar"
  ((chain-lambda (string-append "foo" _)) "bar"))

(test "chain-lambda with mixed _ position" "barfoobaz"
  ((chain-lambda (string-append _ "foo")
                 (string-append "bar" _)
                 (string-append _ "baz"))
   ""))

(test "chain-lambda multiple _" "foobarbazqux"
  ((chain-lambda (string-append _ "bar" _)
                 (string-append _ "qux"))
   "foo"
   "baz"))

(test "chain-lambda without _" "barqux"
  ((chain-lambda (string-append "bar")
                 (string-append _ "qux"))))

(test "chain-lambda _ ..." "foobarbazqux"
  ((chain-lambda (string-append "foo" _ ...)
                 (string-append _ "qux"))
   "bar"
   "baz"))

(test "chain-lambda _ _ ..." "foobarbazquxquux"
  ((chain-lambda (string-append _ "bar" _ ...)
                 (string-append _ "quux"))
   "foo"
   "baz"
   "qux"))

(test "chain-lambda with custom _" "bazbarfoo!"
  ((chain-lambda <>
                 (string-append "foo" <>)
                 (string-append "bar" <>)
                 (string-append "baz" <>)
                 (exclamation <>))
   ""))

(test "chain-lambda with custom ..." "foobarbazqux"
  ((chain-lambda - ---
                 (string-append "foo" - ---)
                 (string-append - "qux"))
   "bar"
   "baz"))

(test "nest" '(1 2 (3 (4) 5))
  (nest (quote _)
        (1 2 _)
        (3 _ 5)
        (_)
        4))

(test "nest with custom _" '(1 2 (3 (4) 5))
  (nest <>
        (quote <>)
        (1 2 <>)
        (3 <> 5)
        (<>)
        4))

(test "nested nest" '(1 2 3 (4 5 6))
  (nest (nest _2 (quote _2) (1 2 3 _2) _ 6)
        (_ 5 _2)
        4))

(test "nest-reverse" '(1 2 (3 (4) 5))
  (nest-reverse 4
                (_)
                (3 _ 5)
                (1 2 _)
                (quote _)))

(test "nest-reverse with custom _" '(1 2 (3 (4) 5))
  (nest-reverse 4 <>
                (<>)
                (3 <> 5)
                (1 2 <>)
                (quote <>)))

)
(test-exit)
