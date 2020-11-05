;#lang lazy
(require rackunit)
; First, we needed to change our language so that we can write lazy code
; This allows all functions we define to be delayed in evaluating,
; meaning an expression like (if ... ... ...)
; would not immediately return an error
; for not immediately knowing the evaluation of the first "...",
; it instead processes in a call-by-need manner


; In order to check our lambda abstractions,
; we need a way to bypass the lazy language's call-by-need evaluation
; and create tests that evaluate immediately
(define (check-expect assertion expected)
  (check-equal? (! assertion) (! expected)))


; The following function outputs the n-th Fibonacci number 
; fib : Nat -> Nat
(define (fib n)
  (if (zero? n)
      0
      (if (= n 1)
          1
          (+ (fib (- n 2)) (fib (sub1 n))))))
(check-expect (fib 0) 0)
(check-expect (fib 6) 8)

; This is a great function for obtaing the n-th number in the Fibonacci sequence,
; but there are a lot of non-lambda components!
;; The recreation of this function using only lambda functions will be our end goal!

;; But how do we do this? Where to begin?

; We need to redefine each component to as its own lambda function
; - def fib
; - if
; - zero? --> bools
; - 0,1 --> nats
; - =(?)
; - +,-(?),sub1 --> arithmetic
; - fib --> recursion, different from define fib


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NATURAL NUMBERS
; Nats are used for counting,
; meaning how much of something or how many times something occurs


; recall the nat numbers template:
;; A Nat is one of:
;; - 0
;; - (sub1 (add1 Nat)) -> Nat
#;(define (nat-temp nat)
  (...
   (cond [(zero? nat) ...]
         [(positive? nat) ... (nat-temp (sub1 nat)) ...])))

; Since Nats can be defined as 0 or the successor of another Nat,
; we will define a lambda nat as the number of times we call for the successor


; λNat : [[X -> X] -> [X -> X]]
; represents the number of times a function is called on an input
; template:
#;(define (λnat-temp λnat)
  ((λnat ...) ...))

; for example...

; λzero should be a function that applies a function no times on an input
(define λzero
  (λ (func)
    (λ (n) n)))

; λone should be a function that applies a function one time on an input
(define λone
  (λ (func)
    (λ (n) (func n))))

; λtwo should be a function that applies a function two times on an input
(define λtwo
  (λ (func)
    (λ (n) (func (func n)))))

; λsix should be a function that applies a function six times on an input
(define λsix
  (λ (func)
    (λ (n) (func (func (func (func (func (func n)))))))))

; and so on...


; to create a λnat, we get the following template
; based on the common pattern from the given examples
#;(define λnat
  (λ (func)
    (λ (n) ...)))


; At this point, the question is as sound as this argument seems,
; how do we know we are defining natural numbers correctly?
;; How do we know if anything from this point on behaves as we
;; decided it logically should? --> the importance of TESTING!!!

; Since lambdas are anonymously define functions, we can't test
; the functionality of our lambda functions with just a simple check-expect.

; We need a function that will "translate" our lambda equivalents back to
; something "testable" -- their "real" numerical values --
; to test if our newly defined λnats "count" as they are expected to

; Recall our template for λnat functions
#;(define (λnat-temp λnat)
  ((λnat ...) ...))
;; the first ... should be the function we want to apply
;; the second ... should be the input

;; For our "translating function", we need to give a function
;; that will count each time the λnat is designed to apply a function
;; and our input should be the starting numerical value, 0,
;; if no functions are applied

; λnat->nat : λNat -> Nat
; counts the number of functions applied on the input
(define (λnat->nat lnat)
  ((lnat add1) 0))
(check-expect (λnat->nat λzero) 0)
(check-expect (λnat->nat λtwo) 2)
(check-expect (λnat->nat λsix) 6)

;; Note how this function uses a non-lambda component "add1"
;; This reminds us that nats are recursive
;; Isn't it inconvenient to have to define lambda nats separately
;; and one at a time??


; Since nats are recursively defined, we can make a function
; that finds the successor of a given λnat by applying a function
; one more time

; λadd1 : λNat -> λNat
; applies a given function one more time to the given input
(define λadd1
  (λ (lnat)
    (λ (func)
      (λ (input) (func ((lnat func) input))))))
(check-expect (λnat->nat (λadd1 λone)) 2)
(check-expect (λnat->nat (λadd1 λtwo)) 3)
(check-expect (λnat->nat (λadd1 λzero)) 1)


; Yay! We don't have to define every λnat separately now using our template
; and can define a λnat as the successor of another λnat

(define λthree
  (λadd1 λtwo))
(check-expect (λnat->nat λthree) 3)

(define λseven
  (λadd1 λsix))
(check-expect (λnat->nat λseven) 7)

(define λeight
  (λadd1 λseven))
(check-expect (λnat->nat λeight) 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ARITHMETIC
; Now that we have defined λnats, recalling our list of functions
; to redefine, we can lead into arithmetic such as '+' (addition)

; λ+ : λNat λNat -> λNat
; adds two lambda nats together
(define λ+
  (λ (n1 n2)
    (λ (func)
      (λ (input) ((n1 func) ((n2 func) input))))))
(check-expect (λnat->nat (λ+ λzero λone)) 1)
(check-expect (λnat->nat (λ+ λtwo λsix)) 8)


; But how do we do '-' (subtraction)?
; This is a bit difficult since we are not just adding
; to the number of times a function is applied to an input

; We need to develop something like the concept of negation
; in order to "cancel" out an applied function
;; So let's come back to arithmetic after redefining our other goals:
; - def fib
; - if
; - zero? --> bools
; - 0,1 --> nats                                  -- YAY! DEFINED!!
; - =(?)
; - +,-(?),sub1 --> arithmetic                    -- to be continued...
; - fib --> recursion, different from define fib

;; Looking through our list, a next good starting place should be booleans!
;; We've already defined λzero so how about getting to defining our
;; recognizer for λzero... the lambda equivalent to 'zero?'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BOOLEANS
; Booleans are data types with two possible values: 'true' or 'false'
; These two options are complete opposites!
; Thus, we can define booleans as something that is
; either 'true' or 'false' -- choosing one or the other

; λBool : [X X -> X]
; represents the choosing of an input
; - input1 => true
; - input2 => false


; λtrue : λBool
; chooses the first input option
(define λtrue
  (λ (in1 in2) in1))
(check-expect (λtrue 1 2) 1)
(check-expect (λtrue #true #false) #true)

; λfalse : λBool
; chooses the second input option
(define λfalse
  (λ (in1 in2) in2))
(check-expect (λfalse 1 2) 2)
(check-expect (λfalse #true #false) #false)


; Again, we need a way to check our logical reasoning
; for defining booleans in this way is sound
; Let's define a function that "translates" our λBools
; back into their boolean equivalents

; λbool->bool : λBool -> Boolean
; chooses #true if the given λBool chooses the first option
; chooses #false if the given λBool chooses the second option
(define (λbool->bool b)
  (b #true #false))
(check-expect (λbool->bool λtrue) #true)
(check-expect (λbool->bool λfalse) #false)


; Now that we have λbools defined as something that chooses one option
; over another, this is almost identical to the logic behind 'if'!

; Recall the syntax for 'if':
;; (if questionEXP thenEXP elseEXP)

; The 'if' function works exactly like a chooser
; except that it must evaluate the questionEXP
; to #true before choosing option1 (thenEXP)
; or #false before choosing option2 (elseEXP)

; λif : λBool X X -> X
; chooses the first option if the given λBool is λtrue
; chooses the second option if the given λBool is λfalse
(define λif
  (λ (b true-then false-else)
    (b true-then false-else)))
(check-expect (λif λtrue #true #false) #true)
(check-expect (λif λfalse #true #false) #false)


; And now we know enough to define our recognizer for λzero, λzero?

; λzero? : λNat -> λBool
; returns λtrue if the given λNat is λzero
; - remains λtrue if the given λNat does not apply any λfalse functions
(define λzero?
  (λ (n)
    ((n (λ (input) λfalse)) λtrue)))
(check-expect (λbool->bool (λzero? λzero)) #true)
(check-expect (λbool->bool (λzero? λone)) #false)


; 'Equivalence' was easy to define for recognizing λzero since
; λzero does not apply the given function to a given input
;; But equivalence in general is a tough thing to logically define
;; Thus we'll have to find another way to redefine "=" with lambdas

; Let's check back on our list:
; - def fib
; - if                                            -- Yay! DEFINED!!
; - zero? --> bools                               -- Yay! DEFINED!!
; - 0,1 --> nats                                  -- Yay! DEFINED!!
; - =(?)                                          -- to be continued...
; - +,-(?),sub1 --> arithmetic                    -- to be continued...
; - fib --> recursion, different from define fib


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ARITHMETIC -- revisited
; We are so close to defining sub1!!
; We need to somehow keep track of the recursive relationship between λnats...
;; specifically the successor relationship

; 'sub1' is the returning the predecessor of a given nat...
; if only we could somehow remember the previous nat and our current nat
; at the same time

; How about lists?
; Based on how we defined λbools as choosing one input or the other,
; if we had a list of size 2,
; we could store the current value and previous value
; -- the successor and its predecessor --,
; and we could choose the previous value to be returned when calling 'sub1'

; Let's define a Pair to contain our current nat and its successor!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PAIRS
; Why pairs? Well, we want very specific lists...
; - size 2
; - designed to store λnats, a current λnat and the one preceeding it
; - must be able to access these two items

; λNatPair : [X X -> [[X X -> X] -> X]]
; represents a list of size 2 that will return
; the first object or second object
; based on the given accessor function
(define λmake-np
  (λ (input1 input2)
    (λ (accessor) (accessor input1 input2))))
(check-expect ((λmake-np #true #false) λtrue) #true)
(check-expect ((λmake-np #true #false) λfalse) #false)

; λfirst : λNatPair -> X
; accesses the first object in a λnatpair
(define λfirst
  (λ (np)
    (np λtrue)))
(check-expect (λfirst (λmake-np #true #false)) #true)
(check-expect (λfirst (λmake-np 1 0)) 1)

; λsecond : λNatPair -> X
; accesses the second object in a λnatpair
(define λsecond
  (λ (np)
    (np λfalse)))
(check-expect (λsecond (λmake-np #true #false)) #false)
(check-expect (λsecond (λmake-np 1 0)) 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Now that we have a way to make our pairs for nats
; and access the nats within these pairs...
;; Let's define functions that relate the the two nats to
;; each other according to their recursive relationship

; λsuccNat : λNatPair -> λNatPair
; stores a nat in the second spot and stores the successor in the first
(define λsuccNat
  (λ (np)
    (λmake-np (λadd1 (λfirst np)) (λfirst np))))
(check-expect (λnat->nat
               (λfirst (λsuccNat (λmake-np λzero λzero)))) 1)
(check-expect (λnat->nat
               (λsecond (λsuccNat (λmake-np λzero λzero)))) 0)

; λsub1 : λNat -> λNat
; returns the stored lambda nat that applies a function one less time
(define λsub1
  (λ (lnat)
    (λsecond
     ((lnat λsuccNat)
      (λmake-np λzero λzero)))))
(check-expect (λnat->nat (λsub1 λtwo)) 1)
(check-expect (λnat->nat (λsub1 λthree)) 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; We still do not know how to do subtraction,
; but since we're only redefining simple math functions,
; we can convert (- n 2) to an equivalent "sub2" function
; running on the same premises as "sub1"!

; In order to return a nat that applies a function 2 times
; less than the current nat, we need a list of size 3
; that stores the current nat, the previous nat and the
; previous previous nat

; Let's define a Triple to contain
; - our current nat
; - its predecessor
; - its predecessor's predecessor 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TRIPLES!
; Again, we'll need a function to make a λnat triple,
; accessor functions,
; and function that relate the stored λnats according to
; their recursive relationship to be fully equipped for
; defining "sub2"

; λNatTriple : [X X X -> [[X X -> X][X X -> X] -> X]]
; represents a list of size 3 that will return
; the first object or second object or third object
; based on the given accessor function
(define λmake-ntrip
  (λ (input1 input2 input3)
    (λ (accessor1 accessor2)
      (accessor1 (accessor2 input1 input2) input3))))
(check-expect ((λmake-ntrip #true #false 0) λtrue λtrue) #true)
(check-expect ((λmake-ntrip #true #false 0) λfalse λtrue) 0)
(check-expect ((λmake-ntrip #true #false 0) λtrue λfalse) #false)
(check-expect ((λmake-ntrip #true #false 0) λfalse λfalse) 0)

; λfirstTrip : λNatTriple -> X
; accesses the first object in a λnattriple
(define λfirstTrip
  (λ (ntrip)
    (ntrip λtrue λtrue)))
(check-expect (λfirstTrip (λmake-ntrip #true #false 0)) #true)
(check-expect (λfirstTrip (λmake-ntrip 1 2 3)) 1)

; λsecondTrip : λNatTriple -> X
; accesses the second object in a λnattriple
(define λsecondTrip
  (λ (ntrip)
    (ntrip λtrue λfalse)))
(check-expect (λsecondTrip (λmake-ntrip #true #false 0)) #false)
(check-expect (λsecondTrip (λmake-ntrip 1 2 3)) 2)

; λthirdTrip : λNatTriple -> X
; accesses the third object in a λnattriple
; as long as the first accessor is λfalse the third value
; will be returned no matter what the second accessor is
(define λthirdTrip
  (λ (ntrip)
    (ntrip λfalse λfalse)))
(check-expect (λthirdTrip (λmake-ntrip #true #false 0)) 0)
(check-expect (λthirdTrip (λmake-ntrip 1 2 3)) 3)

; λsucc2Nat : λNatTriple -> λNatTriple
; stores a nat in the third spot, the successor in the second,
; and the successor of the successor in the first
(define λsucc2Nat
  (λ (ntrip)
    (λmake-ntrip (λadd1 (λfirstTrip ntrip))
                 (λfirstTrip ntrip)
                 (λsub1 (λfirstTrip ntrip)))))
(check-expect (λnat->nat
               (λfirstTrip
                (λsucc2Nat (λmake-ntrip λzero λzero λzero)))) 1)
(check-expect (λnat->nat
               (λsecondTrip
                (λsucc2Nat (λmake-ntrip λzero λzero λzero)))) 0)
(check-expect (λnat->nat
               (λthirdTrip
                (λsucc2Nat (λmake-ntrip λzero λzero λzero)))) 0)
(check-expect (λnat->nat
               (λfirstTrip
                (λsucc2Nat (λmake-ntrip λtwo λone λzero)))) 3)
(check-expect (λnat->nat
               (λsecondTrip
                (λsucc2Nat (λmake-ntrip λtwo λone λzero)))) 2)
(check-expect (λnat->nat
               (λthirdTrip
                (λsucc2Nat (λmake-ntrip λtwo λone λzero)))) 1)

; λsub2 : λNat -> λNat
; returns the stored lambda nat that applies a function two less times
(define λsub2
  (λ (lnat)
    (λthirdTrip
     ((lnat λsucc2Nat)
      (λmake-ntrip λzero λzero λzero)))))
(check-expect (λnat->nat (λsub2 λtwo)) 0)
(check-expect (λnat->nat (λsub2 λsix)) 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Now that we have found a way around having to define '-',
; we can check our progress on our list of goals
; and see what's components are left to redefine in order
; to redefine our fibonacci sequence function:
; - def fib
; - if                                            -- Yay! DEFINED!!
; - zero? --> bools                               -- Yay! DEFINED!!
; - 0,1 --> nats                                  -- Yay! DEFINED!!
; - =(?)                                          -- to be continued...
; - +,-(?)[=> sub2],sub1 --> arithmetic           -- Yay! DEFINED!!
; - fib --> recursion, different from define fib


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BOOLEANS -- revisted
; Equivalence again!
; We still do not know how to completely prove equivalence,
; especially with nats that are functions that count the number
; of times a given function has been applied on a given input

; But like how we decided to define a "sub2" function
; to get around subtraction,
; we can work our way around equivalence.

; Again, since we're only redefining simple math functions,
; we can convert (= n 1) to an equivalent recognizer function!

; If a given nat were equal to the value 1, that would be the
; same as saying the given nat IS 1
;; So we need a recognizer to check if a given λnat IS λone


; It's difficult to compare functions...
; But we already have defined the recognizer 'λzero?' to help us
;; Remembering the recursive relationship between nats,
;; we can define our recognizer, 'λone?' as:
;; - if the given λnat is not λzero
;; - then if the given λnat is not λzero,
;; the given λnat should be the successor to λzero

; λone? : λNat -> λBool
; recognizer for λone that check if the given λnat is not λzero
; then checks if the given λnat's predecessor is λzero
(define λone?
  (λ (n)
    (λif (λzero? n)
         λfalse
         (λif (λzero? (λsub1 n))
              λtrue
              λfalse))))
(check-expect (λbool->bool (λone? λzero)) #false)
(check-expect (λbool->bool (λone? λone)) #true)


; Now that we have found a way around having to define '=',
; we are very close to being fully equipped with lambda functions
; that are needed to redefine our fibonacci sequence function!!
;; Recall our list of TODOs:
; - def fib
; - if                                            -- Yay! DEFINED!!
; - zero? --> bools                               -- Yay! DEFINED!!
; - 0,1 --> nats                                  -- Yay! DEFINED!!
; - =(?)[==> one?]                                -- Yay! DEFINED!!
; - +,-(?)[==> sub2],sub1 --> arithmetic          -- Yay! DEFINED!!
; - fib --> recursion, different from define fib

;; All that is left is figuring out the logic behind recursion,
;; and how that relates to our main function, 'fib'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RECURSION & FIB
; Recursion --> repeat copy and pasting an entire function body,
; each copy/paste takes n-1 as input instead of n
; until we reach a stopping point (base case/non-recursive branch)
;; However, recannot refer to the entire function body as the
;; original function name so let's make it its own function

; original fib function
#;(define (fib n)
  (if (zero? n)
      0
      (if (= n 1)
          1
          (+ (fib (- n 2)) (fib (sub1 n))))))

; original fib function with lambda components and
; recursive calls marked differently
; from our orignal function name
#;(define λfib
    (λ (lnat)
      (λif (λzero? lnat)
           λzero
           (λif (λone? lnat)
                λone
                (λ+ (copy&pasteFib (λsub2 lnat)) (copy&pasteFib (λsub1 lnat)))))))

; fib function with lambda components
; and recursion given as an input as its own function called 'recur'
#;(define λfib
  (... λ (recur)
       (λ (lnat)
         (λif (λzero? lnat)
              λzero
              (λif (λone? lnat)
                   λone
                   (λ+ (recur (λsub2 lnat)) (recur (λsub1 lnat))))))))


;; Recursion:
; Now that we allocated 'recur' to be a function representing
; the entire body of the fib function,
; we need to figure out how to copy/paste 'recur' an x amount of
; times until we reach a base case or non-recursive branch

; Starting with the copy/paste functionality,
; we need to take 'recur' and given it back to the
; 'lnat' input function while maintaing the original
; fib function's signature (fib : λNat -> λNat),
;; where λNat : [[X -> X] -> [X -> X]]
;; which leads us to...
#;((λ (f) (f f))
   (λ (f) (f f)))
;; but this is an infinte loop...

; Now, we need to incorporate the 'limiter'
; where we only recur an 'x' number of times
; instead of recurring for infinity
;; We can give our current infinite loop of recursion
;; an input that 'x' that specifies the number of times
;; we wish to copie and paste the recurring function
;; which leads us to...
(define λcopyPaste
  (λ (x)
    ((λ (f) (f f))
     (λ (f) (x (f f))))))


; Alright! We should be fully equipped to redefine
; our fib function with lambdas now but let's
; check back on our list to be sure:
; - def fib                                       -- Yay! DEFINED!!
; - if                                            -- Yay! DEFINED!!
; - zero? --> bools                               -- Yay! DEFINED!!
; - 0,1 --> nats                                  -- Yay! DEFINED!!
; - =(?)[==> one?]                                -- Yay! DEFINED!!
; - +,-(?)[==> sub2],sub1 --> arithmetic          -- Yay! DEFINED!!
; - fib --> recursion, different from define fib  -- Yay! DEFINED!!

; Everything on our list is checked off so we can switch out
; each component in our fib function with their lambda equivalents
; that we have defined above!
#;(define λfib
  (λcopyPaste
   (λ (recur)
       (λ (lnat)
         (λif (λzero? lnat)
              λzero
              (λif (λone? lnat)
                   λone
                   (λ+ (recur (λsub2 lnat)) (recur (λsub1 lnat)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Finally, to redefine our fib function to only use lambdas,
; we simply need to replace every occurance of the components'
; lambda equivalents with the body of each of these anonymous functions!

(define λfib
  ((λ (x)
     ((λ (f) (f f))
      (λ (f) (x (f f)))))
   (λ (recur-fib)
     (λ (lnat)
       ((λ (b true-then false-else)
          (b true-then false-else))
        ((λ (n)
           ((n (λ (input) (λ (in1 in2) in2))) (λ (in1 in2) in1)))
         lnat)
        (λ (func) (λ (n) n))
        ((λ (b true-then false-else)
           (b true-then false-else))
         ((λ (n)
            ((λ (b true-then false-else)
               (b true-then false-else))
             ((λ (n)
                ((n (λ (input) (λ (in1 in2) in2))) (λ (in1 in2) in1)))
              n)
             (λ (in1 in2) in2)
             ((λ (b true-then false-else)
                (b true-then false-else))
              ((λ (n)
                 ((n (λ (input) (λ (in1 in2) in2))) (λ (in1 in2) in1)))
               ((λ (lnat)
                  ((λ (np)
                     (np (λ (in1 in2) in2)))
                   ((lnat (λ (np)
                            ((λ (input1 input2)
                               (λ (accessor) (accessor input1 input2)))
                             ((λ (lnat)
                                (λ (func)
                                  (λ (input) (func ((lnat func) input)))))
                              ((λ (np)
                                 (np (λ (in1 in2) in1))) np))
                             ((λ (np)
                                (np (λ (in1 in2) in1))) np))))
                    ((λ (input1 input2)
                       (λ (accessor) (accessor input1 input2)))
                     (λ (func)
                       (λ (n) n))
                     (λ (func)
                       (λ (n) n)))))) n))
              (λ (in1 in2) in1)
              (λ (in1 in2) in2))))
          lnat)
         (λ (func) (λ (n) (func n)))
         ((λ (n1 n2)
            (λ (func)
              (λ (input) ((n1 func) ((n2 func) input)))))
          (recur-fib
           ((λ (lnat)
              ((λ (ntrip)
                 (ntrip (λ (in1 in2) in2) (λ (in1 in2) in2)))
               ((lnat (λ (ntrip)
                        ((λ (input1 input2 input3)
                           (λ (accessor1 accessor2)
                             (accessor1 (accessor2 input1 input2) input3)))
                         ((λ (lnat)
                            (λ (func)
                              (λ (input) (func ((lnat func) input)))))
                          ((λ (ntrip)
                             (ntrip (λ (in1 in2) in1) (λ (in1 in2) in1)))
                           ntrip))
                         ((λ (ntrip)
                            (ntrip (λ (in1 in2) in1) (λ (in1 in2) in1)))
                          ntrip)
                         ((λ (lnat)
                            ((λ (np) (np (λ (in1 in2) in2)))
                             ((lnat
                               (λ (np)
                                 ((λ (input1 input2)
                                    (λ (accessor) (accessor input1 input2)))
                                  ((λ (lnat)
                                     (λ (func)
                                       (λ (input)
                                         (func ((lnat func) input)))))
                                   ((λ (np)
                                      (np (λ (in1 in2) in1)))
                                    np))
                                  ((λ (np)
                                     (np (λ (in1 in2) in1)))
                                   np))))
                              ((λ (input1 input2)
                                 (λ (accessor) (accessor input1 input2)))
                               (λ (func)
                                 (λ (n) n))
                               (λ (func)
                                 (λ (n) n))))))
                          ((λ (ntrip)
                             (ntrip (λ (in1 in2) in1) (λ (in1 in2) in1)))
                           ntrip)))))
                ((λ (input1 input2 input3)
                   (λ (accessor1 accessor2)
                     (accessor1 (accessor2 input1 input2) input3)))
                 (λ (func)
                   (λ (n) n))
                 (λ (func)
                   (λ (n) n))
                 (λ (func)
                   (λ (n) n))))))
            lnat))
          (recur-fib
           ((λ (lnat)
              ((λ (np) (np (λ (in1 in2) in2)))
               ((lnat
                 (λ (np)
                   ((λ (input1 input2)
                      (λ (accessor) (accessor input1 input2)))
                    ((λ (lnat)
                       (λ (func)
                         (λ (input) (func ((lnat func) input)))))
                     ((λ (np)
                        (np (λ (in1 in2) in1))) np))
                    ((λ (np)
                       (np (λ (in1 in2) in1))) np))))
                ((λ (input1 input2)
                   (λ (accessor) (accessor input1 input2)))
                 (λ (func)
                   (λ (n) n))
                 (λ (func)
                   (λ (n) n))))))
            lnat)))))))))


; Let's use our newly redefined λfib function to evaluate on an input...
(λnat->nat
 (((λ (x)
     ((λ (f) (f f))
      (λ (f) (x (f f)))))
   (λ (recur-fib)
     (λ (lnat)
       ((λ (b true-then false-else)
          (b true-then false-else))
        ((λ (n)
           ((n (λ (input) (λ (in1 in2) in2))) (λ (in1 in2) in1)))
         lnat)
        (λ (func) (λ (n) n))
        ((λ (b true-then false-else)
           (b true-then false-else))
         ((λ (n)
            ((λ (b true-then false-else)
               (b true-then false-else))
             ((λ (n)
                ((n (λ (input) (λ (in1 in2) in2))) (λ (in1 in2) in1)))
              n)
             (λ (in1 in2) in2)
             ((λ (b true-then false-else)
                (b true-then false-else))
              ((λ (n)
                 ((n (λ (input) (λ (in1 in2) in2))) (λ (in1 in2) in1)))
               ((λ (lnat)
                  ((λ (np)
                     (np (λ (in1 in2) in2)))
                   ((lnat (λ (np)
                            ((λ (input1 input2)
                               (λ (accessor) (accessor input1 input2)))
                             ((λ (lnat)
                                (λ (func)
                                  (λ (input) (func ((lnat func) input)))))
                              ((λ (np)
                                 (np (λ (in1 in2) in1))) np))
                             ((λ (np)
                                (np (λ (in1 in2) in1))) np))))
                    ((λ (input1 input2)
                       (λ (accessor) (accessor input1 input2)))
                     (λ (func)
                       (λ (n) n))
                     (λ (func)
                       (λ (n) n)))))) n))
              (λ (in1 in2) in1)
              (λ (in1 in2) in2))))
          lnat)
         (λ (func) (λ (n) (func n)))
         ((λ (n1 n2)
            (λ (func)
              (λ (input) ((n1 func) ((n2 func) input)))))
          (recur-fib
           ((λ (lnat)
              ((λ (ntrip)
                 (ntrip (λ (in1 in2) in2) (λ (in1 in2) in2)))
               ((lnat (λ (ntrip)
                        ((λ (input1 input2 input3)
                           (λ (accessor1 accessor2)
                             (accessor1 (accessor2 input1 input2) input3)))
                         ((λ (lnat)
                            (λ (func)
                              (λ (input) (func ((lnat func) input)))))
                          ((λ (ntrip)
                             (ntrip (λ (in1 in2) in1) (λ (in1 in2) in1)))
                           ntrip))
                         ((λ (ntrip)
                            (ntrip (λ (in1 in2) in1) (λ (in1 in2) in1)))
                          ntrip)
                         ((λ (lnat)
                            ((λ (np) (np (λ (in1 in2) in2)))
                             ((lnat
                               (λ (np)
                                 ((λ (input1 input2)
                                    (λ (accessor) (accessor input1 input2)))
                                  ((λ (lnat)
                                     (λ (func)
                                       (λ (input)
                                         (func ((lnat func) input)))))
                                   ((λ (np)
                                      (np (λ (in1 in2) in1)))
                                    np))
                                  ((λ (np)
                                     (np (λ (in1 in2) in1)))
                                   np))))
                              ((λ (input1 input2)
                                 (λ (accessor) (accessor input1 input2)))
                               (λ (func)
                                 (λ (n) n))
                               (λ (func)
                                 (λ (n) n))))))
                          ((λ (ntrip)
                             (ntrip (λ (in1 in2) in1) (λ (in1 in2) in1)))
                           ntrip)))))
                ((λ (input1 input2 input3)
                   (λ (accessor1 accessor2)
                     (accessor1 (accessor2 input1 input2) input3)))
                 (λ (func)
                   (λ (n) n))
                 (λ (func)
                   (λ (n) n))
                 (λ (func)
                   (λ (n) n))))))
            lnat))
          (recur-fib
           ((λ (lnat)
              ((λ (np) (np (λ (in1 in2) in2)))
               ((lnat
                 (λ (np)
                   ((λ (input1 input2)
                      (λ (accessor) (accessor input1 input2)))
                    ((λ (lnat)
                       (λ (func)
                         (λ (input) (func ((lnat func) input)))))
                     ((λ (np)
                        (np (λ (in1 in2) in1))) np))
                    ((λ (np)
                       (np (λ (in1 in2) in1))) np))))
                ((λ (input1 input2)
                   (λ (accessor) (accessor input1 input2)))
                 (λ (func)
                   (λ (n) n))
                 (λ (func)
                   (λ (n) n))))))
            lnat))))))))
  (λ (func)
    (λ (n) (func (func (func (func (func (func n))))))))))


