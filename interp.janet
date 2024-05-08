#!/usr/bin/env janet

(defn parse-line
  [line-idx line]

  (def grammar
    ~{:main (* (+ :item) -1)

      :item (+ :var-decl
               :push :len :get :cmp
               :rep :idx :end
               :drop :dup :over :swap :rot :pick
               :eq :lte :lt :gte :gt
               :and :or :not
               :add :sub :mul :div :mod
               :asrt)

      :var-decl (group (* (constant :var) "var" :s+ :ident :literal))
      :push (group (* (constant :push) "push" :s+ (+ :num :ident)))
      :len (group (* (constant :len) "len" :s+ :ident))
      :get (group (* (constant :get) "get" :s+ :ident))
      :cmp (group (* (constant :cmp) "cmp" :s+ :ident))

      :rep (* (constant :rep) "rep" :s*)
      :idx (* (constant :idx) "idx" :s*)
      :end (* (constant :end) "end" :s*)

      :drop (* (constant :drop) "drop" :s*)
      :dup (* (constant :dup) "dup" :s*)
      :over (* (constant :over) "over" :s*)
      :swap (* (constant :swap) "swap" :s*)
      :rot (* (constant :rot) "rot" :s*)
      :pick (* (constant :pick) "pick" :s*)

      :eq (* (constant :eq) "eq" :s*)
      :lt (* (constant :lt) "lt" :s*)
      :lte (* (constant :lte) "lte" :s*)
      :gt (* (constant :gt) "gt" :s*)
      :gte (* (constant :gte) "gte" :s*)

      :and (* (constant :and) "and" :s*)
      :or (* (constant :or) "or" :s*)
      :not (* (constant :not) "not" :s*)

      :add (* (constant :add) "add" :s*)
      :sub (* (constant :sub) "sub" :s*)
      :mul (* (constant :mul) "mul" :s*)
      :div (* (constant :div) "div" :s*)
      :mod (* (constant :mod) "mod" :s*)

      :asrt (* (constant :asrt) "asrt" :s*)

      :ident (* (<- (* :a (any :w))) :s*)

      :literal (+ :num
                  :array)

      :num (* (number (some :d)) :s*)

      :array (* "[" :s* (replace (some :num) ,tuple) "]" :s*)})

  (let [matches (peg/match grammar (string/trim line))]
    (if (nil? matches)
      (error (string/format "line: %d: parse failure: '%s'" line-idx line))
      (get matches 0))))

# Check stack won't underflow for operation.
(defn assert-stack
  [line-idx stack cmd-name min-size]

  (if (< (length stack) min-size)
    (error (string/format "line %d: %s: stack underflow" line-idx cmd-name))))

# var a b: ( -- )
(defn cmd-var
  [vars name val]

  (print (string/format "- initialising %s to %p" name val))
  (put vars name val))

# push a: ( -- a )
(defn cmd-push
  [line-idx stack vars val]

  (case (type val)
    :number
    (do
      (print (string/format "- pushing number %d" val))
      (array/push stack val))

    :string
    (let [v (get vars val)]
      (if (nil? v)
        (error (string/format "line %d: unknown variable '%s'" line-idx val)))

      (print (string/format "- pushing variable %s" val))
      (case (type v)
        :number
        (array/push stack v)

        :tuple
        (array/concat stack (reverse v))

        (error (string/format "line %d: unhandled push variable type: %p" line-idx (type v)))))

    (error (string/format "line %d: unhandled 'push' value type: %p" line-idx (type val)))))

# len a: ( -- n )
(defn cmd-len
  [line-idx stack vars ary]

  (def v (get vars ary))

  (if (nil? v)
    (error (string/format "line %d: unknown variable '%s'" line-idx ary)))

  (if (not= :tuple (type v))
    (error (string/format "line %d: 'len' must be for array variables: found '%p'" line-idx ary)))

  (print (string/format "- pushing array '%s' length: %d" ary (length v)))
  (array/push stack (length v)))

# get a: ( i -- x )
(defn cmd-get
  [line-idx stack vars ary]

  (assert-stack line-idx stack "get" 1)

  (def v (get vars ary))

  (if (nil? v)
    (error (string/format "line %d: unknown variable '%s'" line-idx ary)))

  (if (not= :tuple (type v))
    (error (string/format "line %d: 'get' must be for array variables: found '%p'" line-idx ary)))

  (def i (array/pop stack))

  (if (>= i (length v))
    (error (string/format
             "line %d: 'get' out of bounds for array '%s' with index %d" line-idx ary i)))

  (def x (get v i))

  (print (string/format "- pushing array '%s' index %d value %d" ary i x))
  (array/push stack x))

# cmp a: ( a b.. -- p )
(defn cmd-cmp
  [line-idx stack vars ary]

  (def v (get vars ary))

  (if (nil? v)
    (error (string/format "line %d: unknown variable '%s'" line-idx ary)))

  (if (not= :tuple (type v))
    (error (string/format "line %d: 'cmp' must be for array variables: found '%p'" line-idx ary)))

  (def cnt (length v))

  (assert-stack line-idx stack "cmp" cnt)

  (var result 1)
  (each v-el (reverse v)
    (if (not= (array/pop stack) v-el)
      (set result 0)))

  (print (string/format "- comparing %d elements of %s with stack: pushing %d" cnt ary result))
  (array/push stack result))

# rep: ( c -- )
(defn cmd-rep
  [line-idx stack ctrl-stack addr]

  (assert-stack line-idx stack "rep" 1)

  (print "- begin rep loop")
  (array/push ctrl-stack [(inc addr) 0 (array/pop stack)]))

# idx: ( -- i )
(defn cmd-idx
  [line-idx stack ctrl-stack]

  (if (empty? ctrl-stack)
    (error (string/format "line %d: idx: control stack underflow" line-idx)))

  (def idx (get (array/peek ctrl-stack) 1))

  (print (string/format "- push loop index: %d" idx))
  (array/push stack idx))

# end: ( -- )
(defn cmd-end
  "Control flow: return new instr-idx"
  [line-idx ctrl-stack instr-idx]

  (if (empty? ctrl-stack)
    (error (string/format "line %d: end: control stack underflow" line-idx)))

  (def [addr idx cnt]
    (array/pop ctrl-stack))
  (def idx (inc idx))

  (if (< idx cnt)
    # Loop again; jump to addr and put the incremented idx back on the control stack.
    (do
      (print (string/format "- index+1 %d is less than %d, loop back to %d" idx cnt addr))
      (array/push ctrl-stack [addr idx cnt])
      addr)

    # End of loop; move to next instruction.
    (do
      (print (string/format "- index+1 %d equals %d, end loop" idx cnt))
      (inc instr-idx))))

# drop: ( a -- )
(defn cmd-drop
  [line-idx stack]

  (assert-stack line-idx stack "drop" 1)

  (print "- dropping")
  (array/pop stack))

# dup: ( a -- a a )
(defn cmd-dup
  [line-idx stack]

  (assert-stack line-idx stack "dup" 1)

  (print "- duping")
  (array/push stack (last stack)))

# over: ( a b -- a b a )
(defn cmd-over
  [line-idx stack]

  (assert-stack line-idx stack "over" 2)

  (print "- overing")
  (array/push stack (get stack (- (length stack) 2))))

# swap: ( a b -- b a )
(defn cmd-swap
  [line-idx stack]

  (assert-stack line-idx stack "swap" 2)

  (print "- swapping")
  (def b (array/pop stack))
  (def a (array/pop stack))
  (array/push stack b)
  (array/push stack a))

# rot: ( a b c -- b c a)
(defn cmd-rot
  [line-idx stack]

  (assert-stack line-idx stack "rot" 3)

  (print "- rotating")
  (def c (array/pop stack))
  (def b (array/pop stack))
  (def a (array/pop stack))
  (array/push stack b)
  (array/push stack c)
  (array/push stack a))

# pick: ( ... n -- ... a )
(defn cmd-pick
  [line-idx stack]

  (assert-stack line-idx stack "pick" 1)

  (def idx (array/pop stack))
  (assert-stack line-idx stack "pick (idx)" (inc idx))

  (print (string/format "- pick %d" idx))
  (array/push stack (get stack (- (length stack) 1 idx))))

# Binary operator wrapper.
(defn binary-op
  [name cmd line-idx stack]

  (assert-stack line-idx stack name 2)

  (print (string/format "- %s" name))
  (let [rhs (array/pop stack)
        lhs (array/pop stack)]
    (array/push stack (cmd lhs rhs))))

# Convert boolean comparison op to return 1 or 0 instead of true or false.
(defmacro comparison-op
  [op]

  (with-syms [$lhs $rhs]
    ~(fn (,$lhs ,$rhs) (if (,op ,$lhs ,$rhs) 1 0))))

# eq, lt, gt, lte, gte: ( a a -- p )
(defn cmd-eq [line-idx stack] (binary-op "eq" (comparison-op =) line-idx stack))
(defn cmd-lt [line-idx stack] (binary-op "lt" (comparison-op <) line-idx stack))
(defn cmd-gt [line-idx stack] (binary-op "gt" (comparison-op >) line-idx stack))
(defn cmd-lte [line-idx stack] (binary-op "lte" (comparison-op <=) line-idx stack))
(defn cmd-gte [line-idx stack] (binary-op "gte" (comparison-op >=) line-idx stack))

# Convert boolean op to work on numbers.  In Janet zero is truthy.
(defmacro boolean-op
  [op]

  (with-syms [$lhs $rhs]
    ~(fn (,$lhs ,$rhs) (if (,op (not (zero? ,$lhs)) (not (zero? ,$rhs))) 1 0))))

# and, or: ( p p -- p )
(defn cmd-and [line-idx stack] (binary-op "and" (boolean-op and) line-idx stack))
(defn cmd-or [line-idx stack] (binary-op "or" (boolean-op or) line-idx stack))

# not: ( p -- p )
(defn cmd-not [line-idx stack]

  (assert-stack line-idx stack "not" 1)

  (print "- not")
  (array/push stack (if (zero? (array/pop stack)) 1 0)))

# add, sub, mul, div, mod: ( a a -- a )
(defn cmd-add [line-idx stack] (binary-op "add" + line-idx stack))
(defn cmd-sub [line-idx stack] (binary-op "sub" - line-idx stack))
(defn cmd-mul [line-idx stack] (binary-op "mul" * line-idx stack))
(defn cmd-div [line-idx stack] (binary-op "div" div line-idx stack))
(defn cmd-mod [line-idx stack] (binary-op "mod" mod line-idx stack))

# asrt: ( p -- )
(defn cmd-asrt
  [line-idx stack]

  (assert-stack line-idx stack "asrt" 1)

  (def val (array/pop stack))

  (print (string/format "- assert (%p)" val))

  (when (zero? val)
    (print "ASSERTION FAILED")
    (os/exit 1)))

(defn main
  [& args]

  (when (<= (length args) 1)
    (print "use: " (get args 0) " FILE")
    (os/exit 0))

  (def vars @{})
  (def stack @[])
  (def ctrl-stack @[])

  (defn print-stack
    []

    (prin "    - stack:")
    (each el stack
      (prin " " el))
    (print))

  (defn print-ctrl-stack
    []

    (when (not (empty? ctrl-stack))
      (prin "    - control stack:")
      (each [addr idx cnt] ctrl-stack
        (prin (string/format " @%d,%d/%dx" addr idx cnt)))
      (print)))

  # Read the input.
  (def src-buf (slurp (get args 1)))

  # Parse each line into an array.
  (var line-idx 1)
  (def program
    (seq [line :in (string/split "\n" src-buf)
          :when (not (empty? line))
          :after (++ line-idx)]
      (parse-line line-idx line)))

  # Interpret each line.
  (var instr-idx 0)
  (while (< instr-idx (length program))
    (let [cmd (get program instr-idx)]
      (prin (string/format "%3d " instr-idx))

      (match cmd
        # Control flow ops first.
        :end (set instr-idx (cmd-end line-idx ctrl-stack instr-idx))

        # The rest, which will just post-increment the instr-idx.
        (do
          (match cmd

            @[:var name val] (cmd-var vars name val)
            @[:push val] (cmd-push line-idx stack vars val)
            @[:len ary] (cmd-len line-idx stack vars ary)
            @[:get ary] (cmd-get line-idx stack vars ary)
            @[:cmp ary] (cmd-cmp line-idx stack vars ary)

            :rep (cmd-rep line-idx stack ctrl-stack instr-idx)
            :idx (cmd-idx line-idx stack ctrl-stack)

            :drop (cmd-drop line-idx stack)
            :dup (cmd-dup line-idx stack)
            :over (cmd-over line-idx stack)
            :swap (cmd-swap line-idx stack)
            :rot (cmd-rot line-idx stack)
            :pick (cmd-pick line-idx stack)

            :eq (cmd-eq line-idx stack)
            :lt (cmd-lt line-idx stack)
            :gt (cmd-gt line-idx stack)
            :lte (cmd-lte line-idx stack)
            :gte (cmd-gte line-idx stack)

            :add (cmd-add line-idx stack)
            :sub (cmd-sub line-idx stack)
            :mul (cmd-mul line-idx stack)
            :div (cmd-div line-idx stack)
            :mod (cmd-mod line-idx stack)

            :and (cmd-and line-idx stack)
            :or (cmd-or line-idx stack)
            :not (cmd-not line-idx stack)

            :asrt (cmd-asrt line-idx stack)

            (error (string/format "line %d: unknown cmd: %p" line-idx cmd)))

          (++ instr-idx)))

      (print-stack)
      (print-ctrl-stack))))
