#!/usr/bin/env janet

# Notes on proposed repeat:
# - `repeat-var n` puts a counter on the stack which is generally immutable.  n is usually a var.
# - `repeat` marks the start of a loop.
# - `repeat-counter` fetches the current counter value.
# - `repeat-end` decrements the counter, jumps to `repeat` marker on non-zero.

(defn parse-line
  [line]

  (def grammar
    ~{:main (* (+ :item) -1)

      :item (+ :var-decl
               :push :drop
               :dup :over
               :eq :lte :lt :gte :gt)

      # :array-get :array-set
      # :dup-from-top :swap :nip :tuck :rot
      # :and :or :not
      # :add :sub :mul :div :mod
      # :halt :jump :jump-if
      # :repeat https://github.com/essential-contributions/specs/pull/98
      # :alloc :free :truncate
      # :load :store

      :var-decl (group (* (constant :var) "var" :s+ :ident :literal))

      :push (group (* (constant :push) "push" :s+ (+ :num :ident)))
      :drop (* (constant :drop) "drop" :s*)

      :dup (* (constant :dup) "dup" :s*)
      :over (* (constant :over) "over" :s*)

      :eq (* (constant :eq) "eq" :s*)
      :lt (* (constant :lt) "lt" :s*)
      :lte (* (constant :lte) "lte" :s*)
      :gt (* (constant :gt) "gt" :s*)
      :gte (* (constant :gte) "gte" :s*)

      :ident (* (<- (* :a (any :w))) :s*)

      :literal (+ :num
                  :array)

      :num (* (number (some :d)) :s*)

      :array (* "[" :s* (replace (some :num) ,tuple) "]" :s*)})

  (let [matches (peg/match grammar (string/trim line))]
    (if (nil? matches)
      (error (string "parse failure: '" line "'"))
      (get matches 0))))

# Check stack won't underflow for operation.
(defn assert-stack
  [line-idx stack cmd-name min-size]

  (if (< (length stack) min-size)
    (error (string/format "line %d: %s: stack underflow" line-idx cmd-name))))

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

# drop: ( a -- )
(defn cmd-drop
  [line-idx stack]

  (assert-stack line-idx stack "drop" 1)

  (print "- dropping")
  (array/pop stack))

# dup: ( a -- a a)
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

# eq, lt, gt, lte, gte: ( a a -- p)
(defn cmd-eq [line-idx stack] (binary-op "eq" (comparison-op =) line-idx stack))
(defn cmd-lt [line-idx stack] (binary-op "lt" (comparison-op <) line-idx stack))
(defn cmd-gt [line-idx stack] (binary-op "gt" (comparison-op >) line-idx stack))
(defn cmd-lte [line-idx stack] (binary-op "lte" (comparison-op <=) line-idx stack))
(defn cmd-gte [line-idx stack] (binary-op "gte" (comparison-op >=) line-idx stack))

(defn main
  [& args]

  (when (<= (length args) 1)
    (print "use: " (get args 0) " FILE")
    (os/exit 0))

  (def vars @{})
  (def stack @[])

  (defn print-stack
    []

    (prin "  - stack:")
    (each el stack
      (prin " " el))
    (print))

  (var line-idx 1)

  # Read the input.
  (def src-buf (slurp (get args 1)))

  # Parse and interpret each line in turn.
  (loop [line :in (string/split "\n" src-buf)
         :after (+= line-idx 1)]

    (when (not (empty? line))
      (let [cmd (parse-line line)]
        #(pp cmd)
        (match cmd

          @[:var name val]
          (do
            (print (string/format "- initialising %s to %p" name val))
            (put vars name val))

          @[:push val] (cmd-push line-idx stack vars val)

          :drop (cmd-drop line-idx stack)
          :dup (cmd-dup line-idx stack)
          :over (cmd-over line-idx stack)

          :eq (cmd-eq line-idx stack)
          :lt (cmd-lt line-idx stack)
          :gt (cmd-gt line-idx stack)
          :lte (cmd-lte line-idx stack)
          :gte (cmd-gte line-idx stack)

          (error (string/format "line %d: unknown cmd: %p" line-idx cmd))))

      (print-stack))))
