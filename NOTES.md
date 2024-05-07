# Stack ops

push a  `( -- a )`
drop    `( a -- )`
pick    `( a b c 2 -- a b c a )`, `( a b c d 1 -- a b c d c )`
swap    `( a b -- b a )`

dup     `( a  -- a a )`                 same as `push 0` `pick`
over    `( a b -- a b a )`              same as `push 1 pick`
pick2   `( a b c 2 -- a b c a )`        same as `push 2` `pick`

rot     `( a b c -- b c a )`

-rot    `( a b c -- c a b )`    same as `rot` `rot`
2dup    `( a b -- a b a b )`    same as `over` `over`
nip     `( a b -- b )`          same as `swap` `drop`
tuck    `( a b -- b a b )`      same as `swap` `over`

# Rules

- For single var:
    - Assume var `a` is on top of stack.
    - If binary op:
        - If `a` is LHS then `dup`, push RHS, op, `swap`.
        - If `a` is RHS then `dup`, push LHS, `swap`, op, `swap`.
        - If `a` is neither then `-rot`, op, `swap`.
        - If `a` is both then `dup`, `dup`, op, `swap`.
    - If unary op:
        - If `a` is arg then `dup`, op, `swap`.
        - If `a` is not arg then `swap`, op, `swap`.
- For double var:
    - Assume vars `a` and `b` on top of stack.

# Single examples

_a must be at the top of the stack for every sub-expression_
_f and g represent a sub-expression compilation_

`(+ a f)`

f       # f a
tuck    # a f a
swap    # a a f
add     # a a+f
swap    # a+f a

`(+ f a)`

f       # f a
tuck    # a f a
add     # a f+a
swap    # f+a a

`(+ a a)`

dup     # a a
dup     # a a a
add     # a a+a
swap    # a+a a

`(+ f g)`

f       # f a
g       # f g a
-rot    # a f g
add     # a f+g
swap    # f+g a

# Double examples

_a and b must be at the top of the stack for every sub-expressoin_
_f represents a sub-expression compilation_

`(+ a b)`

2dup    # a b a b
add     # a b a+b
-rot    # a+b a b

`(+ b a)`

2dup    # a b a b
swap    # a b b a
add     # a b b+a
-rot    # b+a a b

`(+ a f)`

f       # f a b
rot     # a b f
pick2   # a b f a
swap    # a b a f
add     # a b a+f
-rot    # a+f a b

`(+ b f)`

f       # f a b
rot     # a b f
over    # a b f b
swap    # a b b f
add     # a b b+f
-rot    # b+f a b

`(+ f a)`

f       # f a b
rot     # a b f
pick2   # a b f a
add     # a b f+a
-rot    # f+a a b

`(+ f b)`

f       # f a b
rot     # a b f
over    # a b f b
add     # a b f+b
-rot    # f+b a b

`(+ a a)`

over    # a b a
dup     # a b a a
add     # a b a+a
-rot    # a+a a b

`(+ b b)`

dup     # a b b
dup     # a b b b
add     # a b b+b
-rot    # b+b a b

`(+ f g)`

f       # f a b
rot     # a b f
pick2   # a b f a
pick2   # a b f a b
g       # a b f g a b
drop    # a b f g a
drop    # a b f g
add     # a b f+g
-rot    # f+g a b

%% vim:foldlevel=2
