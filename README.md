# TypeBlocks
https://penteract.github.io/typeblocks/svg2/typeblocks.html

Types are shapes.
A function has the shape of its return type and holes for its arguments.
The shapes of those holes are the types of the arguments.

If the argument's type is itself a function type, then the hole needs an
antihole to match the hole of the expected argument. This antihole can be
thought of as an argument to a lambda.

[TODO: Add pictures]


# UI

Ctrl-drag to copy terms ; right click to delete

Dragging something into an appropriately shaped hole will make it fill the hole,
dragging it a little will make it un-snap and appear as a floating term within
the hole.

The inputs in the top left let you define a function by entering the name,
the type and clicking define. This function can be used by copying its LHS.

The "reduce" button in the top right does a single step of evaluation.
At present, this does lazy evaluation, so it should give the same result as an
equivalent Haskell program (ignoring the js-like behaviour of large Ints),
although Haskell itself would do much less copying of large terms.

# Theory
Writing a correctly typed program should be as straightforward as putting
blocks into correctly shaped holes, something
[children are capable of from a young age](https://duckduckgo.com/?q=child+putting+blocks+into+holes+shapes&iax=images&ia=images).


We can see one fun consequence of this visualisation if we consider the church
encoding of pairs (where a pair is identified with the combinator which takes a
function and applies it to both elements of the pair - `(x,y) === λf.f x y`),
the type of a pair `(A,B)` type is `forall a. (A->B->a)->a`. Using the
metaphors in typeblocks, this means the pair looks like a block of shape `A`
and a block of shape `B`, surrounded by a ring with inner and outer boundaries
corresponding to the type variable `a`. In a context where we're using the pair
rather than defining it, this shape should be flexible, with the caveat that
both occurrences of the shape (the inside and outside) must remain in sync,
leading to a boundary which could be viewed as a piece of string surrounding
the pair. Overall, this means a pair `(A,B)` looks like something of type `A`
and something of type `B` together in a container, which is exactly what a pair
is!

Unfortunately, the visualisation of the church encoding of sum types doesn't
quite so obviously reflect exectly what it is, `forall a. (A->a)->(B->a)->a`
has 2 holes, so it's the feeling that the boundary can be made thin is missing.


# Existing work
Tools like Scratch have a small set of types (boolean, 'number or text',
'action', 'event' ) and have carefully chosen shapes for each. From a Haskell
perspective, a block such as "next costume" has the type "action -> action"
(where "action" is roughly 'IO ()'), with the statements connected below being
seen as an argument (alternatively, you could see it as having the type
"action" and view the connections as something like `do` notation; however
there are blocks such as "delete this clone" which do not connect to blocks
below and terminate the running script, leading me to prefer the
"action->action" characterisation). Scratch does not support higher-order
types.

[Snap!](https://snap.berkeley.edu/) and
[Blockly](https://sites.google.com/view/2023blocklysummit/home) do support
higer-order functions.
Snap! also makes it easy to have lambdas as arguments to second order
functions, although requires the arguments to be named. I understand its
semantics are based on scheme, so it doesn't really want strong typing.

Blockly is really designed to be a library which other projects can build on
and has some features to support strong types but doesn't really treat them as
central. I don't know how much people have used it with higher order
functions.


