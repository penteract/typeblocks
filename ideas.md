

Which programming language to use?

 - javascript:
  currently written in js;
  browser layout engine is an incredibly good cross platform GUI library;
  imperative/mutable style is very useful for efficient live manipulations
 - Haskell:
  Type system is based on Haskell; Could become self-hosting
  Laziness isn't actually a great idea and makes some evaluation steps confusing
 - Erlang/gleam:
  1 process per box is a nice idea
  Imperative enough that the ideas used in the js version would work
  Functional enough that the model typeblocks advocates would make sense in the context of erlang
  Cons:
   I don't actually know erlang or gleam
   ex11 is not a fully developed GUI library
   To make it self-hosting, I'd need a type system for Erlang, and any decent one that can deal with PIDs properly would have to support subtyping, which is hard to represent.
   

   

Combinable operators:

Infix +, -, *, /

In general a chain of same-precedence infixl operators with shape ((... -> a) -> b -> a)
or infixr operators with shape (b -> (... -> a) -> a).
Note that generic ^ and ^^ have the wrong shape/fixity

   
Drawing Monads:
`let` is to `|>` as `do` is to `>>=`
More precisely, in `do` notation `=` is to `|>` as `<-` is to `>>=`

The notation works most sensibly with any function with type signature isomorphic to  `∀a. T a -> (a -> B) -> C` for any values of `T`, `B` and `C` on the condition that `C` does not contain `a` in a contravariant(hole-like) position (i.e. `C` is not `a->d` or `((a->d)->e)->f`). Under those conditions, the nature of parametric polymorphism means that any `a` passed to the second argument must have come from the first argument.

`|>`, `>>=`, `flip map` and `flip uncurry` all have that sort of type.
