# Algebraic effects from monads and algebras

Given a monad `'a t`, an algebra is a type `a` with a map `a t -> a`.
	
For example, if `t` is the list monad, from a map `f : a list -> a` we can obtain a monoid
```ocaml
let id : a = f []
let mul : a -> a -> a = 
	fun x y -> f [x; y]
```
such that `id` is the identity and `mul` is associative.

More examples in the file [monads.ml](monads.ml).
		
Suppose we are given a monad `'a t` and an algebra `f : a t -> a`, then
the corresponding effect for this algebra is of the form
```
eff_f : i -> o
```
such that the type of its handler
```
i -> (o -> a) -> a
```
is isomorphic to the type of algebras `a t -> a`.
In particular this means that the algebra `f` is a handler for `eff_f`.

For example, given a monoid `(a,id,mul)` as above, we define
```
Id : unit -> empty
Mul : unit -> bool
```
the corresponding handler has type
```
Id () : (empty -> a) -> a
Mul () : (bool -> a) -> a
```
so we can define the handler
```
Id () k => id
Mul () k => (mul (k true) (k false))
```
which corresponds to nondeterministic execution.

More examples:
 - [ideal_effects.ml](ideal_effects.ml) (idealized syntax)
 - [state.ml](state.ml)
 - [nondeterminism.ml](nondeterminism.ml)
