module Type00007 where

type Foo f a = (Functor f, Traversable f) => f a

type Bar f = forall x. f x

type Baz f = forall x. (Functor f, Traversable f) => f x
