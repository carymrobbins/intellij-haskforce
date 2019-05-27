{-# LANGUAGE NamedFieldPuns #-}
module Record00003 where

data Foo = Foo { bar :: Int, baz :: Char }

mkFoo0 :: Char -> Foo
mkFoo0 baz = Foo {bar, baz}
  where
  bar = ord baz

mkFoo1 :: Char -> Foo
mkFoo1 baz = Foo {bar = ord baz, baz}
