module Foo where

-- Ensure we don't catch this foo as the declaration.
foo <~> bar = "baz"

<resolved>foo [] = putStrLn "oh no!"
foo xs = putStrLn xs
