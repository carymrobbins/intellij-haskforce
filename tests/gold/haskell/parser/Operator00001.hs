module Operator00001 where

renderNode (s, a) = text (label a) # bold # font "sans-serif"
