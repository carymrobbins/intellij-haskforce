module Import00002 where

import qualified Prelude hiding (zipWith, head)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Maybe (Maybe(..))
import qualified Prelude as P

data Maybe2 a = Just2 a | Nothing2

dropBitMask :: B.ByteString -> B.ByteString
dropBitMask xs  = go (B.breakSubstring "here" xs) []
  where comma = B.pack ","
        rparen = B.pack ")"
        go (h, t) acc
          | B.null t = B.append (B.concat $ reverse acc) h
          | otherwise = go (B.breakSubstring wrd rest) (rparen:as:wrd:h:acc)
             where (as, rest) = fixArg (B.span (/= ';') $ B.drop (B.length wrd) t)

