module Layout00016 where

decodeLen e
    | [p@(_,_)] <- reads e :: [(Int,String)]
    = Just p

