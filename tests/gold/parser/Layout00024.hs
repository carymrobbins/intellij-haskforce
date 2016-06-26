{-# LANGUAGE RecursiveDo #-}
module Layout00024 where

-- https://downloads.haskell.org/~ghc/7.2.2/docs/html/users_guide/syntax-extns.html
justOnes = do { rec { xs <- Just (1:xs) }
              ; return (map negate xs) }

-- Indentation-based variant of above.
justOnes' = do
  rec
    xs <- Just (1:xs)
  return (map negate xs)

-- https://github.com/carymrobbins/intellij-haskforce/issues/264
grammar :: forall r . Grammar r (Prod r String String Sentence)
grammar = do
  rec
    nounPhrase <- rule $  A_NP <$> adjective  <*> nounPhrase
                      <|> N    <$> noun
                      <|> D_N  <$> determiner <*> noun
                      <|> A_N  <$> adjective  <*> noun
                      <?> "noun phrase"
  let prepPhrase :: Prod r String String PrepPhrase
      prepPhrase = P_NP <$> preposition   <*> nounPhrase <?> "prepositional phrase"
  rec
    verbPhrase <- rule $  V_NP  <$> verb       <*> nounPhrase
                      <|> VP_PP <$> verbPhrase <*> prepPhrase
                      <|> V     <$> verb
                      <?> "verb phrase"
