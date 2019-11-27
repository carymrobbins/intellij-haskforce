module BlockArguments00001 where

spec :: Spec
spec = do
  describe "BlockArguments" do
    it "is parsed in HaskForce" $ test \Resources { .. } -> do
      thing `shouldReturn` []
      other >>= expectPatternM \[Obj { foo, bar }] -> do
        foo `shouldBe` 1
        bar `shouldBe` 'b'
