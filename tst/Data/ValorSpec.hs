module Data.ValorSpec where
--
import Test.Hspec ( Spec , it , describe , shouldBe )
--

spec :: Spec
spec = do
  describe "Something" $
    it "is a dummy test" $
      True `shouldBe` True

