module ValorSpec where
--
import Test.Hspec
import Test.Hspec.Checkers ( testBatch )
import Test.QuickCheck
import Test.QuickCheck.Classes ( semigroup )

import Valor
import Data.Text ( Text )
--

spec :: Spec
spec = do
  describe "identity" $ do
    it "returns what it was given" $
      id 1 `shouldBe` 1
