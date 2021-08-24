{-# LANGUAGE TypeApplications #-}
--
module Data.Valor.InternalSpec where
--
import Test.Gen ( genWrong , genSmallInt , genSmallList , genSmallString )
import Test.Hspec ( Spec , it , describe , shouldBe )
import Test.QuickCheck ( Gen , Property , property )
import Data.Valor.Internal ( Valid (..) , unValid , Wrong (..) )
--

spec :: Spec
spec = do
  describe "Valid" $ do
    describe "unValid" $ do
      it "should retrieve a value 'e' from 'Valid e'" $ do
        unValid ( Valid True ) `shouldBe` True

  describe "Wrong" $ do
    describe "'Wrong' property tests" $ do
      describe "Semigroup instance for 'Wrong'" $ do
        it "Associativity: x <> (y <> z) ≡ (x <> y) <> z" $ property $
          semigroupAssociativity $ genWrong genSmallString

      describe "Monoid instance for 'Wrong'" $ do
        it "Left identity: mempty <> x ≡ x" $ property $
          monoidLeftIdentity $ genWrong genSmallString

        it "Right identity: x <> mempty ≡ x" $ property $
          monoidRightIdentity $ genWrong genSmallString

        it "Concatenation: mconcat ≡ foldr (<>) mempty" $ property $
          monoidConcatenation $ genWrong genSmallString

      describe "Functor instance for 'Wrong'" $ do
        it "Identity: fmap id ≡ id" $ property $
          functorIdentity $ genWrong genSmallString

--

associativity :: Eq a => Gen a -> ( a -> a -> a ) -> Gen Bool
associativity gen op = do
  x <- gen
  y <- gen
  z <- gen
  pure $ x `op` ( y `op` z ) == ( x `op` y ) `op` z

semigroupAssociativity :: ( Eq a , Semigroup a ) => Gen a -> Gen Bool
semigroupAssociativity = flip associativity (<>)

monoidLeftIdentity :: ( Eq a , Monoid a ) => Gen a -> Gen Bool
monoidLeftIdentity gen = do
  x <- gen
  pure $ ( mempty <> x ) == x

monoidRightIdentity :: ( Eq a , Monoid a ) => Gen a -> Gen Bool
monoidRightIdentity gen = do
  x <- gen
  pure $ ( x <> mempty ) == x

monoidConcatenation :: ( Eq a , Monoid a ) => Gen a -> Gen Bool
monoidConcatenation gen = do
  xs <- genSmallList gen
  pure $ ( mconcat xs ) == ( foldr (<>) mempty xs )

functorIdentity :: ( Eq ( f v ) , Functor f ) => Gen ( f v ) -> Gen Bool
functorIdentity gen = do
  x <- gen
  pure $ fmap id x == id x
