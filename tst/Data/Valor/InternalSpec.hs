{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
--
module Data.Valor.InternalSpec where
--
import Prelude hiding ( fail )
--
import Test.Gen ( genWrong , genSmallInt , genSmallList , genSmallString , genFunction1 )
import Test.Hspec ( Spec , it , describe , shouldBe )
import Test.QuickCheck ( Gen , property )
import Data.Valor ( fail , validateP )
import Data.Valor.Internal ( Valid (..) , unValid , Wrong (..) , altW , accW , valW , wrong , isInert , isWrong )
--

spec :: Spec
spec = do
  validSpec
  wrongSpec
  valorSpec

validSpec :: Spec
validSpec = describe "Valid" $ do
  describe "unValid" $ do
    it "should retrieve a value 'e' from 'Valid e'" $ do
      unValid ( Valid True ) `shouldBe` True

wrongSpec :: Spec
wrongSpec = describe "Wrong" $ do
  describe "'Wrong' property tests" $ do
    describe "Semigroup instance for 'Wrong'" $ do
      it "Associativity: x <> (y <> z) ≡ (x <> y) <> z" $ property $
        semigroupAssociativity $ genWrong genSmallString

      it "Inert a <> Inert b ≡ Inert $ a <> b" $ property $
        wrongSemigroupResult genSmallString Inert Inert Inert

      it "Inert a <> Wrong b ≡ Wrong $ a <> b" $ property $
        wrongSemigroupResult genSmallString Inert Wrong Wrong

      it "Wrong a <> Inert b ≡ Wrong $ a <> b" $ property $
        wrongSemigroupResult genSmallString Wrong Inert Wrong

      it "Wrong a <> Wrong b ≡ Wrong $ a <> b" $ property $
        wrongSemigroupResult genSmallString Wrong Wrong Wrong

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

    describe "Applicative instance for 'Wrong'" $ do
      it "Identity: pure id <*> v ≡ v" $ property $
        applicativeIdentity $ genWrong genSmallString

      it "Homomorphism: pure f <*> pure x ≡ pure (f x)" $ property $
        applicativeHomomorphism @Wrong genFunction1 genSmallInt

      it "Interchange: u <*> pure y ≡ pure ($ y) <*> u" $ property $
        applicativeInterchange @Wrong genFunction1 genSmallInt

      it "Composition: pure (.) <*> u <*> v <*> w ≡ u <*> (v <*> w)" $ property $
        applicativeComposition @Wrong genFunction1 genFunction1 genSmallInt

      it "Inert f <*> Inert v ≡ Inert $ f v" $ property $
        wrongApplicativeResult genFunction1 genSmallInt Inert Inert Inert

      it "Inert f <*> Wrong v ≡ Wrong $ f v" $ property $
        wrongApplicativeResult genFunction1 genSmallInt Inert Wrong Wrong

      it "Wrong f <*> Inert v ≡ Wrong $ f v" $ property $
        wrongApplicativeResult genFunction1 genSmallInt Wrong Inert Wrong

      it "Wrong f <*> Wrong v ≡ Wrong $ f v" $ property $
        wrongApplicativeResult genFunction1 genSmallInt Wrong Wrong Wrong

  describe "'Wrong' utility functions" $ do
    describe "altW" $ do
      it "Inert a `altW` Inert b ≡ Inert a" $ property $
        altWResult genSmallString Inert Inert Inert fst

      it "Inert a `altW` Wrong b ≡ Inert a" $ property $
        altWResult genSmallString Inert Wrong Inert fst

      it "Wrong a `altW` Inert b ≡ Inert b" $ property $
        altWResult genSmallString Wrong Inert Inert snd

      it "Wrong a `altW` Wrong b ≡ Wrong b" $ property $
        altWResult genSmallString Wrong Wrong Wrong snd

    describe "accW" $ do
      it "Inert a `accW` Inert b ≡ Inert a" $ property $
        accWResult genSmallString Inert Inert Inert const

      it "Inert a `accW` Wrong b ≡ Inert a" $ property $
        accWResult genSmallString Inert Wrong Inert const

      it "Wrong a `accW` Inert b ≡ Inert b" $ property $
        accWResult genSmallString Wrong Inert Inert ( flip const )

      it "Wrong a `accW` Wrong b ≡ Wrong $ a <> b" $ property $
        accWResult genSmallString Wrong Wrong Wrong ( <> )

    describe "valW" $ do
      it "Should extract a raw value from 'Wrong'" $ property $ do
        w <- genWrong genSmallString
        pure $ valW w == case w of
          Inert v -> v
          Wrong v -> v

    describe "wrong" $ do
      it "Should apply appropriate function depending on the 'Wrong' constructor" $ property $ do
        f1 <- genFunction1
        f2 <- genFunction1
        w  <- genWrong genSmallInt
        r  <- pure $ wrong f1 f2 w
        pure $ case w of
          Inert v -> r == f2 v
          Wrong v -> r == f1 v

    describe "isInert" $ do
      it "Should return 'False' when 'Wrong'" $
        isInert ( Wrong @Int 0 ) `shouldBe` False

      it "Should return 'True' when 'Inert'" $
        isInert ( Inert @Int 0 ) `shouldBe` True

    describe "isWrong" $ do
      it "Should return 'True' when 'Wrong'" $
        isWrong ( Wrong @Int 0 ) `shouldBe` True

      it "Should return 'False' when 'Inert'" $
        isWrong ( Inert @Int 0 ) `shouldBe` False

valorSpec :: Spec
valorSpec = describe "Valor" $ do
  describe "'Valor' property tests" $ do
    describe "Monad instance for 'Valor'" $ do
      it "Left identity: return a >>= k ≡ k a" $ property $ \ s ->
        let
          i  = 0 :: Int
          v1 = return s >>= fail
          v2 = fail ( s :: String )
        in
          validateP v1 i `shouldBe` validateP v2 i

      it "Right identity: m >>= return ≡ m" $ property $ \ s ->
        let
          i = 0 :: Int
          m = fail ( s :: String )
          v = m >>= return
        in
          validateP v i `shouldBe` validateP m i

      it "Associativity: m >>= (\\x -> k x >>= h) ≡ (m >>= k) >>= h" $ property $ \ s ->
        let
          i  = 0 :: Int
          m  = fail ( s :: String )
          k  = fail
          h  = pure
          v1 = m >>= ( \ x -> k x >>= h )
          v2 = ( m >>= k) >>= h
        in
          validateP v1 i `shouldBe` validateP v2 i

--

altWResult :: Eq a => Gen a -> ( a -> Wrong a ) -> ( a -> Wrong a ) -> ( a -> Wrong a ) -> ( ( a , a ) -> a ) -> Gen Bool
altWResult gv c1 c2 c3 c = do
  a <- gv
  b <- gv
  pure $ ( c1 a `altW` c2 b ) == ( c3 $ c ( a , b ) )

accWResult :: ( Semigroup a , Eq a ) => Gen a -> ( a -> Wrong a ) -> ( a -> Wrong a ) -> ( a -> Wrong a ) -> ( a -> a -> a ) -> Gen Bool
accWResult gv c1 c2 c3 c = do
  a <- gv
  b <- gv
  pure $ ( c1 a `accW` c2 b ) == ( c3 $ c a b )

--

associativity :: Eq a => Gen a -> ( a -> a -> a ) -> Gen Bool
associativity gen op = do
  x <- gen
  y <- gen
  z <- gen
  pure $ x `op` ( y `op` z ) == ( x `op` y ) `op` z

--

semigroupAssociativity :: ( Eq a , Semigroup a ) => Gen a -> Gen Bool
semigroupAssociativity = flip associativity (<>)

wrongSemigroupResult :: ( Eq a , Semigroup a ) => Gen a -> ( a -> Wrong a ) -> ( a -> Wrong a ) -> ( a -> Wrong a ) -> Gen Bool
wrongSemigroupResult gv c1 c2 c3 = do
  a <- gv
  b <- gv
  pure $ ( c1 a <> c2 b ) == ( c3 $ a <> b )

--

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

--

functorIdentity :: ( Eq ( f v ) , Functor f ) => Gen ( f v ) -> Gen Bool
functorIdentity gen = do
  x <- gen
  pure $ fmap id x == id x

--

applicativeIdentity :: ( Eq ( f v ) , Applicative f ) => Gen ( f v ) -> Gen Bool
applicativeIdentity gen = do
  x <- gen
  pure $ ( pure id <*> x ) == x

applicativeHomomorphism
  :: forall f v b. ( Applicative f , Eq ( f b ) )
  => Gen ( v -> b )
  -> Gen v
  -> Gen Bool
applicativeHomomorphism fg vg = do
  f <- fg
  v <- vg
  pure $ ( pure @f f <*> pure v ) == ( pure $ f v )

applicativeInterchange
  :: forall f v b. ( Applicative f , Eq ( f b ) )
  => Gen ( v -> b )
  -> Gen v
  -> Gen Bool
applicativeInterchange fg vg = do
  f <- fg
  v <- vg
  pure $ ( pure @f f <*> pure v ) == ( pure ($ v) <*> pure f )

applicativeComposition
  :: forall f a b c. ( Applicative f , Eq ( f c ) )
  => Gen ( b -> c )
  -> Gen ( a -> b )
  -> Gen a
  -> Gen Bool
applicativeComposition fag fbg ag = do
  fb <- fag
  fa <- fbg
  a  <- ag
  pure $
    ( pure @f (.) <*> pure fb <*> pure fa <*> pure a )
    ==
    ( pure fb <*> ( pure fa <*> pure a ) )

wrongApplicativeResult
  :: Gen ( Int -> Int )
  -> Gen Int
  -> ( ( Int -> Int ) -> Wrong ( Int -> Int ) )
  -> ( Int -> Wrong Int )
  -> ( Int -> Wrong Int )
  -> Gen Bool
wrongApplicativeResult gf gv c1 c2 c3 = do
  f <- gf
  v <- gv
  pure $ ( c1 f <*> c2 v ) == ( c3 $ f v )
