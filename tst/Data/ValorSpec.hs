{-# LANGUAGE TypeApplications #-}
--
module Data.ValorSpec where
--
import Prelude hiding ( fail )
--
import Data.Valor ( pass , fail , test , make , peek , poke , nerf , peer , adapt , check1 , checkN , validateP )
import Data.Valor.Internal ( Valid (..) , unValid , Wrong (..) , unValor )
--
import Data.Bool ( bool )
import Data.Functor.Identity ( Identity (..) )
import Data.Bifunctor ( bimap )
--
import Test.Gen ( genSmallInt , genSmallString , genPredicateInt , genSmallList )
import Test.Hspec ( Spec , it , describe , shouldBe )
import Test.QuickCheck ( property , elements )
--

newtype TestType a = TestType
  { unTestType :: a
  } deriving ( Eq , Show )

--

spec :: Spec
spec = do
  describe "Valor" $
    describe "'Valor' utility functions" $ do
      it "pass : should always pass" $ property $ \ i ->
        shouldBe
          ( validateP ( pass @Identity @String ) ( i :: String ) )
          ( Left $ Valid i )

      it "fail : should always fail" $ property $ \ e i ->
        shouldBe
          ( validateP ( fail ( e :: String ) ) ( i :: String ) )
          ( Right e )

      it "test : should choose one or the other depending on the predicate" $ property $ do
        i   <- genSmallInt
        e   <- genSmallString
        inv <- pure $ elements [ fail e , pass ]
        va  <- inv
        vb  <- inv
        p   <- genPredicateInt
        chosen <- pure $ bool va vb ( p i )
        t   <- pure $ test va vb ( pure . p )
        pure $ ( validateP t i ) == ( validateP chosen i )

      it "make : should fail with error 'e' if result is 'Maybe e'" $ property $ do
        i <- genSmallInt
        e <- genSmallString
        p <- genPredicateInt
        chosen <- pure $ bool ( fail e ) pass ( p i )
        t <- pure $ make ( pure . \ i' -> if p i' then Nothing else Just e )
        pure $ ( validateP t i ) == ( validateP chosen i )

      it "peek : should choose one or the other validator depending on result of a third" $ property $ do
        i  <- genSmallInt
        vm <- pure $ \ e -> elements [ pass , fail e ]
        v1 <- genSmallString >>= vm
        v2 <- genSmallString >>= vm
        v3 <- genSmallString >>= vm
        vr <- pure $ case validateP v3 i of
          Left _ -> v2
          Right _ -> v1
        pure $ ( validateP ( peek v1 v2 v3 ) i ) == ( validateP vr i )

      it "poke : should choose one or the other validator depending on result of a third, and aggregate the error" $ property $ do
        i  <- genSmallInt
        vm <- pure $ \ e -> elements [ pass , fail e ]
        v1 <- genSmallString >>= vm
        v2 <- genSmallString >>= vm
        v3 <- genSmallString >>= vm
        r <- pure $ case validateP v3 i of
          Left _ -> validateP v2 i
          Right e3' -> case validateP v1 i of
            Left i' -> Left i'
            Right e1' -> Right $ e3' <> e1'

        pure $ ( validateP ( poke v1 v2 v3 ) i ) == r

      it "nerf : should neutralize a failing validator" $ property $ do
        i <- genSmallInt
        e <- genSmallString
        pure $ ( validateP ( nerf $ fail e ) i ) == ( Left $ Valid i )

      it "peer : should convert neutral to Nothing and error to Maybe" $ property $ do
        i <- genSmallInt
        e <- genSmallString
        p <- genPredicateInt
        t <- pure $ peer $ test ( fail e ) pass ( pure . p )
        pure $ ( runIdentity $ unValor t i ) == bool ( Wrong $ Just e ) ( Inert $ Nothing ) ( p i )

      it "adapt : should make validator work with a different input type" $ property $ do
        i <- genSmallInt
        e <- genSmallString
        v <- elements [ fail e , pass ]
        pure $
          ( validateP ( adapt unTestType v ) ( TestType i ) )
          ==
          ( bimap ( Valid . TestType . unValid ) id $ validateP v i )

      it "check1 : should adapt validator to different input and result in a maybe error" $ property $ do
        i <- fmap TestType genSmallInt
        e <- genSmallString
        p <- genPredicateInt
        t <- pure $ check1 unTestType $ test ( fail e ) pass ( pure . p )
        pure $
          ( runIdentity $ unValor t i )
          ==
          ( bool ( Wrong $ Just e ) ( Inert $ Nothing ) ( p $ unTestType i ) )

      it "checkN : applies validator to a list of items and results in Just [ Maybe e ] if there's even a single error, Nothing otherwise" $ property $ do
        is <- genSmallList genSmallInt
        e  <- genSmallString
        p  <- genPredicateInt
        t  <- pure $ checkN unTestType $ test ( fail e ) pass ( pure . p )
        cs <- pure $ fmap p is
        es <- pure $ fmap ( bool ( Just e ) ( Nothing ) ) cs
        pure $
          ( runIdentity $ unValor t ( TestType is ) )
          ==
          ( bool ( Wrong $ Just es ) ( Inert $ Nothing ) ( and cs ) )
