{-# LANGUAGE TypeApplications #-}
--
module Data.ValorSpec where
--
import Prelude hiding ( fail )
--
import Data.Valor ( Valor , pass , fail , test , make , peek , poke , nerf , peer , adapt , check1 , checkN , validateP , failIf , passIf )
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

--

data State = State
  { teams     :: [ String ]
  , countries :: [ String ]
  } deriving ( Eq , Show )

--

newtype Age = Age
  { unAge :: Int
  } deriving ( Eq , Show )

data AgeError = AgeUnder | AgeOver
  deriving ( Eq , Show )

ageV :: Monad m => Valor Age m [ AgeError ]
ageV = adapt unAge $ passIf [ AgeUnder ] (>18) <> failIf [ AgeOver ] (>65)

--

newtype Team = Team
  { unTeam :: String
  } deriving ( Eq , Show )

data TeamError = TeamEmpty | TeamShort | TeamLong | TeamTaken
  deriving ( Eq , Show )

teamV :: Valor Team ( (->) State ) [ TeamError ]
teamV = adapt unTeam $ mconcat
  [ failIf [ TeamEmpty ] null
  , passIf [ TeamShort ] ( (>3) . length )
  , failIf [ TeamLong  ] ( (>50) . length )
  , make $ \ i -> do
      ts <- teams
      pure $ if i `elem` ts
        then Just [ TeamTaken ]
        else Nothing
  ]

exTeamValid :: Team
exTeamValid = Team "Team Haskell"

exTeamEmpty :: Team
exTeamEmpty = Team ""

exTeamShort :: Team
exTeamShort = Team "srt"

exTeamLong :: Team
exTeamLong = Team "Super Mega Awesome Team Rocket Pocket Locket Wow Still Going"

exTeamTaken :: Team
exTeamTaken = Team "Taken"

--

newtype Email = Email
  { unEmail :: String
  } deriving ( Eq , Show )

data EmailError = EmailEmpty | EmailNoAt | EmailNoDot
  deriving ( Eq , Show )

emailV :: Monad m => Valor Email m [ EmailError ]
emailV = adapt unEmail $ mconcat
  [ failIf [ EmailEmpty ] null
  , passIf [ EmailNoAt ] ( any (=='@') )
  , passIf [ EmailNoDot ] ( any (=='.') )
  ]

exEmailValid :: Email
exEmailValid = Email "valid@email.com"

exEmailEmpty :: Email
exEmailEmpty = Email ""

exEmailNoAt :: Email
exEmailNoAt = Email "invalidemail.com"

exEmailNoDot :: Email
exEmailNoDot = Email "invalid@emailcom"

--

newtype Country = Country
  { unCountry :: String
  } deriving ( Eq , Show )

data CountryError = CountryEmpty | CountryNotAllowed
  deriving ( Eq , Show )

countryV :: Valor Country ( (->) State ) [ CountryError ]
countryV = adapt unCountry $ mconcat
  [ failIf [ CountryEmpty ] null
  , make $ \ i -> do
      cs <- countries
      pure $ if i `elem` cs
        then Nothing
        else Just [ CountryNotAllowed ]
  ]

exCountryValid :: Country
exCountryValid = Country "Croatia"

exCountryEmpty :: Country
exCountryEmpty = Country ""

exCountryNotAllowed :: Country
exCountryNotAllowed = Country "Europe"

--

data Participant = Participant
  { age     :: Age
  , name    :: String
  , surname :: String
  , email   :: Email
  } deriving ( Eq , Show )

data ParticipantError = ParticipantError
  { ageE      :: Maybe [ AgeError ]
  , nameE     :: Maybe [ String ]
  , surnameE  :: Maybe [ String ]
  , emailE    :: Maybe [ EmailError ]
  } deriving ( Eq , Show )

participantV :: Monad m => Valor Participant m ParticipantError
participantV = ParticipantError
  <$> check1 age ageV
  <*> check1 name ( failIf [ "name can't be empty" ] null )
  <*> check1 surname ( failIf [ "surname can't be empty"] null )
  <*> check1 email emailV

exParticipantValid1 :: Participant
exParticipantValid1 = Participant
  { age = Age 30
  , name = "Pero"
  , surname = "Perić"
  , email = Email "pero.peric@email.com"
  }

exParticipantValid2 :: Participant
exParticipantValid2 = Participant
  { age = Age 51
  , name = "Marko"
  , surname = "Marić"
  , email = Email "marko.maric@email.com"
  }

exParticipantValid3 :: Participant
exParticipantValid3 = Participant
  { age = Age 29
  , name = "Jane"
  , surname = "Doe"
  , email = Email "jane.doe@email.com"
  }

exParticipantInvalid1 :: Participant
exParticipantInvalid1 = Participant
  { age = Age 48
  , name = ""
  , surname = "Perić"
  , email = Email "peropericemailcom"
  }

exParticipantInvalid2 :: Participant
exParticipantInvalid2 = Participant
  { age = Age 73
  , name = "John"
  , surname = "Doe"
  , email = Email "john.doe@mail.com"
  }

exParticipantInvalid3 :: Participant
exParticipantInvalid3 = Participant
  { age = Age 17
  , name = "Mini"
  , surname = "Morris"
  , email = Email ""
  }

--

data Application = Application
  { team    :: Team
  , country :: Country
  , captain :: Participant
  , members :: [ Participant ]
  } deriving ( Eq , Show )

data ApplicationError = ApplicationError
  { teamE     :: Maybe [ TeamError ]
  , countryE  :: Maybe [ CountryError ]
  , captainE  :: Maybe ParticipantError
  , membersE  :: Maybe [ Maybe ParticipantError ]
  } deriving ( Eq , Show )

applicationV :: Valor Application ( (->) State ) ApplicationError
applicationV = ApplicationError
  <$> check1 team teamV
  <*> check1 country countryV
  <*> check1 captain participantV
  <*> checkN members participantV

exApplicationValid :: Application
exApplicationValid = Application
  { team = Team "Valor"
  , country = Country "Croatia"
  , captain = exParticipantValid1
  , members = [ exParticipantValid2 , exParticipantValid3 ]
  }

exApplicationInvalid1 :: Application
exApplicationInvalid1 = Application
  { team = Team "Taken"
  , country = Country ""
  , captain = exParticipantValid1
  , members = [ exParticipantInvalid1 , exParticipantValid3 ]
  }

exApplicationInvalid2 :: Application
exApplicationInvalid2 = Application
  { team = Team "srt"
  , country = Country "Murica!"
  , captain = exParticipantInvalid1
  , members = [ exParticipantInvalid2 , exParticipantValid1 , exParticipantValid3 , exParticipantValid2 ]
  }

state :: State
state = State
  { teams = [ "Taken" ]
  , countries = [ "Croatia" , "Germany" , "USA" , "Japan" ]
  }
