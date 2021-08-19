{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
--
module Data.ValorSpec where
--
import Data.Valor
import Data.Valor.Internal
--
import Data.Bool ( bool )
import Data.Char ( isNumber )
import Data.List.NonEmpty hiding ( length , filter )
--
import Test.Hspec ( Spec , describe , it , shouldBe )
import Test.Hspec.QuickCheck ( prop )
--

spec :: Spec
spec = undefined
  -- describe "passV" $ do
  --   prop "should return any value as 'Left' and 'Valid'" $ \v ->
  --     validateP (passV @_ @String @[String]) v `shouldBe` Left (Valid v)


--

newtype Age = Age
  { unAge :: Int
  } deriving ( Eq , Show )

ageV :: Monad m => Valor Age m [ String ]
ageV = check unAge $ over0 <> over18 <> under65

newtype Email = Email
  { unEmail :: String
  } deriving ( Eq , Show )

emailV :: Monad m => Valor Email m [ String ]
emailV = check unEmail $ nonempty <> hasAt

newtype Phone = Phone
  { unPhone :: String
  } deriving ( Eq , Show )

phoneV :: Monad m => Valor Phone m [ String ]
phoneV = check unPhone $ numeric

newtype UserName = UserName
  { unUserName :: String
  } deriving ( Eq , Show )

userNameV :: Monad m => Valor UserName m [ UserNameError ]
userNameV = check unUserName $ uneEmpty <> uneShort <> uneTaken
  where
    uneEmpty :: Monad m => Valor String m [ UserNameError ]
    uneEmpty = make $ \ i -> justT [ UNEEmpty ] $ null i

    uneShort :: Monad m => Valor String m [ UserNameError ]
    uneShort = make $ \ i -> justT [ UNEShort ] $ length i < 4

    uneTaken :: Monad m => Valor String m [ UserNameError ]
    uneTaken = make $ \ i -> justT [ UNETaken ] $ i `elem` [ "taken" ]

data UserNameError = UNEEmpty | UNEShort | UNETaken
  deriving ( Eq , Show )

newtype PassWord = PassWord
  { unPassWord :: String
  } deriving ( Eq , Show )

passWordV :: Monad m => Valor PassWord m [ PassWordError ]
passWordV = undefined

data PassWordError = PWEEmpty | PWEShort | PWENoNumbers | PWENoLetters
  deriving ( Eq , Show )

data Address = Address
  { name    :: String
  , street  :: String
  , number  :: String
  , city    :: String
  , country :: String
  } deriving ( Eq , Show )

data AddressReport = AddressReport
  { name    :: Maybe ( NonEmpty String )
  , street  :: Maybe ( NonEmpty String )
  , number  :: Maybe ( NonEmpty String )
  , city    :: Maybe ( NonEmpty String )
  , country :: Maybe ( NonEmpty String )
  } deriving ( Eq , Show )

data Credentials = Credentials
  { username :: UserName
  , password :: PassWord
  } deriving ( Eq , Show )

data CredentialsReport = CredentialsReport
  { username :: Maybe ( NonEmpty UserNameError )
  , password :: Maybe ( NonEmpty PassWordError )
  } deriving ( Eq , Show )

data UserForm = UserForm
  { age         :: Age
  , email       :: Email
  , phone       :: Maybe Phone
  , addresses   :: NonEmpty Address
  , credentials :: Credentials
  } deriving ( Eq , Show )

data UserFormReport = UserFormReport
  { age         :: Maybe ( NonEmpty String )
  , email       :: Maybe ( NonEmpty String )
  , phone       :: Maybe ( NonEmpty String )
  , addresses   :: Maybe ( NonEmpty AddressReport )
  , credentials :: Maybe CredentialsReport
  } deriving ( Eq , Show )

--

justT :: e -> Bool -> Maybe e
justT e = bool Nothing ( Just e )

--

over0 :: Monad m => Valor Int m [ String ]
over0 = make $ \ i -> justT [ "age must be greater than 0" ] $ i <= 0

over18 :: Monad m => Valor Int m [ String ]
over18 = make $ \ i -> justT [ "you must be over 18 years old" ] $ i < 18

under65 :: Monad m => Valor Int m [ String ]
under65 = make $ \ i -> justT [ "our service is not applicable for people over 65" ] $ i >= 65

hasAt :: Monad m => Valor String m [ String ]
hasAt = make $ \ i -> justT [ "invalid email address, missing @" ] $ length ( filter (=='@') i ) /= 1

nonempty :: Monad m => Valor String m [ String ]
nonempty = make $ \ i -> justT [ "this field cannot be empty" ] $ null i

numeric :: Monad m => Valor String m [ String ]
numeric = make $ \ i -> justT [ "only numbers are acceptable" ] $ not $ all isNumber i

exactLength :: Monad m => Int -> Valor String m [String]
exactLength l = make $ \ i -> justT [ "must be exactly " <> show l <> "characters long" ] $ length i /= l
