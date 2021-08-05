{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
--
module Data.ValorSpec where
--
import Data.Valor
import Data.Valor.Internal
--
import Data.Bool (bool)
import Data.List (intercalate)
import Data.Functor.Identity (Identity (..))
--
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
--

spec :: Spec
spec = do
  describe "passV" $ do
    prop "should return any value as 'Left' and 'Valid'" $ \v ->
      validateP (passV @_ @String @[String]) v `shouldBe` Left (Valid v)

  describe "failV" $ do
    prop "should return the error as 'Right' for any value" $ \v e ->
      validateP (failV @_ @String @[String] [e]) v `shouldBe` Right [e]

  describe "makeV" $ do
    prop "should fail if string length is out of bounds" $ \(v::String) (n::Int) (d::Int) ->
      let
        lo = abs n
        hi = lo + (abs d)
        ln = length v
        er = "not between " <> show lo <> " and " <> show hi <> ": "
        ad = \l -> intercalate "<" $ fmap show [lo, l, hi]
        pr = \v' ->
          let
            l = length v'
          in
            if
              l >= lo && l <= hi
            then
              Nothing
            else
              Just [er <> ad l]
      in
        validateP (makeV ( pure . pr )) v `shouldBe` maybe (Left $ Valid v) Right (pr v)

  describe "testV" $ do
    prop "shoud fail or succeed depending on the predicate" $ \(v::String) (e::String) (p::Bool) ->
      let
        o = bool (Left $ Valid v) (Right [e]) p
        t = testV passV (failV [e])
      in
        validateP (t $ pure . const p) v `shouldBe` o

  describe "caseV" $ do
    prop "should test one or the other validator based on the status of the first" $ \(v::String) (b1::Bool) (b2::Bool) ->
      let
        t1 = bool (failV ["error 1"]) passV b1
        t2 = bool (failV ["error 2"]) passV b2
        t3 = bool (failV ["error 3"]) passV b2
      in
        validateP (caseV t2 t3 t1) v `shouldBe` case (b1, b2) of
          (_, True) -> Left $ Valid v
          (True, False) -> Right $ ["error 3"]
          (False, False) -> Right $ ["error 2"]

  describe "scanV" $ do
    prop "should test one or the other validator based on the status of the first, and accumulate the error in case both first and second one fail" $ \(v::String) (b1::Bool) (b2::Bool) ->
      let
        t1 = bool (failV ["error 1"]) passV b1
        t2 = bool (failV ["error 2"]) passV b2
        t3 = bool (failV ["error 3"]) passV b2
      in
        validateP (scanV t2 t3 t1) v `shouldBe` case (b1, b2) of
          (_, True) -> Left $ Valid v
          (True, False) -> Right $ ["error 3"]
          (False, False) -> Right $ ["error 1", "error 2"]

{- CORE TESTS -}

-- newtype Test a = Test { payload :: a }
--   deriving ( Eq, Show )

-- skipTest :: Monad m => Validator (Test String) m (Test (Maybe String))
-- skipTest = Test <$> skip

-- checkTest :: Monad m => Validator (Test Int) m (Test (Maybe String))
-- checkTest = Test <$> check payload nonover18

-- checksTest :: Monad m => Validator (Test Text) m (Test (Maybe [String]))
-- checksTest = Test <$> checks payload [nonbollocks, nonshort]

-- mapCheckTest :: Monad m => Validator (Test [Text]) m (Test (Maybe [Maybe String]))
-- mapCheckTest = Test <$> mapCheck payload nonempty'

-- mapChecksTest :: Monad m => Validator (Test [Text]) m (Test (Maybe [Maybe [String]]))
-- mapChecksTest = Test <$> mapChecks payload [nonempty, nonbollocks]

-- subValidatorTest :: Monad m => Validator (Test User) m (Test (Maybe UserError))
-- subValidatorTest = Test <$> subValidator payload userValidator

-- mapSubValidatorTest :: Monad m => Validator (Test [User]) m (Test (Maybe [Maybe UserError]))
-- mapSubValidatorTest = Test <$> mapSubValidator payload userValidator

--

{- COMPLEX TESTS -}

data User = User
  { email    :: String
  , username :: String
  } deriving (Eq, Show)

data UserError = UserError
  { eemail    :: Maybe [String]
  , eusername :: Maybe [String]
  } deriving (Eq, Show)

userValidator :: forall m. Monad m => Validator m User UserError
userValidator = UserError
  <$> check email (nonempty @m)
  <*> check username [nonempty @m, nonbollocks, nonshort]

-- badUser :: User
-- badUser = User "boaty@mcboatface.com" "bollocks"

-- badUserError :: UserError
-- badUserError = User Nothing $ Just ["can't be bollocks", "too short"]

-- goodUser :: User
-- goodUser = User "hello@kitty.com" "kittyusername"

--

-- data Article' a = Article
--   { id      :: Validatable a String            Int
--   , title   :: Validatable a [String]          Text
--   , content :: Validatable a [String]          Text
--   , tags1   :: Validatable a [Maybe [String]]  [Text]
--   , tags2   :: Validatable a [Maybe String]    [Text]
--   , author  :: Validatable a UserError         User
--   , authors :: Validatable a [Maybe UserError] [User]
--   }

-- type Article = Article' 'Simple
-- deriving instance Eq   Article
-- deriving instance Show Article

-- type ArticleError = Article' 'Report
-- deriving instance Eq   ArticleError
-- deriving instance Show ArticleError

-- articleValidator :: Monad m => Validator Article m ArticleError
-- articleValidator = Article
--   <$> chk id      nonover18
--   <*> chk title   [nonempty, nonbollocks]
--   <*> chk content [nonempty, nonshort, nonbollocks]
--   <*> chk tags1   [nonempty, nonbollocks]
--   <*> chk tags2   nonempty'
--   <*> chk author  userValidator
--   <*> chk authors userValidator

-- badArticle :: Article
-- badArticle = Article
--   { id      = 17
--   , title   = ""
--   , content = "bollocks"
--   , tags1   = ["", "tag01", "tag02"]
--   , tags2   = ["tag01", ""]
--   , author  = badUser
--   , authors = [badUser, goodUser]
--   }

-- badArticleError :: ArticleError
-- badArticleError = Article
--   { id      = Just "must be greater than 18"
--   , title   = Just ["can't be empty"]
--   , content = Just ["too short", "can't be bollocks"]
--   , tags1   = Just [Just ["can't be empty"], Nothing, Nothing]
--   , tags2   = Just [Nothing, Just "can't be empty"]
--   , author  = Just badUserError
--   , authors = Just [Just badUserError, Nothing]
--   }

-- goodArticle :: Article
-- goodArticle = Article
--   { id      = 19
--   , title   = "This is a dumb title!"
--   , content = "Even dumber content, let's include some lipsum : Lorem ipsum dolor sit amet, consectetur adipisicing elit."
--   , tags1   = ["tag01", "tag02"]
--   , tags2   = ["tag01", "tag02"]
--   , author  = goodUser
--   , authors = [goodUser, goodUser]
--   }

--

{- BASIC FIELD TESTS -}

nonover18 :: Applicative m => Validator m Int [String]
nonover18 = makeV $ pure . \n -> bool
  (Just ["must be greater than 18"])
  Nothing
  (n < 18)

nonempty :: Monad m => Validator m String [String]
nonempty = testV passV (failV ["can't be empty"]) $ pure . null

nonbollocks :: Monad m => Validator m String [String]
nonbollocks = testV passV (failV ["can't be bollocks"]) $ pure . (=="bollocks")

nonshort :: Monad m => Validator m String [String]
nonshort = testV passV (failV ["too short"]) $ pure . ((<10) . length)
