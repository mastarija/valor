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

  describe "oneCheck" $ do
    prop "should fail if value is under 18" $ \n ->
      let
        e = ["must be greater than 18"]
      in
        shouldBe
          (validateP (oneCheck id nonover18) n)
          (bool (Left $ Valid n) (Right $ Just e) (n < 18))

  describe "oneChecks" $ do
    prop "should test multiple rules and fail if any of them fail" $ \s ->
      let
        e  = (bool mempty e1 (null s)) <> (bool mempty e2 (length s < 10))
        e1 = ["can't be empty"]
        e2 = ["too short"]
      in
        shouldBe
          (validateP (oneChecks id [nonempty, nonshort]) s)
          (bool (Left $ Valid s) (Right $ Just e) (e /= mempty))

  describe "mapCheck" $ do
    prop "should test a single rule over a list of subjects" $ \(ns::[Int]) ->
      let
        e  = ["must be greater than 18"]
        es = fmap (bool Nothing (Just e) . (<18)) ns
      in
        shouldBe
          (validateP (mapCheck id nonover18) ns)
          (bool (Left $ Valid ns) (Right $ Just es) (any (<18) ns))

  describe "mapChecks" $ do
    prop "should test multiple rules over a list of subjects" $ \(ss::[String]) ->
      let
        ef = \s ->
          let
            es' = (bool mempty e1 (null s)) <> (bool mempty e2 (length s < 10))
          in
            bool Nothing (Just es') (es' /= mempty)
        e1 = ["can't be empty"]
        e2 = ["too short"]
        es = fmap ef ss
      in
        shouldBe
          (validateP (mapChecks id [nonempty, nonshort]) ss)
          (bool (Left $ Valid ss) (Right $ Just es) (any (/=mempty) es))

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

userValidator :: Monad m => Validator m User UserError
userValidator = UserError
  <$> check email nonempty
  <*> check username (mconcat [nonempty, nonbollocks, nonshort])

badUser :: User
badUser = User "boaty@mcboatface.com" "bollocks"

badUserError :: UserError
badUserError = UserError Nothing $ Just ["can't be bollocks", "too short"]

goodUser :: User
goodUser = User "hello@kitty.com" "kittyusername"

--

data Article = Article
  { aid     :: Int
  , title   :: String
  , content :: String
  , tags1   :: [String]
  , tags2   :: [String]
  , author  :: User
  , authors :: [User]
  } deriving (Eq, Show)

data ArticleError = ArticleError
  { eaid     :: Maybe [String]
  , etitle   :: Maybe [String]
  , econtent :: Maybe [String]
  , etags1   :: Maybe [Maybe [String]]
  , etags2   :: Maybe [Maybe [String]]
  , eauthor  :: Maybe UserError
  , eauthors :: Maybe [Maybe UserError]
  } deriving (Eq, Show)

articleValidator :: Monad m => Validator m Article ArticleError
articleValidator = ArticleError
  <$> check aid     nonover18
  <*> check title   (mconcat [nonempty, nonbollocks])
  <*> check content (mconcat [nonempty, nonshort, nonbollocks])
  <*> check tags1   (mconcat [nonempty, nonbollocks])
  <*> check tags2   nonempty
  <*> check author  userValidator
  <*> check authors userValidator

badArticle :: Article
badArticle = Article
  { aid     = 17
  , title   = ""
  , content = "bollocks"
  , tags1   = ["", "tag01", "tag02"]
  , tags2   = ["tag01", ""]
  , author  = badUser
  , authors = [badUser, goodUser]
  }

badArticleError :: ArticleError
badArticleError = ArticleError
  { eaid     = Just ["must be greater than 18"]
  , etitle   = Just ["can't be empty"]
  , econtent = Just ["too short", "can't be bollocks"]
  , etags1   = Just [Just ["can't be empty"], Nothing, Nothing]
  , etags2   = Just [Nothing, Just ["can't be empty"]]
  , eauthor  = Just badUserError
  , eauthors = Just [Just badUserError, Nothing]
  }

goodArticle :: Article
goodArticle = Article
  { aid      = 19
  , title   = "This is a dumb title!"
  , content = "Even dumber content, let's include some lipsum : Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  , tags1   = ["tag01", "tag02"]
  , tags2   = ["tag01", "tag02"]
  , author  = goodUser
  , authors = [goodUser, goodUser]
  }

--

{- BASIC FIELD TESTS -}

nonover18 :: Monad m => Validator m Int [String]
nonover18 = makeV $ pure . \n -> bool
  Nothing
  (Just ["must be greater than 18"])
  (n < 18)

nonempty :: Monad m => Validator m String [String]
nonempty = testV passV (failV ["can't be empty"]) $ pure . null

nonbollocks :: Monad m => Validator m String [String]
nonbollocks = testV passV (failV ["can't be bollocks"]) $ pure . (=="bollocks")

nonshort :: Monad m => Validator m String [String]
nonshort = testV passV (failV ["too short"]) $ pure . ((<10) . length)
