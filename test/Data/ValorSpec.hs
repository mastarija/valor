{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
--
module Data.ValorSpec where
--
import Prelude hiding ( id )
import Test.Hspec ( Spec, describe, it, shouldBe )

import Data.Text ( Text )
import Data.Valor
  ( Validatable, Validator, Validate, Identity, ExceptT
  , skip, check, mapCheck, checks, mapChecks, subValidator, mapSubValidator
  , validatePure, throwE
  )
import qualified Data.Text as Text
--

spec :: Spec
spec = do
  describe "skip" $ do
    it "should return 'Nothing'" $
      validatePure skipTest (Test "test") `shouldBe`
        Nothing
  describe "check" $ do
    it "should return 'Nothing'" $
      validatePure checkTest (Test $ 19) `shouldBe`
        Nothing
    it "should return an error" $
      validatePure checkTest (Test $ 11) `shouldBe`
        Just (Test $ Just "must be greater than 18")
  describe "mapCheck" $ do
    it "should return 'Nothing'" $
      validatePure mapCheckTest (Test $ ["hello", "again"]) `shouldBe`
        Nothing
    it "should return an error" $
      validatePure mapCheckTest (Test $ ["", "hello", "", "again"]) `shouldBe`
        Just (Test $ Just [Just "can't be empty", Nothing, Just "can't be empty", Nothing])
  describe "checks" $ do
    it "should return 'Nothing'" $
      validatePure checksTest (Test "I'm super ok for now!") `shouldBe`
        Nothing
    it "should return an error" $
      validatePure checksTest (Test "bollocks") `shouldBe`
        Just (Test $ Just ["can't be bollocks", "too short"])
  describe "mapChecks" $ do
    it "should return 'Nothing'" $
      validatePure mapChecksTest (Test ["Hello", "world"]) `shouldBe`
        Nothing
    it "should return an error" $
      validatePure mapChecksTest (Test ["", "bollocks", "ok"]) `shouldBe`
        Just (Test $ Just [Just ["can't be empty"], Just ["can't be bollocks"], Nothing])
  describe "subValidator" $ do
    it "should return 'Nothing'" $ do
      validatePure subValidatorTest (Test goodUser) `shouldBe`
        Nothing
    it "should return an error" $ do
      validatePure subValidatorTest (Test badUser) `shouldBe`
        Just (Test $ Just badUserError)
  describe "mapSubValidator" $ do
    it "should return 'Nothing'" $ do
      validatePure mapSubValidatorTest (Test [goodUser, goodUser]) `shouldBe`
        Nothing
    it "should return an error" $ do
      validatePure mapSubValidatorTest (Test [goodUser, badUser]) `shouldBe`
        Just (Test $ Just [Nothing, Just badUserError])
  describe "validate 'Article'" $ do
    it "should return 'Nothing'" $ do
      validatePure articleValidator goodArticle `shouldBe`
        Nothing
    it "should return an error" $ do
      validatePure articleValidator badArticle `shouldBe`
        Just badArticleError
  describe "validate 'User'" $ do
    it "should return 'Nothing'" $ do
      validatePure userValidator goodUser `shouldBe`
        Nothing
    it "should return an error" $ do
      validatePure userValidator badUser `shouldBe`
        Just badUserError

--

{- CORE TESTS -}

newtype Test a = Test { payload :: a }
  deriving ( Eq, Show )

skipTest :: Monad m => Validator (Test String) m (Test (Maybe String))
skipTest = Test <$> skip

checkTest :: Monad m => Validator (Test Int) m (Test (Maybe String))
checkTest = Test <$> check payload nonover18

checksTest :: Monad m => Validator (Test Text) m (Test (Maybe [String]))
checksTest = Test <$> checks payload [nonbollocks, nonshort]

mapCheckTest :: Monad m => Validator (Test [Text]) m (Test (Maybe [Maybe String]))
mapCheckTest = Test <$> mapCheck payload nonempty'

mapChecksTest :: Monad m => Validator (Test [Text]) m (Test (Maybe [Maybe [String]]))
mapChecksTest = Test <$> mapChecks payload [nonempty, nonbollocks]

subValidatorTest :: Monad m => Validator (Test User) m (Test (Maybe UserError))
subValidatorTest = Test <$> subValidator payload userValidator

mapSubValidatorTest :: Monad m => Validator (Test [User]) m (Test (Maybe [Maybe UserError]))
mapSubValidatorTest = Test <$> mapSubValidator payload userValidator

data User' a = User
  { email    :: Validatable a String   Text
  , username :: Validatable a [String] Text
  }

type User = User' Identity
deriving instance Eq   User
deriving instance Show User

type UserError = User' Validate
deriving instance Eq   UserError
deriving instance Show UserError

userValidator :: Monad m => Validator User m UserError
userValidator = User
  <$> check  email nonempty'
  <*> checks username [nonempty, nonbollocks, nonshort]

badUser :: User
badUser = User "boaty@mcboatface.com" "bollocks"

badUserError :: UserError
badUserError = User Nothing $ Just ["can't be bollocks", "too short"]

goodUser :: User
goodUser = User "hello@kitty.com" "kittyusername"

--

{- COMPLEX TESTS -}

data Article' a = Article
  { id      :: Validatable a String            Int
  , title   :: Validatable a [String]          Text
  , content :: Validatable a [String]          Text
  , tags1   :: Validatable a [Maybe [String]]  [Text]
  , tags2   :: Validatable a [Maybe String]    [Text]
  , author  :: Validatable a UserError         User
  , authors :: Validatable a [Maybe UserError] [User]
  }

type Article = Article' Identity
deriving instance Eq   Article
deriving instance Show Article

type ArticleError = Article' Validate
deriving instance Eq   ArticleError
deriving instance Show ArticleError

articleValidator :: Monad m => Validator Article m ArticleError
articleValidator = Article
  <$> check           id      nonover18
  <*> checks          title   [nonempty, nonbollocks]
  <*> checks          content [nonempty, nonshort, nonbollocks]
  <*> mapChecks       tags1   [nonempty, nonbollocks]
  <*> mapCheck        tags2   nonempty'
  <*> subValidator    author  userValidator
  <*> mapSubValidator authors userValidator

badArticle :: Article
badArticle = Article
  { id      = 17
  , title   = ""
  , content = "bollocks"
  , tags1   = ["", "tag01", "tag02"]
  , tags2   = ["tag01", ""]
  , author  = badUser
  , authors = [badUser, goodUser]
  }

badArticleError :: ArticleError
badArticleError = Article
  { id      = Just "must be greater than 18"
  , title   = Just ["can't be empty"]
  , content = Just ["too short", "can't be bollocks"]
  , tags1   = Just [Just ["can't be empty"], Nothing, Nothing]
  , tags2   = Just [Nothing, Just "can't be empty"]
  , author  = Just badUserError
  , authors = Just [Just badUserError, Nothing]
  }

goodArticle :: Article
goodArticle = Article
  { id      = 19
  , title   = "This is a dumb title!"
  , content = "Even dumber content, let's include some lipsum : Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  , tags1   = ["tag01", "tag02"]
  , tags2   = ["tag01", "tag02"]
  , author  = goodUser
  , authors = [goodUser, goodUser]
  }

--

{- BASIC FIELD TESTS -}

nonover18 :: Monad m => Int -> ExceptT String m Int
nonover18 n = if n < 18
  then throwE "must be greater than 18"
  else pure n

nonempty' :: Monad m => Text -> ExceptT String m Text
nonempty' t = if Text.null t
  then throwE "can't be empty"
  else pure t

nonempty :: Monad m => Text -> ExceptT [String] m Text
nonempty t = if Text.null t
  then throwE ["can't be empty"]
  else pure t

nonbollocks :: Monad m => Text -> ExceptT [String] m Text
nonbollocks t = if t == "bollocks"
  then throwE ["can't be bollocks"]
  else pure t

nonshort :: Monad m => Text -> ExceptT [String] m Text
nonshort t = if Text.length t < 10
  then throwE ["too short"]
  else pure t