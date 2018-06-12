{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
--
module Valor where
--
import Valor.Utils
import Data.Text ( Text )
import GHC.Generics
import Data.Functor.Identity
import Data.List.NonEmpty ( NonEmpty )
import Data.Semigroup
--

data Form = Form
  { name :: String
  , age  :: Int
  , user :: User
  }

data FormError = FormError
  { name :: Maybe String
  , age  :: Maybe String
  , user :: Maybe UserError
  }

data User = User
  { username :: String
  , password :: String
  }

data UserError = UserError
  { username :: Maybe String
  , password :: Maybe String
  }

--------------------------------------------------------------------------------

nonempty :: String -> Maybe String
nonempty s
  | length s == 0 = Just "can't be empty"
  | otherwise     = Nothing

positive :: Int -> Maybe String
positive n
  | n < 0     = Just "can't be negative"
  | otherwise = Nothing

nonzero :: Int -> Maybe String
nonzero n
  | n == 0    = Just "can't be zero"
  | otherwise = Nothing

--------------------------------------------------------------------------------

data Validated e a = Valid a | Invalid (NonEmpty e) a | NonValid
  deriving ( Eq, Show, Functor )

instance Semigroup (Validated e a) where
  NonValid <> x = x
  x <> NonValid = x

  Valid _ <> x  = x
  x <> Valid _  = x

  Invalid e1 _ <> Invalid e2 a = Invalid (e1 <> e2) a

instance Monoid (Validated e a) where
  mempty  = NonValid
  mappend = (<>)

validateForm :: Form -> FormError
validateForm f = FormError
  (nonempty $ r @Form name f)
  (positive $ r @Form age  f)
  (Just $ validateUser $ r @Form user f)

validateUser :: User -> UserError
validateUser u = UserError
  (nonempty $ r @User username u)
  (nonempty $ r @User password u)