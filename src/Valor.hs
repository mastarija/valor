{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
--
module Valor where
--
import Data.Text

import GHC.Generics
import Data.Functor.Identity
--

r :: (r -> a) -> r -> a
r f = f

--------------------------------------------------------------------------------
-- | Handy type family for constructing validatable parametrized types.
type family Validatable f x where
  Validatable Identity x = x
  Validatable f        x = f x

--------------------------------------------------------------------------------
-- | Type representing validation result.
type Validated e a = Either e a

--------------------------------------------------------------------------------
-- | Thing doing the validation.
newtype Validator s e a m = Validator
  { unValidator :: s -> m (Validated e a)
  }

--------------------------------------------------------------------------------

data User = User
  { username :: Text
  , password :: Text
  } deriving ( Eq, Show, Generic )

data UserErrors = UserErrors
  { username :: Text
  , password :: Text
  } deriving ( Eq, Show, Generic )

validateUser :: User -> Either UserErrors User
validateUser u = undefined
