{-# LANGUAGE Trustworthy #-}

module GHC.Exts.Rebindable (
    module Control.Applicative,
    module Control.Arrow,
    module Control.Category,
    module Control.Monad,
    module Control.Monad.Fail,
    module Data.Bool,
    module Data.Eq,
    module Data.Functor,
    module Data.Ord,
    module Data.String,
    module GHC.Err,
    module GHC.Exts,
    module GHC.Num,
    module GHC.OverloadedLabels,
    module GHC.Real,
    ifThenElse
) where

import Control.Applicative  (Applicative (..))
import Control.Arrow        (Arrow (..), ArrowChoice (..), ArrowApply (..), ArrowLoop (..))
import Control.Category     (Category (..), (<<<), (>>>))
import Control.Monad        (Monad (..), join)
import Control.Monad.Fail   (MonadFail (..))
import Data.Bool            (Bool (..))
import Data.Eq              (Eq (..))
import Data.Functor         (Functor (..))
import Data.Ord             (Ord (..))
import Data.String          (IsString (..))
import GHC.Err              (error, undefined)
import GHC.Exts             (IsList (..))
import GHC.Num              (Num (..))
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Real             (Fractional (..), fromIntegral, mod)

ifThenElse :: forall a . Bool -> a -> a -> a
ifThenElse True  x _ = x
ifThenElse False _ y = y
