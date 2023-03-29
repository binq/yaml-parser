module Lexer (
    Alex (..),
    AlexInput,
    AlexPosn (..),
    AlexState (..)) where

import Control.Applicative     (Applicative (..))
import Data.ByteString.Lazy    (ByteString ())
import Data.Char               (Char ())
import Data.Either             (Either (..))
import Data.Functor            (Functor (..))
import Data.Int                (Int (), Int64 ())
import Data.Kind               (Type)
import Data.String             (String ())
import Control.Monad           (Monad (..))

-- local
import Aux1 (AlexUserState (..))

type AlexPosn :: Type
data AlexPosn = AlexPn Int Int Int

type AlexState :: Type
data AlexState where
    AlexState :: {
        alex_pos :: AlexPosn,
        alex_bpos :: Int64,
        alex_inp :: ByteString,
        alex_chr :: Char,
        alex_scd :: Int,
        alex_ust :: AlexUserState
    } -> AlexState

type AlexInput :: Type
type AlexInput =
        (
            AlexPosn,    -- current position,
            Char,        -- previous char
            ByteString,  -- current input string
            Int64        -- bytes consumed so far
        )

type Alex :: Type -> Type
newtype Alex a where
    Alex ::
        {
            unAlex :: AlexState -> Either String (AlexState, a)
        } -> Alex a

instance Functor Alex
instance Applicative Alex
instance Monad Alex
