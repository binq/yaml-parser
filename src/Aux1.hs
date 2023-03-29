{-# OPTIONS_GHC -Wno-partial-fields #-}

module Aux1 (
    AlexUserState (..),
    Token (..),
    alexInitUserState) where

-- base
import Data.Eq                 (Eq (..))
import Data.Int                (Int (), Int64 ())
import Data.Kind               (Type)
import Data.List               (repeat)
import Data.Maybe              (Maybe (..))
import GHC.Exts                (IsList (..))
import GHC.Num                 (Num (..))
import Text.Show               (Show (..))

type Token :: Type -> Type
data Token s where
    EOF :: Token s
    LeadingSpaces :: { amount :: Int64 } -> Token s

    Alias :: { value :: s } -> Token s
    Anchor :: { value :: s } -> Token s
    AnchorName :: { value :: s } -> Token s
    BreakNonContent :: { value :: s } -> Token s
    CollectEntry :: { value :: s } -> Token s
    Comment :: { value :: s } -> Token s
    Directive :: { value :: s } -> Token s
    MappingEnd :: { value :: s } -> Token s
    MappingKey :: { value :: s } -> Token s
    MappingStart :: { value :: s } -> Token s
    MappingValue :: { value :: s } -> Token s
    Reserved :: { value :: s } -> Token s
    SeparateInline :: { value :: s } -> Token s
    SequenceEnd :: { value :: s } -> Token s
    SequenceEntry :: { value :: s } -> Token s
    SequenceStart :: { value :: s } -> Token s
    String :: { value :: s } -> Token s
    Tag :: { value :: s } -> Token s
    deriving (Eq, Show)

data AlexUserState :: Type where
    AlexUserState :: {
        context :: forall s . [Token s],
        leadingSpaces :: Maybe Int64,
        startcodes :: [Int]
    } -> AlexUserState

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { 
    context = [],
    leadingSpaces = Just 0,
    startcodes = repeat 0
}
