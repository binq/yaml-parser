{-# LANGUAGE NoRebindableSyntax #-}

module UserState (
    Alex (..),
    AlexPosn (..),
    AlexState (..),
    AlexUserState (..),
    alexInitUserState
) where

import Prelude (Int ())
import Data.Char (Char ())
import Data.Either
import Data.Kind
import Data.String (String ())

-- import GHC.Exts.Rebindable
-- import Lens.Micro

type AlexUserState :: Type
data AlexUserState where
    AlexUserState :: { commentDepth :: Int, stringValue :: String } -> AlexUserState

type AlexPosn :: Type
data AlexPosn where
    AlexPosn :: {
        absoluteCharacterOffset :: Int,
        lineNumber :: Int,
        columnNumber :: Int
    } -> AlexPosn

type AlexInput :: Type
type AlexInput = String

type AlexState :: Type
data AlexState where
    AlexState :: {
        pos :: AlexPosn,      -- position at current input location
        inp :: AlexInput,     -- the current input
        chr :: Char,          -- the character before the input
        scd :: Int,           -- the current startcode
        ust :: AlexUserState  -- custom data
    } -> AlexState

type Alex :: Type -> Type
newtype Alex a where
    Alex :: { unAlex :: AlexState -> Either String (AlexState, a) } -> Alex a

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { commentDepth = 0, stringValue = "" }

{-
startPosition :: Position
startPosition = Position 0 1 1

getCommentDepth :: Alex Int
getCommentDepth = Alex f
    where
        f :: AlexState -> Either String (AlexState, Int)
        f = id &&& #commentDepth . #ust >>> Right

setCommentDepth :: Int -> Alex ()
setCommentDepth ss = Alex f
    where
        f :: AlexState -> Either String (AlexState, ())
        f = #commentDepth . #ust .~ ss &&& const () >>> Right

getStringValue :: Alex String
getStringValue = Alex f
    where
        f :: AlexState -> Either String (AlexState, String)
        f = id &&& #stringValue . #ust >>> Right

setStringValue :: String -> Alex ()
setStringValue ss = Alex f
    where
        f :: AlexState -> Either String (AlexState, ())
        f = #stringValue . #ust .~ ss &&& const () >>> Right

addCharToStringValue :: Char -> Alex ()
addCharToStringValue c = Alex f
    where
        f :: AlexState -> Either String (AlexState, ())
        f = #stringValue . #ust %~ (c:) &&& const () >>> Right

runAlex :: String -> Alex a -> Either String a
runAlex input (Alex f) = 
        case f as of
            Left msg -> Left msg
            Right (_, a) -> Right a
    where
        as = AlexState {
            pos = startPosition,
            inp = input,
            chr = '\n',
            scd = 0,
            ust = alexInitUserState
        } 

alexGetInput :: Alex AlexInput
alexGetInput = Alex f
    where
        f :: AlexState -> Either String (AlexState, AlexInput)
        f = id &&& #inp >>> Right

alexSetInput :: AlexInput -> Alex ()
alexSetInput x = Alex f
    where
        f :: AlexState -> Either String (AlexState, ())
        f = #inp .~ x &&& const () >>> Right

alexError :: String -> Alex a
alexError e = Alex f
    where
        f :: AlexState -> Either String (AlexState, a)
        f = const e >>> Left

alexGetStartCode :: Alex Int
alexGetStartCode = Alex f
    where
        f :: AlexState -> Either String (AlexState, Int)
        f = id &&& #scd >>> Right

alexSetStartCode :: Int -> Alex ()
alexSetStartCode x = Alex f
    where
        f :: AlexState -> Either String (AlexState, ())
        f = #scd .~ x &&& const () >>> Right
-}