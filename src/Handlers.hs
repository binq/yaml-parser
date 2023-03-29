{-# LANGUAGE FieldSelectors, NoOverloadedRecordUpdate, NoRebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Handlers (
    pattern AlexInput,
    pushStartcode,
    popStartcode,
    peekStartcode,
    popLeadingSpaces,
    setLeadingSpaces,
    processLeadingSpaces,
    onBreakNonContent,
    grab,
    recordSpaces,
    resetSpaces,
    alexEOF) where

-- base
import Control.Category        (Category (..))
import Control.Monad           (Monad (..))
import Data.ByteString.Lazy    (ByteString (), take)
import Data.Char               (Char ())
import Data.Either             (Either (..))
import Data.Function           (($))
import Data.Int                (Int (), Int64 ())
import Data.List               (head, singleton, uncons)
import Data.Maybe              (Maybe (..), maybe)
import GHC.Err                 (undefined)

-- local
import Lexer (
    Alex (..), 
    AlexInput, 
    AlexPosn (..),
    AlexState (..))

import Aux1 (
    AlexUserState (..),
    Token (..))

pushStartcode :: 
    Int -> Alex ()
pushStartcode sc =
    Alex $ \s ->
        let
            startcodes' = sc:s.alex_ust.startcodes
            alex_ust' = s.alex_ust {startcodes = startcodes'}
            s' = s {alex_ust = alex_ust'}
        in Right (s', ())

popStartcode :: Alex Int
popStartcode =
    Alex $ \s ->
        let
            (r, startcodes') = maybe undefined id . uncons $ s.alex_ust.startcodes
            alex_ust' = s.alex_ust {startcodes = startcodes'}
            s' = s {alex_ust = alex_ust'}
        in Right (s', r)

peekStartcode :: Alex Int
peekStartcode =
    Alex $ \s ->
        let
            r = head s.alex_ust.startcodes
        in Right (s, r)

popLeadingSpaces :: Alex (Maybe Int64)
popLeadingSpaces =
    Alex $ \s ->
        let 
            r = s.alex_ust.leadingSpaces
            alex_ust = s.alex_ust
            alex_ust' = alex_ust {leadingSpaces = Nothing}
            s' = s {alex_ust = alex_ust'}
        in Right (s', r)
        
setLeadingSpaces :: Maybe Int64 -> Alex ()
setLeadingSpaces leadingSpaces =
    Alex $ \s -> 
        let
            alex_ust = s.alex_ust {leadingSpaces}
            s' = s {alex_ust}
        in Right (s', ())

pattern AlexInput ::
    Int -> Int -> Int -> 
    Char -> 
    ByteString ->
    Int64 -> 
    AlexInput
pattern
    AlexInput {
        absolute_offset, line_number, column_number,
        previous_char,
        current_input,
        startcode 
    } = (
        AlexPn absolute_offset line_number column_number,
        previous_char,
        current_input,
        startcode
    )

{-# COMPLETE AlexInput #-}

processLeadingSpaces ::
    (result ~ [token], token ~ Token s, s ~ ByteString) => 
    Alex result
processLeadingSpaces =
    popLeadingSpaces >>= return . \case
        Nothing -> []
        (Just l) -> [LeadingSpaces l]

onBreakNonContent ::
    (token ~ Token s, s ~ ByteString) => 
    Alex () -> token -> Alex token
onBreakNonContent f = \case
        r@(BreakNonContent _) -> f >> return r
        r -> return r

grab ::
    (result ~ [token], token ~ Token s, s ~ ByteString) => 
    (s -> token) -> AlexInput -> Int64 -> Alex result
grab f (AlexInput {current_input}) len = do
    rs <- processLeadingSpaces
    r <- onBreakNonContent 
        (setLeadingSpaces . Just $ 0)
        (f . take len $ current_input)
    return $ r:rs

recordSpaces ::
    (result ~ [token], token ~ Token s) =>
    AlexInput -> Int64 -> Alex result
recordSpaces _ len = do
    setLeadingSpaces . Just $ len
    return []

resetSpaces ::
    (result ~ [token], token ~ Token s) =>
    AlexInput -> Int64 -> Alex result
resetSpaces _ _ = do
    setLeadingSpaces Nothing
    return []

alexEOF :: (result ~ [token], token ~ Token s) => Alex result
alexEOF = return . singleton $ EOF
