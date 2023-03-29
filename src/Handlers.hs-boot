module Handlers (
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
import Data.ByteString.Lazy    (ByteString ())
import Data.Int                (Int (), Int64 ())
import Data.Maybe              (Maybe (..))

-- local
import {-# SOURCE #-} Lexer (
        Alex (..),
        AlexInput
    )

import Aux1 (
    Token (..))

pushStartcode :: 
    Int -> Alex ()

popStartcode :: Alex Int

peekStartcode :: Alex Int

popLeadingSpaces :: Alex (Maybe Int64)

setLeadingSpaces :: Maybe Int64 -> Alex ()

processLeadingSpaces ::
    (result ~ [token], token ~ Token s, s ~ ByteString) => 
    Alex result

onBreakNonContent ::
    (token ~ Token s, s ~ ByteString) => 
    Alex () -> token -> Alex token

grab ::
    (result ~ [token], token ~ Token s, s ~ ByteString) => 
    (s -> token) -> AlexInput -> Int64 -> Alex result

recordSpaces ::
    (result ~ [token], token ~ Token s) =>
    AlexInput -> Int64 -> Alex result

resetSpaces ::
    (result ~ [token], token ~ Token s) =>
    AlexInput -> Int64 -> Alex result

alexEOF ::
    (result ~ [token], token ~ Token s) =>
    Alex result
