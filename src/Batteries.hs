{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Batteries (
    module Lexer,
    module Aux1,
    lexer,
    fromStdIn
) where

import Data.Bool               (otherwise)
import Data.ByteString.Lazy    (ByteString (), getContents)
import Data.Either             (Either (..))
import Data.Function           (($))
import Data.List               ((++))
import Data.String             (String ())
import GHC.Base                (($!))
import GHC.Exts.Rebindable
import System.IO               (IO (), putStrLn)
import Text.Show               (Show (..))

import Aux1
import Lexer

lexer :: ByteString -> Either String [Token ByteString]
lexer str = runAlex str f
    where
    cycle :: (Monad Alex) => [Token ByteString] -> Alex [Token ByteString]
    cycle r = do 
        toks <- alexMonadScan
        if | [EOF] == toks -> return r
           | otherwise -> cycle $! toks ++ r
    f :: Alex [Token ByteString]
    f = cycle []

fromStdIn :: IO ()
fromStdIn = getContents >>= return . lexer >>= \case
    Left e -> putStrLn e
    Right r -> putStrLn . show $ r
