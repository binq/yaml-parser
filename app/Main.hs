{-# LANGUAGE ImportQualifiedPost #-}

module Main (
  main) where

import Control.Exception       (throw)
import Data.ByteString         (toStrict)
import Data.ByteString.Lazy    (ByteString (), readFile)
import Data.Either             (Either (..))
import Data.Function           ((&))
import Data.List               (reverse)
import Data.String             qualified as Data (String ()) 
import Data.Text.Lazy          (Text (), lines, pack, strip, unlines, unwords)
import Data.Text.Lazy.IO       (putStrLn)
import Data.Yaml               (ParseException, Value (), decodeEither')
import GHC.IO.Encoding         (setLocaleEncoding, utf8)
import System.Environment      (getArgs)
import System.IO               (IO ())
import Text.Show               (Show (..))

import GHC.Exts.Rebindable     
import Paths_yaml_parser       (getDataFileName)
import Batteries               (Token (..), lexer)

main :: IO ()
main =
  do
    setLocaleEncoding utf8
    content <- "data/test.yaml" & getDataFileName >>= readFile
    getArgs >>= putStrLn . \case
      ["parsed"] -> fromParsed content
      ["lib"] -> fromLib content
      _ -> "select context: parsed or lib"
  where
    showTokens :: Token ByteString -> Text
    showTokens = \case
      BreakNonContent _ -> "\n"
      EOF -> "EOF"
      LeadingSpaces (pack . show -> v) -> unwords ["Indent", v]
      (pack . show -> v) -> v

    g :: Either Data.String [Token ByteString] -> [Token ByteString]
    g (Left e) = error e
    g (Right r) = reverse r

    f :: Either ParseException Value -> Data.String
    f (Left e) = throw e
    f (Right a) = show a

    fromParsed :: ByteString -> Text
    fromParsed = unlines . fmap strip . lines . unwords . fmap showTokens . g . lexer

    fromLib :: ByteString -> Text
    fromLib = pack . f . decodeEither' @Value . toStrict
