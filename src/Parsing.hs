{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Parsing(Parser, ParseResult, lineParser, parseAdventFile, adventFile) where

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as C
import Data.Void(Void)
import Data.Functor.Identity(Identity)

type Parser = MP.ParsecT Void String Identity -- No custom errors, String parser, using IO
type ParseResult m a = m (Either (MP.ParseErrorBundle String Void) a)

lineParser :: (MP.MonadParsec e s m, MP.Token s ~ Char) => m a -> m [a]
lineParser p = MP.many (p <* C.char '\n')

adventFile :: Int -> String
adventFile i = "C:\\Users\\me\\advent2018\\day" ++ (show i) ++ ".txt"

parseAdventFile :: (MP.Parsec e String a) -> Int -> IO (Either (MP.ParseErrorBundle String e) a)
parseAdventFile p i = MP.runParser p file <$> readFile file where
    file = adventFile i
