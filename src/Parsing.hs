{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-} -- Yeah, really

module Parsing(Parser, ParseResult, lineParser, 
    parseAdventFile, parseAdventFile',
    adventFile, adventFile', countSepBy) where

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as C
import Data.Void(Void)
import Data.Functor.Identity(Identity)
import Control.Applicative(Alternative)

type Parser = MP.ParsecT Void String Identity -- No custom errors, String parser, using IO
type ParseResult m a = m (Either (MP.ParseErrorBundle String Void) a)

lineParser :: (MP.MonadParsec e s m, MP.Token s ~ Char) => m a -> m [a]
lineParser p = MP.sepEndBy p $ C.char '\n'

adventFile :: Int -> String
adventFile i = adventFile' (show i)

adventFile' :: String -> String
#ifdef mingw32_HOST_OS
adventFile' s = "C:\\Users\\me\\advent2018\\day" ++ s ++ ".txt"
#else
adventFile' s = "/mnt/c/Users/me/advent2018/day" ++ s ++ ".txt"
#endif

parseAdventFile' :: MP.Parsec e String a -> String -> IO (Either (MP.ParseErrorBundle String e) a)
parseAdventFile' p f = MP.runParser p f <$> readFile f where

parseAdventFile :: MP.Parsec e String a -> Int -> IO (Either (MP.ParseErrorBundle String e) a)
parseAdventFile p f = parseAdventFile' p (adventFile f)

countSepBy :: (Alternative m, Monad m) => Int -> m a -> m b -> m [a]
countSepBy n p sep | n > 0 = (:) <$> p <*> MP.count (n-1) (sep *> p) 
                   | otherwise = pure []