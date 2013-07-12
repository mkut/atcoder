{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Functor.Identity
import System.IO
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Printf

main = contestMain printer solver parser

solver (n, q, cs) = solver' q fstName cs
   where
      fstName = "kogakubu10gokan"

solver' _ n []                = n
solver' q n ((year, name):cs)
   | q <  year = n
   | q >= year = solver' q name cs

printer = hPutStrLn

parser = do
   n <- number
   q <- number
   cs <- count n $ liftM2 (,) number word2
   return (n, q, cs)

word2 = do spaces; many1 (letter <|> digit)

-- Milib.IO
number' :: (Stream s m Char, Integral a, Read a) => ParsecT s u m a
number' =
       do ds <- many1 digit
          return (read ds)
   <?> "number"

number :: (Stream s m Char, Integral a, Read a) => ParsecT s u m a
number = do spaces; number'

-- Milib.Contest
type Printer a = Handle -> a -> IO ()
type Solver a b = a -> b
type Parser b = Stream C.ByteString Identity Char => Parsec C.ByteString () b
type CMain a b = Printer b -> Solver a b -> Parser a -> IO ()
type HCMain a b = Handle -> Handle -> CMain a b

instance Stream C.ByteString Identity Char where
   uncons = return . C.uncons

hContestMain :: HCMain a b
hContestMain hin hout printer solver parser = do
   input <- C.hGetContents hin
   case parse parser "" input of
      Left err -> do { hPutStr stderr "parse err: "; hPrint stderr err }
      Right x  -> printer hout $ solver x

contestMain :: CMain a b
contestMain = hContestMain stdin stdout

-- vim: set expandtab:
