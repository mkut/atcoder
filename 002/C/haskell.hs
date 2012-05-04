{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
import System.IO

-- lib imports
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Printf
import Data.Functor.Identity
import Control.Monad

-- Main
main = contestMain printer solver parser

solver str = minimum $ map (solver' str) lrs
   where
      keys = "ABXY"
      lrs = [ (l, l', r, r') | l <- keys, l' <- keys, r <- keys, r' <- keys ]

solver' []  _ = 0
solver' [_] _ = 1
solver' (a:b:c) lr@(l, l', r, r')
   | a == l && b == l' = solver' c lr + 1
   | a == r && b == r' = solver' c lr + 1
   | otherwise         = solver' (b:c) lr + 1

printer = hPrint

parser = do
   n <- number
   word

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

number' :: (Stream s m Char, Integral a, Read a) => ParsecT s u m a
number' =
       do ds <- many1 digit
          return (read ds)
   <?> "number"

number :: (Stream s m Char, Integral a, Read a) => ParsecT s u m a
number = do spaces; number'

-- Milib.IO
word' :: Stream s m Char => ParsecT s u m String
word' = many1 letter

word :: Stream s m Char => ParsecT s u m String
word = do spaces; word'

-- vim: set expandtab:
