{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
import Control.Applicative
import Data.Array
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Functor.Identity
import System.IO
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Printf

main = contestMain printer solver parser

solver (a:as) = solve1 a + sum (map solve2 as)

solve1 []  = 0
solve1 [a] = a
solve1 (1:a2:as) = 1 + solve1 (rev a2 : as)
solve1 ass@(0:a2:as) = solve1' (reverse ass)

solve1' [] = 0
solve1' [a] = a
solve1' (a1:a2:as) = a1 + solve1' (rev a2 : as)

solve2 = solve1 . map rev

rev 0 = 1
rev 1 = 0

printer = hPrint

parser = do
   m <- number
   n <- number
   a <- count m $ count n number
   return a

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
