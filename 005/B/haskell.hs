{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
import qualified Data.ByteString.Lazy.Char8 as C
import System.IO
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Printf
import Data.Functor.Identity
import Data.Array
import Control.Monad
import Control.Arrow

main = contestMain printer solver parser

solver (p, w, c) = map ((c!) . join (***) fix) $ take 4 ps
   where
      ps = p : map (next w) ps
      fix x
         | x < 1     = fix (2 - x)
         | x > 9     = fix (18 - x)
         | otherwise = x
      next "R"  (x, y) = (x    , y + 1)
      next "L"  (x, y) = (x    , y - 1)
      next "U"  (x, y) = (x - 1, y    )
      next "D"  (x, y) = (x + 1, y    )
      next "RU" (x, y) = (x - 1, y + 1)
      next "RD" (x, y) = (x + 1, y + 1)
      next "LU" (x, y) = (x - 1, y - 1)
      next "LD" (x, y) = (x + 1, y - 1)

printer = hPutStrLn

parser = do
   x <- number
   y <- number
   w <- word
   c <- count 81 (do spaces; digit)
   return ((y, x), w, listArray bnd c)
   where
      bnd = ((1, 1), (9, 9))

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

-- Milib.IO
number' :: (Stream s m Char, Integral a, Read a) => ParsecT s u m a
number' =
       do ds <- many1 digit
          return (read ds)
   <?> "number"

number :: (Stream s m Char, Integral a, Read a) => ParsecT s u m a
number = do spaces; number'

word' :: Stream s m Char => ParsecT s u m String
word' = many1 letter

word :: Stream s m Char => ParsecT s u m String
word = do spaces; word'

-- vim: set expandtab:
