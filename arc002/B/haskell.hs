{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
import System.IO
import Data.Time.Calendar
import Data.List
import Data.Maybe
import Text.Printf

-- lib imports
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import Data.Functor.Identity
import Control.Monad

-- Main
main = contestMain printer solver parser

solver day = fromJust . find f $ days
   where
      days = day : map (addDays 1) days
      f x  = y `mod` fromIntegral (m * d) == 0
         where
            (y, m, d) = toGregorian x

printer h day = hPrintf h "%04d/%02d/%02d\n" y m d
   where
      (y, m, d) = toGregorian day

parser = do
   spaces
   y <- number'
   char '/'
   m <- number'
   char '/'
   d <- number'
   return $ fromGregorian y m d

-- Milib.IO

number' :: (Stream s m Char, Integral a, Read a) => ParsecT s u m a
number' =
       do ds <- many1 digit
          return (read ds)
   <?> "number"

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
