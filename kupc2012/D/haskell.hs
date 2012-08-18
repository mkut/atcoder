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
import Data.List
import Data.Ord
import Control.Monad

main = contestMain printer solver parser

solver (n, a)
   | pfs == []     = Nothing
   | last pfs == n = Just (length pfs)
   | otherwise     = Nothing
   where
      pfs = unfoldr next 1
      a' = sortBy (flip $ comparing snd) a
      next i = liftM (f . snd) $ find ((<=i) . fst) $ takeWhile ((>=i) . snd) a'
      f x = (x, x + 1)

printer h Nothing  = hPutStrLn h "Impossible"
printer h (Just x) = hPrint h x

parser = do
   n <- number
   m <- number
   a <- count m $ liftM2 (,) number number
   return (n, a)

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

-- vim: set expandtab:
