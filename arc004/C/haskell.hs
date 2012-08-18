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
import Control.Monad

main = contestMain printer solver parser

solver :: (Integer, Integer) -> [(Integer, Integer)]
solver (x', y') = takeWhile ok $ dropWhile ng $ map f [2*x`div`y`div`y..]
   where
      g = gcd x' y'
      x = div x' g
      y = div y' g
      f d = (n, m)
         where
            n = d * y
            s = n * (n+1) `div` 2
            m = s - d * x
      ok (n, m) = m <= n
      ng (n, m) = m <= 0

printer h []  = hPutStrLn h "Impossible"
printer h ans = mapM_ f ans
   where
      f (x, y) = hPutStrLn h . unwords $ map show [x, y]

parser = do
   x <- number
   char '/'
   y <- number'
   return (x, y)

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
