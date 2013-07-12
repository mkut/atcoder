{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Functor.Identity
import Data.List
import System.IO
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Printf

main = contestMain printer solver parser

solver (n, x, m, lrs) = case filter f a of
   [] -> Nothing
   qs -> Just $ take n $ maximumBy h qs
   where
      f as = all g lrs
         where
            g (l, r, s) = (sum $ take (r-l+1) $ drop (l-1) as) == s
      h as bs = compare (sum as) (sum bs)
      a = [ [a1, a2, a3, a4, a5, a6] | a1 <- [0..x], a2 <- [0..x], a3 <- [0..x], a4 <- [0..x], a5 <- [0..x], a6 <- [0..x] ]

printer h Nothing    = hPrint h (-1)
printer h (Just ans) = hPutStrLn h $ unwords $ map show ans

parser = do
   n <- number
   x <- number
   m <- number
   lrs <- count m $ liftM3 (,,) number number number
   return (n, x, m, lrs)

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
