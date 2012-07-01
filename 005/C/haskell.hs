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
import Data.Array
import Data.Array.ST
import Data.Maybe
import Control.Monad
import Control.Monad.ST

main = contestMain printer solver parser

type Point = (Int, Int)
type Border = (Point, Point)

solver g = f . d . d . d $ g
   where
      d = flip draw s
      s = fst . fromJust . find ((=='s') . snd) $ assocs g
      f = not . elem 'g' . elems

draw :: Array Point Char -> Point -> Array Point Char
draw g p = listArray (bounds g) . map f . elems $ runSTArray $ do
   init <- newListArray (bounds g) (elems g)
   draw' init (bounds g) p
   where
      f '*' = '.'
      f c   = c

draw' :: STArray s Point Char -> Border -> Point -> ST s (STArray s Point Char)
draw' g b p
   | outOfRange b p = return g
   | otherwise      = do
      c <- readArray g p
      case c of
         '*' -> return g
         '#' -> do
            writeArray g p '*'
            return g
         _   -> do
            writeArray g p '*'
            mapM_ (draw' g b) nexts
            return g
   where
      (x, y) = p
      nexts  = [(x, y+1), (x, y-1), (x+1, y), (x-1, y)]
      outOfRange ((a, b), (c, d)) (x, y)
         = x < a || x > c || y < b || y > d

printer h True  = hPutStrLn h "YES"
printer h False = hPutStrLn h "NO"

parser = do
   h <- number
   w <- number
   c <- count (h*w) (do spaces; anyChar)
   return $ listArray ((1,1),(h,w)) c

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
