{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
import qualified Data.ByteString.Lazy.Char8 as C
import System.Random
import System.IO
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Printf
import Data.List
import Data.Array
import Data.Array.IO
import Data.Function
import Data.Functor.Identity
import Control.Monad

main = contestMain printer solver parser
-- main = contestMain hPrint solver parser

solver m = shuffle . take t . cycle . concat . map (uncurry replicate) . flip zip [1..] . map (round . (*fromIntegral t)) $ m'
   -- map (round . (*fromIntegral t)) m'
   where
      m' = foldl' f v (replicate k m)
      t = 1000
      k = 1000
      n = fst . snd $ bounds m
      v = [ if rank i == maxrank then 1.0 else 0.0 | i <- [1..n] ]
      maxrank = maximum $ map rank [1..n]
      rank i = sum [ m ! (i, j) | j <- [1..n] ]
      f x y = let z = y <> x in map (/sum z) z

printer h = (>>= hPutStr h) . liftM (unlines . map show)

parser = do
   n <- number
   liftM (listArray ((1,1),(n,n))) $ count (n*n) (do spaces; liftM cnv anyChar)
   where
      cnv '-' = 1.0
      cnv 'o' = 3.0
      cnv 'x' = 0.0 :: Double

(<>) :: Array (Int, Int) Double -> [Double] -> [Double]
(<>) x y = [ sum $ zipWith (*) (row i) y | i <- [1..n] ]
   where
      n = fst . snd $ bounds x
      row i = [ x ! (i, j) | j <- [1..n] ]

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

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
