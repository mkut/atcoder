{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Array.ST
import Data.STRef
import Data.Char
import Data.Maybe
import Data.Array
import Data.List
import Data.Functor.Identity

import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Printf

import System.IO
import Control.Monad
import Control.Monad.ST
import Control.Monad

type Point = (Int, Int)
data Maze = Maze
   { height :: Int
   , width  :: Int
   , start  :: Point
   , goal   :: Point
   , light  :: Array Point Int
   }

main = contestMain printer solver parser

solver maze = solver' maze (-1) 10

solver' maze a b
   | fin       = c
   | ok        = solver' maze c b
   | otherwise = solver' maze a c
   where
      ok  = reachable maze c
      fin = b - a < 1e-10
      c   = (a + b) / 2

reachable :: Maze -> Double -> Bool
reachable maze u = runST $ do
   visited <- newArray (bounds $ light maze) False
   reachable' maze u 1 [start maze] visited

reachable' :: Maze -> Double -> Int -> [Point] -> STArray s Point Bool -> ST s Bool
reachable' maze u t ps d = do
   ok' <- newSTRef False
   let g p = do
          visited <- readArray d p
          writeArray d p True
          ok <- readSTRef ok'
          writeSTRef ok' (ok || p == goal maze)
          return (not visited)
   nextps <- filterM g (concatMap f ps)
   ok <- readSTRef ok'
   case (ok, ps == []) of
      (True, _) -> return True
      (_, True) -> return False
      otherwise -> reachable' maze u (t+1) nextps d
   where
      f (x, y) = filter f' [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
      f' (x, y) =  x >= 1
                && x <= height maze
                && y >= 1
                && y <= width maze
                && l * 0.99 ^ t >= u
         where
            l = fromIntegral $ light maze ! (x, y)

printer h ans
   | ans < 0   = hPrint h (-1)
   | otherwise = hPrint h ans

parser = do
   n <- number
   m <- number
   v' <- count n (do spaces; count m $ letter <|> digit <|> char '#')
   let v = concat v'
       a = listArray ((1,1),(n,m)) $ map g v
       start = f 's'
       goal = f 'g'
       f c = (x `div` m + 1, x `mod` m + 1)
         where
            x = fromJust $ findIndex (==c) v
       g '#' = -1
       g 's' = 10
       g 'g' = 10
       g c   = digitToInt c
   return $ Maze
      { height = n
      , width  = m
      , start  = start
      , goal   = goal
      , light  = a
      }

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
