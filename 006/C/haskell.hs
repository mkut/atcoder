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
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad

main = contestMain printer solver parser

solver w = Set.size $ foldl' f Set.empty w
   where
      f s x = case lookupGE x s of
         Nothing -> Set.insert x s
         Just y  -> Set.insert x (Set.delete y s)

printer = hPrint

parser = do
   n <- number
   count n number

lookupGE :: Ord a => a -> Set a -> Maybe a
-- lookupGE = Set.lookupGE
lookupGE x s = case Set.splitMember x s of
   (_, True,  _) -> Just x
   (_, False, b) -> if Set.null b then Nothing else Just $ Set.findMin b

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
