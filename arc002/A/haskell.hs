{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
-- import Milib.Contest
-- import Milib.IO

import IO
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import Data.Functor.Identity
import Control.Monad
import Text.Printf

main = contestMain p solve number

solve y
   | y `mod` 400 == 0 = True
   | y `mod` 100 == 0 = False
   | y `mod`   4 == 0 = True
   | otherwise        = False

p h True  = hPutStrLn h "YES"
p h False = hPutStrLn h "NO"

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
number' :: Stream s m Char => ParsecT s u m Int
number' =
   do  ds <- many1 digit
       return (read ds)
   <?> "number"

number :: Stream s m Char => ParsecT s u m Int
number = do spaces; number'

-- vim: set expandtab:
