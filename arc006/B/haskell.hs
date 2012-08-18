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

solver (amida, s) =
   foldl move s amida'
   where
      l = length $ head amida
      amida' = reverse amida
      move p line
         | p-2 >= 0 && line !! (p-2) == '-' = p - 1
         | p-1 <  l && line !! (p-1) == '-' = p + 1
         | otherwise                        = p

printer = hPrint

parser = do
   n <- number
   l <- number
   amida <- count l (line n)
   newline2
   s <- circle
   return (amida, s)

line n = do
   spaces
   char '|'
   count (n-1) line'

line' = do
   ret <- try (char ' ') <|> char '-'
   char '|'
   return ret

circle =   try (do { char 'o'; return 1})
       <|> do
          count 2 $ char ' '
          s <- circle
          return (s+1)

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

newline2 :: Stream s m Char => ParsecT s u m String
newline2 = do
   cr <- optionMaybe $ char '\r'
   case cr of
      Nothing -> do
         lf <- newline
         return [lf]
      Just cr' -> do
         lf <- newline
         return [cr', lf]

-- vim: set expandtab:
