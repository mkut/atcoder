{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Functor.Identity
import Data.List
import Data.Maybe
import System.IO
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (runParser, Parsec, Stream, (<?>), uncons)
import Text.Printf (hPrintf)

main = contestMain parse solve pr

solve as = fst $ foldl' f (0, []) as
   where
      f (r, []        ) a = (r+1, [a])
      f (r, sss@(s:ss)) a
         | s >  a = f (r, ss) a
         | s == a = (r, sss)
         | s <  a = (r+1, a:sss)

pr = hPrint

parse = parserWithoutError $ \h ->  map (fst . fromJust . BS.readInt) . BS.words . (!!1) . BS.lines <$> BS.hGetContents h

-- Milib.Contest
type Printer b = Handle -> b -> IO ()
type Solver a b = a -> b
type Parser err a = Handle -> IO (Either err a)
type ParsecParser a = Stream BS.ByteString Identity Char => Parsec BS.ByteString () a
type CMain err a b = Parser err a -> Solver a b -> Printer b -> IO ()
type HCMain err a b = Handle -> Handle -> CMain err a b

contestMain :: Show err => CMain err a b
contestMain = hContestMain stdin stdout

hContestMain :: Show err => HCMain err a b
hContestMain hin hout parser solver printer = do
   input <- parser hin
   case input of
      Left err -> do { hPutStr stderr "parse error: "; hPrint stderr err }
      Right x  -> printer hout $ solver x

parserWithoutError :: (Handle -> IO a) -> Parser ParseError a
parserWithoutError f h = Right <$> f h

-- vim: set expandtab:
