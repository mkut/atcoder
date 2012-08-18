{-# LANGUAGE TupleSections #-}
import Data.Array
import Data.List
import Data.Maybe
import IO

main = contestMainOnce put solve parse

solve a = dfs a ix

dfs a is
   | anyhit a      = Nothing
   | length a == 8 = Just a
   | is == []      = Nothing
   | n /= Nothing  = n
   | otherwise     = m
   where
      n = dfs (head is:a) (tail is)
      m = dfs a           (tail is)

hit (x1, y1) (x2, y2) = x1 == x2 || y1 == y2 || abs (x1 - x2) == abs (y1 - y2)
anyhit a = any (uncurry hit) $ combi a

combi []     = []
combi (x:xs) = map (x,) xs ++ combi xs

parse = hfmap (mapMaybe f . zip ix . concat) $ hIOList 8 hGetLine
   where
      f (x,'Q') = Just x
      f _       = Nothing

put h Nothing  = hPutStrLn h "No Answer"
put h (Just x) = hPutStr h . unlines $ map (map (toQ . flip elem x)) ix2

toQ True  = 'Q'
toQ False = '.'

ix = range ((1,1),(8,8))
ix2 = unfoldr f ix
   where
      f [] = Nothing
      f x  = Just (take 8 x, drop 8 x)

-- Milib.IO
hIOList :: Int -> (Handle -> IO a) -> Handle -> IO [a]
hIOList n p h = mapM id $ replicate n (p h)

hfmap :: (Functor f) => (a -> b) -> (Handle -> f a) -> Handle -> f b
hfmap g p h = fmap g (p h)

-- Milib.Contest
type Parser a = Handle -> IO a
type Solver a b = a -> b
type Printer b = Handle -> b -> IO ()
type CMain a b = Printer b -> Solver a b -> Parser a -> IO ()

contestMainOnce :: CMain a b
contestMainOnce = hContestMainOnce stdin stdout

hContestMainOnce :: Handle -> Handle -> CMain a b
hContestMainOnce hin hout printer solver parser = printer hout . solver =<< parser hin

-- vim: set expandtab:
