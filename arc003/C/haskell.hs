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
import Data.Graph.Inductive.Tree
import qualified Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.Graph (Node, Path, Graph, lsuc, lab)
import qualified Data.Graph.Inductive.Internal.Heap as H
import Data.Graph.Inductive.Internal.Heap (Heap, build)
import qualified Data.Map as M
import Data.Map (Map)

import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Printf

import System.IO
import Control.Monad
import Control.Monad.ST

main = contestMain printer solver parser

type Point = (Int, Int)
data Maze = Maze
   { height :: Int
   , width  :: Int
   , start  :: Point
   , goal   :: Point
   , light  :: [Int]
   }

pointToNode :: Maze -> Point -> Int
pointToNode maze (x, y) = (x-1) * width maze + (y-1) + 1

solver maze = case dijkstra gr starts f gf of
   Nothing     -> -1
   Just (x, _) -> -x
   where
      gr = mkGraph (height maze) (width maze) (light maze) (\x y -> ()) :: Gr Int ()
      starts = [(-10.0, pointToNode maze $ goal maze)]
      gf = (==(pointToNode maze $ start maze))
      f x y z = max (z * 0.99) (fromIntegral (-x))

printer h ans
   | ans < 0   = hPrint h (-1)
   | otherwise = hPrint h ans

parser = do
   n <- number
   m <- number
   v' <- count n (do spaces; count m $ letter <|> digit <|> char '#')
   let v = concat v'
       light = map g v
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
      , light  = light
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

-- Milib.Data.Graph.Grid
mkGraph :: (G.Graph gr, Show b) => Int -> Int -> [a] -> (G.Node -> G.Node -> b) -> gr a b
mkGraph n m as f = G.mkGraph ns es
   where
      ns = zip [1..n*m] as
      es = [ (i, i', f i i')
           | i <- [1..n*m], let (x, y) = fromId i
           , (dx, dy) <- ds, let x' = x + dx, let y' = y + dy
           , x' >= 1, x' <= n, y' >= 1, y' <= m
           , let i' = toId x' y'
           ]
      fromId i = ((i-1) `div` m + 1, (i-1) `mod` m + 1)
      toId x y = (x-1) * m + (y-1) + 1
      ds = [(1, 0), (-1, 0), (0, 1), (0, -1)]

-- Milib.Algorithm.Dijkstra
dijkstra :: (Graph gr, Ord c) => gr a b -> [(c, Node)] -> (a -> b -> c -> c) -> (Node -> Bool) -> Maybe (c, Path)
dijkstra gr s f gf = runST $ do
   initHeap <- newListHeap $ map g s
   visited <- newMap
   dijkstra' gr f gf initHeap visited
   where
      g (a, b) = (a, (Nothing, b))

dijkstra' :: (Graph gr, Ord c) => gr a b -> (a -> b -> c -> c) -> (Node -> Bool) -> STHeap s c (Maybe Node, Node) -> STMap s Node (Maybe Node) -> ST s (Maybe (c, Path))
dijkstra' gr f gf h s = do
   emptyFlag <- isEmptyHeap h
   if emptyFlag then return Nothing else do
      (cost, (prev, node)) <- splitMinHeap h
      visited <- memberMap node s
      if visited then dijkstra' gr f gf h s else 
         if gf node then return (Just (cost, [])) else do
            insertMap node prev s 
            forM_ (lsuc gr node) $ \(n, b) -> do
               visited' <- memberMap n s
               let a = fromJust $ lab gr n
               if visited' then return () else
                  insertHeap (f a b cost, (Just node, n)) h
            dijkstra' gr f gf h s

-- Milib.Data.Heap.ST
type STHeap s a b = STRef s (Heap a b)

newListHeap :: Ord a => [(a, b)] -> ST s (STHeap s a b)
newListHeap = newSTRef . build

insertHeap :: Ord a => (a, b) -> STHeap s a b -> ST s ()
insertHeap = liftSet H.insert

isEmptyHeap :: Ord a => STHeap s a b -> ST s Bool
isEmptyHeap = liftGet0 H.isEmpty

splitMinHeap :: Ord a => STHeap s a b -> ST s (a, b)
splitMinHeap = lift0 f
   where
      f h = let (a, b, h') = H.splitMin h in ((a, b), h')

-- Milib.Data.Map.ST
type STMap s a b = STRef s (Map a b)

newMap :: Ord a => ST s (STMap s a b)
newMap = newSTRef M.empty

insertMap :: Ord a => a -> b -> STMap s a b -> ST s ()
insertMap x y = liftSet (uncurry M.insert) (x, y)

memberMap :: Ord a => a -> STMap s a b -> ST s Bool
memberMap = liftGet M.member

-- Milib.Data.STRef
liftSet :: (b -> a -> a) -> b -> STRef s a -> ST s ()
liftSet f x y' = do
   y <- readSTRef y'
   writeSTRef y' $! f x y

liftGet :: (b -> a -> c) -> b -> STRef s a -> ST s c
liftGet f x y' = do
   y <- readSTRef y'
   return $! f x y

lift0 :: (a -> (c, a)) -> STRef s a -> ST s c
lift0 f y' = do
   y <- readSTRef y'
   let (ret, ny) = id $! f y
   writeSTRef y' ny
   return ret

liftSet0 :: (a -> a) -> STRef s a -> ST s ()
liftSet0 f y' = do
   y <- readSTRef y'
   writeSTRef y' $! f y

liftGet0 :: (a -> c) -> STRef s a -> ST s c
liftGet0 f y' = do
   y <- readSTRef y'
   return $! f y

-- vim: set expandtab:
