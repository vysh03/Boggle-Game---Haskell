module Boggle (boggle) where

{--
    Fill in the boggle function below. Use as many helpers as you want.
    Test your code by running 'cabal test' from the tester_hs_simple directory.
--}


import Data.List
import qualified Data.Map.Strict as Map

type Grid = [String]
type Coord = (Int, Int)
type Path = [Coord]
    
boggle :: [String] -> [String] -> [ (String, [ (Int, Int) ] ) ]
boggle grid words = Map.toList $ foldl (searchWord grid) Map.empty words


pair :: [Coord]
pair = [(0,-1),(-1,0),(0,1),(1,0),(1,1),(1,-1),(-1,1),(-1,-1)]

searchWord :: Grid -> Map.Map String Path -> String -> Map.Map String Path
searchWord board acc word = 
   -- if no paths are found, return the accumulator
   -- otherwise, update acc with the word and its path
    if null paths 
        then acc
        else Map.insert word (pathOfFirstOccurrence paths) acc
  where
    -- get all possible starting pos on the board
    startingPositions = [(a, b) | a <- [0..boardHeight-1], b <- [0..boardWidth-1]]
    -- Find paths for the word starting from each position
    paths = concat [dfsAlgo board word word [] startPos boardHeight boardWidth | startPos <- startingPositions]
    -- height and width of board
    boardHeight = length board 
    boardWidth = if boardHeight > 0 then length (head board) else 0
    -- getthe path from the first occurrence tuple
    pathOfFirstOccurrence [] = [] -- no paths are found, return an empty path
    pathOfFirstOccurrence (x:_) = snd x -- get the path from the first occurrence tuple



dfsAlgo:: Grid -> String -> String -> Path -> Coord -> Int -> Int -> [(String, Path)]
dfsAlgo board word (hd:tl) path (x,y) a b 
    | outOfBounds = []
    | visitedBefore = []
    | notMatching = []
    | null tl = [(word, reverse ((x,y):path))]
    | otherwise = concatMap explore pair
    where
        outOfBounds = x < 0 || y < 0 || x >= a || y >= b
        visitedBefore = (x, y) `elem` path
        notMatching = board !! x !! y /= hd
        explore (dx, dy) = dfsAlgo board word tl ((x,y):path) (x+dx, y+dy) a b
    
