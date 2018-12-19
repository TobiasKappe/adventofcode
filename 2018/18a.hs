import Data.Map.Strict as M
import Data.List as L
import System.IO (isEOF)
import Data.Maybe (catMaybes)


data Acre = Open | Trees | Lumberyard deriving (Show, Eq)

type Scan = Map (Int, Int) Acre


neighbourhood :: Scan -> (Int, Int) -> [Acre]

neighbourhood scan (x, y) =
    let positions = [(x  , y+1),
                     (x+1, y+1),
                     (x+1, y  ),
                     (x+1, y-1),
                     (x  , y-1),
                     (x-1, y-1),
                     (x-1, y  ),
                     (x-1, y+1)]
        acres = L.map ((flip M.lookup) scan) positions
    in catMaybes acres


count :: Eq a => [a] -> a -> Int

count xs x =
    length $ L.filter (== x) xs


next :: Scan -> (Int, Int) -> Acre -> Acre

next scan (x, y) acre =
    let hood = neighbourhood scan (x, y)
    in case scan M.! (x, y) of
           Open       -> if count hood Trees >= 3
                         then Trees
                         else Open
           Trees      -> if count hood Lumberyard >= 3
                         then Lumberyard
                         else Trees
           Lumberyard -> if count hood Lumberyard >= 1 && 
                            count hood Trees >= 1
                         then Lumberyard
                         else Open


tick :: Scan -> Scan

tick scan =
    M.mapWithKey (next scan) scan


resourceValue scan =
    let acres = M.elems scan
    in (count acres Trees) * (count acres Lumberyard)


readLines = do
    done <- isEOF
    if done
    then return []
    else do
        line <- getLine
        rest <- readLines
        return (line : rest)


parseLine :: Int -> Scan -> String -> Scan

parseLine row scan line =
    parseChars' row 0 scan line
    where parseChars' row column scanned [] = scanned
          parseChars' row column scanned (char : chars) =
              let acre = case char of
                             '.' -> Open
                             '|' -> Trees
                             '#' -> Lumberyard
                  scan = M.insert (column, row) acre scanned
              in parseChars' row (column+1) scan chars


parseLines :: [String] -> Scan

parseLines lines =
    parseLines' 0 M.empty lines
    where parseLines' row scanned [] = scanned
          parseLines' row scanned (line : lines) =
              let scan = parseLine row scanned line
              in parseLines' (row+1) scan lines
    


main = do
    lines <- readLines
    let scan = parseLines lines
        after10 = iterate (tick) scan !! 10
    print $ resourceValue after10
