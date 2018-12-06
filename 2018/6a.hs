import Data.Maybe (catMaybes)
import System.IO (isEOF)
import Text.Regex.Posix
import qualified Data.Map
import qualified Data.Set


manhattanDistance (x1, y1) (x2, y2) =
    abs (x1 - x2) + abs (y1 - y2)


maximaOn conv [x] = [x]

maximaOn conv (x : xs) =
    let (first : maxima) = maximaOn conv xs
        local = conv x
        opt = conv first
    in case compare local opt of
       LT -> (first : maxima)
       EQ -> (x : first : maxima)
       GT -> [x]


closest dist points cell = 
    case maximaOn (negate . (dist cell)) points of
        [] -> Nothing
        [x] -> Just x
        (x : xs) -> Nothing


neighbourhoodSize [] points =
    Data.Map.fromList [(point, 0) | point <- points]

neighbourhoodSize (cell : cells) points =
    let otherNeighbours = neighbourhoodSize cells points
    in case closest (manhattanDistance) points cell of
            Nothing -> otherNeighbours
            Just winner -> Data.Map.adjust (1 +) winner otherNeighbours


borders width height =
    let west = [(-1,i) | i <- [-1..(height+1)]]
        east = [(width+1,i) | i <- [-1..(height+1)]]
        north = [(i,-1) | i <- [-1..(width+1)]]
        south = [(i,height+1) | i <- [-1..(width+1)]]
    in west ++ east ++ north ++ south


extrema width height points =
    let right = maximum $ map (fst) points
        bottom = maximum $ map (snd) points
        borderPoints = borders width height
        found = map (closest manhattanDistance points) borderPoints
    in Data.Set.elems $ Data.Set.fromList $ catMaybes found


censor forbidden values =
    Data.Map.filterWithKey (\k v -> not $ elem k forbidden) values


readLines = do
    done <- isEOF
    if done
    then return []
    else do
        line <- getLine
        rest <- readLines
        return (line : rest)


parseLine :: String -> (Int, Int)

parseLine line =
    let [[x], [y]] = line =~ "[0-9]+"
    in (read x, read y)


main = do
    lines <- readLines
    let points = map parseLine lines
        width = maximum $ map (fst) points
        height = maximum $ map (snd) points
        cells = [(i, j) | i <- [0..width], j <- [0..height]]
        closestCounts = neighbourhoodSize cells points
        infiniteArea = extrema width height points
        withoutOutliers = censor infiniteArea closestCounts
        safest = maximum $ Data.Map.elems $ withoutOutliers
    print $ safest

