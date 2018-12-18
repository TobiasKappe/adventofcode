import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M
import System.IO (isEOF)
import Text.Regex.Posix


data Direction = West | East | Down

data Pos = Pos Int Int deriving (Eq, Ord, Show)

type Walls = S.Set Pos


move :: Direction -> Pos -> Pos

move East (Pos x y) = Pos (x+1) y
move West (Pos x y) = Pos (x-1) y
move Down (Pos x y) = Pos x (y+1)


depth :: Pos -> Int

depth (Pos x y) = y


blocked visited pos =
    blocked' visited West pos && blocked' visited East pos
    where blocked' visited direction pos =
             case M.lookup pos visited of
                 Just isBlocked -> isBlocked && blocked' visited direction (move direction pos)
                 Nothing -> True


fillVer :: Walls -> Int -> M.Map Pos Bool -> Pos -> (Bool, M.Map Pos Bool)

fillVer walls bottom visited pos =
    if depth pos > bottom
    then (False, visited)
    else if S.member pos walls
         then (True, visited)
         else if M.member pos visited
              then (blocked visited pos, visited)
              else let (sVer, vVer) = fillVer walls bottom visited (move Down pos)
                   in if sVer
                      then let (sWest, vWest) = fillHor walls bottom vVer (move West pos) West
                               (sEast, vEast) = fillHor walls bottom vWest (move East pos) East
                           in (sWest && sEast, M.insert pos (sWest && sEast) vEast)
                      else (False, M.insert pos False vVer)


fillHor :: Walls -> Int -> M.Map Pos Bool -> Pos -> Direction -> (Bool, M.Map Pos Bool)

fillHor walls bottom visited pos direction =
    if S.member pos walls
    then (True, visited)
    else let (sVer, vVer) = fillVer walls bottom visited (move Down pos)
         in if sVer
            then let (sHor, vHor) = fillHor walls bottom vVer (move direction pos) direction
                 in (sHor, M.insert pos sHor vHor) 
            else (False, M.insert pos False vVer)


fill :: Walls -> S.Set Pos

fill walls = 
    let ys = S.map (\(Pos x y) -> y) walls
        top = S.findMin ys
        bottom = S.findMax ys
        blockedMap = snd $ fillVer walls bottom M.empty (Pos 500 0)
        filledPositions = S.filter (\p -> depth p >= top) $ S.fromAscList $ M.keys blockedMap
        remains = S.filter (blocked blockedMap) filledPositions
    in remains


parseLine :: String -> S.Set Pos

parseLine line = 
    let pattern = "(.)=(.*), (.)=(.*)\\.\\.(.*)"
        parts = tail $ head (line =~ pattern) :: [String]
        [minor, fixedRaw, major, startRaw, endRaw] = parts
        [fixed, start, end] = map (read) [fixedRaw, startRaw, endRaw] :: [Int]
    in case minor of
           "x" -> S.fromAscList [Pos fixed i | i <- [start..end]]
           "y" -> S.fromAscList [Pos i fixed | i <- [start..end]]


parseLines :: [String] -> S.Set Pos

parseLines lines =
    let parsedLines = map (parseLine) lines
    in foldl (S.union) S.empty parsedLines


readLines = do
    done <- isEOF
    if done
    then return []
    else do
        line <- getLine
        rest <- readLines
        return (line : rest)


main = do
    lines <- readLines
    let walls = parseLines lines
    print $ length $ fill walls
