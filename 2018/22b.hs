import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.HashPSQ as P
import Data.Hashable
import Text.Regex.Posix


data Tool = Gear | Torch | Neither deriving (Eq, Ord, Show)

data Terrain = Rocky | Wet | Narrow


instance Hashable Tool where
    hashWithSalt s t = let numeric = case t of
                                         Gear -> 0 :: Int
                                         Torch -> 1
                                         Neither -> 2
                       in hashWithSalt s numeric


tools = [Gear, Torch, Neither]


erosionLevel depth n = (n + depth) `mod` 20183


regionType depth indices pos =
    let index = indices M.! pos
    in case (erosionLevel depth index) `mod` 3 of
           0 -> Rocky
           1 -> Wet
           2 -> Narrow



growIndices indices depth target (x, y) =
    growIndices'' indices (x+1, y+1)
    where growIndices' ind (x, y)
              | x == 0 && y == 0 = M.insert (x, y) 0 ind
              | x == 0           = let ind' = growIndices'' ind (x, y-1)
                                   in M.insert (x, y) (y * 48271) ind'
              |           y == 0 = let ind' = growIndices'' ind (x-1, y)
                                   in M.insert (x, y) (x * 16807) ind'
              | otherwise        = let ind' = growIndices'' ind (x-1, y)
                                       ind'' = growIndices'' ind' (x, y-1)
                                   in if (x, y) == target
                                      then M.insert (x, y) 0 ind''
                                      else let idxA = ind' M.! (x-1, y)
                                               idxB = ind'' M.! (x, y-1)
                                               lvlA = erosionLevel depth idxA
                                               lvlB = erosionLevel depth idxB
                                               finalLevel = lvlA * lvlB
                                           in M.insert (x, y) finalLevel ind''

          growIndices'' indices (x, y) =
              if M.member (x, y) indices
              then indices
              else growIndices' indices (x, y)


compatible Rocky Neither = False
compatible Wet Torch = False
compatible Narrow Gear = False
compatible _ _ = True


singleStep (x, y) = 
    let candidates = [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]
    in filter (withinGrid) candidates
       where withinGrid (x, y) = x >= 0 && y >= 0


neighbourhood depth indices (pos, tool) =
    let reachable = singleStep pos
        adjacent = [(pos', tool) | pos' <- reachable] ++
                   [(pos, tool') | tool' <- tools, tool' /= tool]
    in filter (isAllowed) adjacent
       where isAllowed (pos', tool') =
                 let region = regionType depth indices pos'
                 in compatible region tool'


manhattan (x1, y1) (x2, y2) =
    (abs (x1 - x2)) + (abs (y1 - y2))


heuristic (pos1, tool1) (pos2, tool2) = 
    let distanceCost = manhattan pos1 pos2
        switchCost = if tool1 == tool2 then 0 else 7
    in distanceCost + switchCost


cost (pos1, tool1) (pos2, tool2) = 
    if tool1 == tool2 then 1 else 7


minimalCost depth origin target =
    let queue = P.singleton origin (heuristic origin target) 0
    in minimalCost' M.empty S.empty queue
       where minimalCost' indices seen queue =
                 let Just (pivot, heuristic, distance) = P.findMin queue
                 in if target == pivot
                    then distance
                    else let newSeen = S.insert pivot seen
                             poppedQueue = P.deleteMin queue
                             (tpos, ppos) = (fst target, fst pivot)
                             newIndices = growIndices indices depth tpos ppos
                             hood = neighbourhood depth newIndices pivot
                             folder = process newSeen pivot distance
                             newQueue = foldl (folder) poppedQueue hood
                         in minimalCost' newIndices newSeen newQueue

             process seen pivot distance queue next =
                 let newDistance = distance + (cost pivot next)
                     newHeuristic = newDistance + heuristic next target
                     newQueue = P.insert next newHeuristic newDistance queue
                 in if S.member next seen
                    then queue
                    else case P.lookup next queue of
                             Nothing -> newQueue
                             Just (_, currentDistance) ->
                                 if newDistance < currentDistance
                                 then newQueue
                                 else queue


parseDepth :: String -> Int

parseDepth line =
    let pattern = "depth: (.*)"
        parts = tail $ head (line =~ pattern) :: [String]
        [depth] = parts
    in (read depth)


parseTarget :: String -> (Int, Int)

parseTarget line =
    let pattern = "target: (.*),(.*)"
        parts = tail $ head (line =~ pattern) :: [String]
        [width,height] = parts
    in (read width, read height)


main = do
    rawDepth <- getLine
    rawTarget <- getLine
    let depth = parseDepth rawDepth
        (x, y) = parseTarget rawTarget
    print $ minimalCost depth ((0, 0), Torch) ((x, y), Torch)

