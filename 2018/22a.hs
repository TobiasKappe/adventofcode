import qualified Data.Map.Strict as M
import Text.Regex.Posix


erosionLevel depth n = (n + depth) `mod` 20183


regionType depth n = (erosionLevel depth n) `mod` 3


growIndices depth target =
    growIndices' M.empty target
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
        target = parseTarget rawTarget
        indices = growIndices depth target
        erosionLevels = M.map (regionType depth) indices
        riskLevel = sum $ M.elems $ erosionLevels
    print $ riskLevel
                 
