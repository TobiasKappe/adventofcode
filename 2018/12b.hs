import qualified Data.Set as S
import Text.Regex.Posix
import System.IO (isEOF)
import Data.List


type Rule = ([Bool], Bool)
type Ruleset = S.Set [Bool]
type Plants = S.Set (Int)


neighbours plants position =
    map ((flip S.member) plants) $ map ((+) position) [-2, -1, 0, 1, 2]


alive rules plants position =
    let neighbourhood = neighbours plants position
    in S.member neighbourhood rules


next rules plants =
    let start = (S.findMin plants) - 2 :: Int
        end   = (S.findMax plants) + 2
    in S.fromList $ filter (alive rules plants) [start..end]


countPlants rules initial =
    map (sum) (iterate (next rules) initial)


deltas (x : xs) =
    let next = head xs
    in (next - x) : deltas xs


cluster n xs =
    (take n xs) : (cluster n (tail xs))


uniform xs =
    (length $ S.fromList xs) == 1


parseRule :: String -> Rule

parseRule line =
    let pattern = "(.)(.)(.)(.)(.) => (.)"
        parts = tail $ head (line =~ pattern) :: [String]
        [l1, l2, c, r1, r2, n] = map ((==) "#") parts
    in ([l1, l2, c, r1, r2], n)


parseInitial :: String -> Plants

parseInitial line =
    let pattern = "initial state: (.*)"
        initialString = head $ tail $ head (line =~ pattern) :: String
        initialBools = map ((==) '#') initialString
    in S.fromList $ filter (initialBools !!) [0..length initialBools - 1]
    

readLines = do
    done <- isEOF
    if done
    then return []
    else do
        line <- getLine
        rest <- readLines
        return (line : rest)


main = do
    initialRaw <- getLine
    empty <- getLine
    rulesRaw <- readLines
    let initial = parseInitial initialRaw
        rules = S.fromList $ map (fst) $ filter (snd) $ map (parseRule) rulesRaw
        counts = countPlants rules initial
        Just steady = findIndex (uniform) (cluster 5 (deltas counts))
        before = counts !! steady
        after = counts !! (steady + 1)
        delta = after - before
    print $ before + (50000000000 - steady) * delta
