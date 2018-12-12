import qualified Data.Set as S
import Text.Regex.Posix
import System.IO (isEOF)


type Ruleset = S.Set [Bool]
type Plants = S.Set (Int)


neighbours plants position =
    map ((flip S.member) plants) $ map ((+) position) [-2, -1, 0, 1, 2]


alive :: Ruleset -> Plants -> Int -> Bool

alive rules plants position =
    let neighbourhood = neighbours plants position
    in S.member neighbourhood rules


next :: Ruleset -> Plants -> Plants

next rules plants =
    let start = (S.findMin plants) - 2 :: Int
        end   = (S.findMax plants) + 2
    in S.fromList $ filter (alive rules plants) [start..end]


countPlants rules initial =
    map (sum) (iterate (next rules) initial)


parseRule :: String -> ([Bool], Bool)

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
        final = (countPlants rules initial) !! 20
    print $ final
