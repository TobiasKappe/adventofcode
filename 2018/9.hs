import Data.Sequence
import qualified Data.Map.Strict as M
import Text.Regex.Posix


rotateClockwise Data.Sequence.Empty =
    Data.Sequence.Empty


rotateClockwise (marbles :|> marble) =
    marble <| marbles


rotateCounterclockwise Data.Sequence.Empty =
    Data.Sequence.empty


rotateCounterclockwise (marble :<| marbles) =
    marbles |> marble


next value Data.Sequence.Empty = (0, Data.Sequence.singleton 0)

next value marbles =
    if value `mod` 23 == 0
    then let tmpMarbles :|> remove = iterate (rotateClockwise) marbles !! 7
             newMarbles = rotateCounterclockwise tmpMarbles
             gain = remove + value
         in (gain, newMarbles)
    else let shifted = rotateCounterclockwise marbles
         in (0, shifted |> value)


simulate marbles players [] = M.empty

simulate marbles players (value : values) =
    let (gain, newMarbles) = next value marbles
        player = value `mod` players
        scores = simulate newMarbles players values
    in M.insertWith (+) player gain scores


highscore players final =
    let scores = simulate Data.Sequence.Empty players [0..final]
    in maximum $ M.elems scores


parseLine :: String -> (Int, Int)

parseLine line =
    let pattern = "(.*) players; last marble is worth (.*) points"
        parts = tail $ head (line =~ pattern) :: [String]
        [first, next] = parts
    in (read first, read next)


main = do
    interact (unlines . map (show . (uncurry highscore) . parseLine) . lines)
