import Data.Sequence
import qualified Data.Map.Strict as M


rotateClockwise Data.Sequence.Empty =
    Data.Sequence.Empty


rotateClockwise (marbles :|> marble) =
    marble <| marbles


rotateCounterclockwise Data.Sequence.Empty =
    Data.Sequence.empty


rotateCounterclockwise (marble :<| marbles) =
    marbles |> marble


step value marbles =
    if value `mod` 23 == 0
    then let tmpMarbles :|> remove = iterate (rotateClockwise) marbles !! 7
             newMarbles = rotateCounterclockwise tmpMarbles
             gain = remove + value
         in (gain, newMarbles)
    else let shifted = rotateCounterclockwise marbles
         in (0, shifted |> value)



simulate players player steps =
    case steps of
        0 -> (M.empty, Data.Sequence.singleton 0)
        n -> let previousPlayer = (player - 1) `mod` players
                 (scores, marbles) = simulate players previousPlayer (n-1)
                 (gain, newMarbles) = step n marbles
                 newScores = M.insertWith (+) player gain scores
             in (newScores, newMarbles)


highscore players steps =
    let (scores, marbles) = simulate players 0 steps
    in maximum $ M.elems scores


main = do
    print $ highscore 411 7205900
