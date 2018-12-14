import Data.Sequence as S
import Data.List as L
import Data.Foldable as F

data Board = Board Int Int (S.Seq Int) deriving Show


digits 0 = S.empty

digits num =
    let rest = digits (num `div` 10)
    in rest |> (num `mod` 10)


recipes score1 score2 =
    let total = score1 + score2
    in if total == 0
       then S.singleton 0
       else digits total


next (Board elf1 elf2 scores) =
    let Just score1 = S.lookup elf1 scores
        Just score2 = S.lookup elf2 scores
        newRecipes = recipes score1 score2
        newScores = scores >< newRecipes
        newElf1 = (elf1 + score1 + 1) `mod` (S.length newScores)
        newElf2 = (elf2 + score2 + 1) `mod` (S.length newScores)
    in Board newElf1 newElf2 newScores


window = 10


suffices input (Board _ _ scores) =
    (S.length scores) >= (input + window)


main = do
    let board = Board 0 1 (3 <| 7 <| S.empty)
        input = 190221
        boards = iterate (next) board
        Just sufficient = L.find (suffices input) boards
        Board _ _ scores = sufficient
        finalScores = S.take window $ S.drop input scores
    print $ intercalate "" $ map (show) $ F.toList finalScores
