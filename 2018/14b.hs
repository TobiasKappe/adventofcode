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


streamScores (Board elf1 elf2 scores) =
    let Just score1 = S.lookup elf1 scores
        Just score2 = S.lookup elf2 scores
        newRecipes = recipes score1 score2
        newScores = scores >< newRecipes
        newElf1 = (elf1 + score1 + 1) `mod` (S.length newScores)
        newElf2 = (elf2 + score2 + 1) `mod` (S.length newScores)
    in (F.toList newRecipes) ++ streamScores (Board newElf1 newElf2 newScores)


buffers :: Int -> [a] -> [[a]]

buffers n xs =
    (L.take n xs) : (buffers n (tail xs))


main = do
    let initialScores = [3,7]
        board = Board 0 1 (S.fromList initialScores)
        input = [1, 9, 0, 2, 2, 1]
        scores = initialScores ++ streamScores board
        scoreBuffers = buffers (L.length input) scores
        Just index = L.elemIndex input scoreBuffers
    print $ index
