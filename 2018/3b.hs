import System.IO (isEOF)
import Text.Regex.Posix
import Data.List

readLines = do
    done <- isEOF
    if done
    then return []
    else do
        line <- getLine
        rest <- readLines
        return (line : rest)


parseLine :: String -> (Int, Int, Int, Int, Int)
parseLine line =
    let numbersWrapped = (line =~ "[0-9]+") :: [[String]]
        numbersAsStrings = map head numbersWrapped
        numbers = map read numbersAsStrings :: [Int]
        [id, x, y, width, height] = numbers
    in (id, x, y, width, height)


containedIn (x, y) (width, height) (px, py) =
    x <= px && px < x + width && y <= py && py < y + height


overlaps :: (Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int) -> Bool
overlaps (id1, x1, y1, width1, height1) (id2, x2, y2, width2, height2) =
    let topLeft = (maximum [x1, x2], maximum [y1, y2])
        bottomRight = (minimum [x1 + width1 - 1, x2 + width2 - 1], minimum [y1 + height1 - 1, y2 + height2 -1])
    in (fst topLeft) <= (fst bottomRight) &&
       (snd topLeft) <= (snd bottomRight)


sovereign (box : boxes) others =
    let found = find (\s -> s /= box && overlaps box s) others
    in case found of
        Just spoiler -> sovereign boxes others
        Nothing -> box


main = do
    linesAsStrings <- readLines
    let boxes = map parseLine linesAsStrings
        (id, _, _, _, _) = sovereign boxes boxes
    print id
