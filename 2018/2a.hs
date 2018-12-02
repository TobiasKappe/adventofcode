import System.IO (isEOF)
import qualified Data.Map


readLines = do
    done <- isEOF
    if done
    then return []
    else do
        line <- getLine
        rest <- readLines
        return (line : rest)


repetitionsIn [] = Data.Map.empty

repetitionsIn (x: xs) =
    let repMapOld = repetitionsIn xs
        repMapNew = Data.Map.insertWith (+) x 1 repMapOld
    in repMapNew


repeatsExactly n repMap =
    let matching = Data.Map.filter (== n) repMap
    in matching /= Data.Map.empty


main = do 
    ids <- readLines
    let repMaps = map repetitionsIn ids
        repeatsTwice = length $ filter (repeatsExactly 2) repMaps
        repeatsThrice = length $ filter (repeatsExactly 3) repMaps
    print $ repeatsTwice * repeatsThrice
