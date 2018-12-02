import System.IO (isEOF)


readLines = do
    done <- isEOF
    if done
    then return []
    else do
        line <- getLine
        rest <- readLines
        return (line : rest)


hammingDistance [] [] = 0

hammingDistance (x : xs) (y : ys) =
    let rest = hammingDistance xs ys
        local = if x == y then 0 else 1
    in local + rest


unitDistance xs ys =
    hammingDistance xs ys == 1


pairs [] = []

pairs (x : xs) =
    let firstWithOthers = [(x, other) | other <- xs]
    in firstWithOthers ++ pairs xs


commonPositions [] [] = []

commonPositions (x : xs) (y : ys) =
    let rest = commonPositions xs ys
        local = if x == y then [x] else []
    in local ++ rest


main = do 
    ids <- readLines
    let pairsOfIds = pairs ids
        closePairs = filter (uncurry unitDistance) pairsOfIds
        closePair = head closePairs
    putStrLn $ (uncurry commonPositions) closePair
