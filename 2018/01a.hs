import System.IO (isEOF)


readLines = do
    done <- isEOF
    if done
    then return []
    else do
        line <- getLine
        rest <- readLines
        return (line : rest)


parseChange (sign : rest) =
    if sign == '-' then -(read rest)
    else read rest


main = do 
    changesAsStrings <- readLines
    let changesAsIntegers = map parseChange changesAsStrings
    print $ sum changesAsIntegers
