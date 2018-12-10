import System.IO (isEOF)
import qualified Data.Set


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


integratedFrequencies buffer [] sum =
    integratedFrequencies buffer buffer sum

integratedFrequencies buffer (change : changes) sum =
    let newSum = sum + change
    in (newSum : integratedFrequencies buffer changes newSum)


firstDuplicate (x : xs) seen =
    if Data.Set.member x seen then x
    else 
        let newSeen = Data.Set.insert x seen
        in firstDuplicate xs newSeen


main = do 
    changesAsStrings <- readLines
    let changesAsIntegers = map parseChange changesAsStrings
        runningTotals = integratedFrequencies changesAsIntegers [] 0
    print $ firstDuplicate runningTotals Data.Set.empty
