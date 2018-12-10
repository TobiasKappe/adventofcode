import System.IO (isEOF)
import Text.Regex.Posix
import Data.List


isMinimal edges node =
    let incoming = filter (\s -> snd s == node) edges
    in incoming == []


nodesOf edges =
    Data.List.nub $ (map fst edges) ++ (map snd edges)


frontOf edges nodes =
    filter (isMinimal edges) nodes


topologicalSort [] nodes = sort nodes

topologicalSort edges nodes =
    let step = minimum $ frontOf edges nodes
        newEdges = filter ((step /=) . fst) edges
        newNodes = filter (step /=) nodes
    in step : topologicalSort newEdges newNodes


readLines = do
    done <- isEOF
    if done
    then return []
    else do
        line <- getLine
        rest <- readLines
        return (line : rest)


parseLine :: String -> (Char, Char)

parseLine line =
    let pattern = "Step (.*) must be finished before step (.*) can begin."
        parts = tail $ head (line =~ pattern) :: [String]
        [first, next] = parts
    in (head first, head next)


main = do
    lines <- readLines
    let edges = map parseLine lines
        nodes = nodesOf edges 
    print $ topologicalSort edges nodes
