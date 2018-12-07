import System.IO (isEOF)
import Text.Regex.Posix
import Data.List
import Data.Char (ord)
import qualified Data.Map
import qualified Data.Set


isMinimal edges node =
    all (\s -> snd s /= node) edges


nodesOf edges =
    Data.List.nub $ (map fst edges) ++ (map snd edges)


frontOf edges nodes =
    sort $ filter (isMinimal edges) nodes


duration x = 61 + ord x - ord 'A'


decrementIn indices k v =
    if elem k indices then (v-1) else v


simulate n todo edges [] [] = []

simulate n todo edges nodes active =
    let available = (frontOf edges nodes) \\ active
        starting = take (n - length active) available
        running = active ++ starting
        newTodo = Data.Map.mapWithKey (decrementIn running) todo
        done = Data.Map.keys $ Data.Map.filter (== 0) newTodo
        newEdges = filter (\(a, b) -> not $ elem a done) edges
        newNodes = nodes \\ done
        newActive = running \\ done
    in active : simulate n newTodo newEdges newNodes newActive


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
        todo = Data.Map.fromSet (duration) (Data.Set.fromList nodes)
    print $ length $ simulate 5 todo edges nodes []
