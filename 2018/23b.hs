import System.IO (isEOF)
import Text.Regex.Posix
import Data.List.Extra as L
import Data.Set as S
import Data.Maybe


data Bot = Bot { x :: Int,
                 y :: Int,
                 z :: Int,
                 r :: Int } deriving (Show, Eq, Ord)


data Octahedron = Octahedron { ppp :: Int,
                               ppn :: Int,
                               pnp :: Int,
                               pnn :: Int,
                               npp :: Int,
                               npn :: Int,
                               nnp :: Int,
                               nnn :: Int }


toOct bot =
    Octahedron (r bot + x bot + y bot + z bot)
               (r bot + x bot + y bot - z bot)
               (r bot + x bot - y bot + z bot)
               (r bot + x bot - y bot - z bot)
               (r bot - x bot + y bot + z bot)
               (r bot - x bot + y bot - z bot)
               (r bot - x bot - y bot + z bot)
               (r bot - x bot - y bot - z bot)


intersect oct1 oct2 =
    Octahedron (min (ppp oct1) (ppp oct2))
               (max (ppn oct1) (ppn oct2))
               (min (pnp oct1) (pnp oct2))
               (max (pnn oct1) (pnn oct2))
               (min (npp oct1) (npp oct2))
               (max (npn oct1) (npn oct2))
               (min (nnp oct1) (nnp oct2))
               (max (nnn oct1) (nnn oct2))


deltas octs start stop =
    let steps = [(1 + stop o, 1) | o <- octs] ++ [(start o, -1) | o <- octs]
    in M.toList $ M.fromListWith (+) steps


splits tbelow [] tabove [] = []

splits tbelow below tabove [] =
    let (bindex, bdelta) = head below
        newtbelow = tbelow + bdelta
    in (newtbelow, tabove) : splits newtbelow (tail below) tabove []

splits tbelow [] tabove above =
    let (aindex, adelta) = head above
        newtabove = abelow + adelta
    in (tbelow, newtabove) : splits tbelow [] newtabove (tail above)

splits tbelow below tabove above
    let (bindex, bdelta) = head below
        (aindex, adelta) = head above
        newtbelow = tbelow + bdelta
        newtabove = tabove + adelta
    case compare bindex aindex of
        LT -> (newtbelow, tabove) : splits newtbelow (tail below) tabove above
        GT -> (tbelow, newtabove) : splits tbelow below newtabove (tail above)
        EQ -> (newtbelow, newtabove) : splits newtbelow (tail below) newtabove (tail above)



bestSplit octs pos neg =
    let below = deltas octs (neg) (pos)
        above = deltas octs (pos) (neg)





parseBot :: String -> Bot

parseBot line =
    let pattern = "pos=<(.*),(.*),(.*)>, r=(.*)"
        parts = tail $ head (line =~ pattern)
        [x, y, z, radius] = L.map (read) parts
    in Bot x y z radius


readLines = do
    done <- isEOF
    if done
    then return []
    else do
        line <- getLine
        rest <- readLines
        return (line : rest)


main = do
    lines <- readLines
    let bots = L.map (parseBot) lines
        clique = S.toList $ S.findMax $ maxCliques bots
        furthest = L.maximum $ L.map (norm) clique
    print $ furthest
