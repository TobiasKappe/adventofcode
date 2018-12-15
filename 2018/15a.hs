import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Tuple as T
import qualified Data.List.Extra as E
import System.IO (isEOF)

import GHC.Exts (sortWith)


data Position = Position Int Int deriving (Eq, Show)

data Faction = Elf | Goblin deriving (Eq, Show)

data Entity = Wall | Creature Faction

data Unit = Unit { 
    position :: Position,
    faction :: Faction,
    hp :: Int
} deriving (Eq, Show)

type Cavern = S.Set Position

type Field = M.Map Position Unit

type Distances = M.Map Position Int


instance Ord Position where
    compare (Position x1 y1) (Position x2 y2) = compare (y1, x1) (y2, x2)


instance Ord Unit where
    compare u1 u2 = compare (position u1) (position u2)


adjacent :: Cavern -> Position -> [Position]

adjacent cavern (Position x y) =
    let candidates = [Position (x+1) y, 
                      Position x (y+1), 
                      Position (x-1) y, 
                      Position x (y-1)]
    in filter ((flip S.notMember) cavern) candidates


neighbourhood :: Cavern -> Field -> Position -> [Position]

neighbourhood cavern field position =
    let adj = adjacent cavern position
    in filter ((flip M.notMember) field) adj


findEnemies :: Field -> Unit -> [Unit]

findEnemies field unit =
    L.filter (enemy unit) $ M.elems field
    where enemy u1 u2 = faction u1 /= faction u2


findOpponents :: Cavern -> Field -> Unit -> [Unit]

findOpponents cavern field unit =
    let adj = S.fromList $ adjacent cavern (position unit)
        enemies = findEnemies field unit
        inRange u = S.member (position u) adj
    in filter (inRange) enemies


floodFill :: Cavern -> Field -> Int -> Distances -> Position -> Distances

floodFill cavern field distance distances current =
    let updatedDistances = M.insert current distance distances
        hood = neighbourhood cavern field current
        folder = floodFill cavern field (distance+1)
        newDistances = foldl folder updatedDistances hood
    in case M.lookup current distances of
        Nothing -> newDistances
        Just currentDistance ->
            if currentDistance <= distance
            then distances
            else newDistances


preferenceCompare :: Distances -> Position -> Position -> Ordering

preferenceCompare distances pos1 pos2 =
    let dist1 = M.lookup pos1 distances
        dist2 = M.lookup pos2 distances
    in case compare dist1 dist2 of
        LT -> LT
        GT -> GT
        EQ -> compare pos1 pos2


over :: Int -> Field -> Bool

over round field =
    (length $ L.nub $ map (faction) $ M.elems field) < 2


findTarget :: Cavern -> Field -> Unit -> Maybe Position

findTarget cavern field unit =
    let enemies = findEnemies field unit
        inrange = concat $ map (neighbourhood cavern field . position) enemies
        distances = floodFill cavern field 0 M.empty (position unit)
        reachable = filter ((flip M.member) distances) inrange
    in case reachable of
        [] -> Nothing
        _ -> Just (L.minimumBy (preferenceCompare distances) reachable)


moveInField :: Field -> Unit -> Position -> (Unit, Field)

moveInField field unit next =
    let Unit oldPosition faction hp = unit
        newUnit = Unit next faction hp
        fieldWithout = M.delete oldPosition field
        fieldWith = M.insert next newUnit fieldWithout
    in (newUnit, fieldWith)


simulateMove :: Cavern -> Field -> Unit -> (Unit, Field)

simulateMove cavern field unit =
    if findOpponents cavern field unit == [] 
    then case findTarget cavern field unit of
            Nothing -> (unit, field)
            Just target ->
                let distances = floodFill cavern field 0 M.empty target
                    hood = neighbourhood cavern field (position unit)
                    valid = L.filter ((flip M.member) distances) hood
                    next = L.minimumBy (preferenceCompare distances) valid
                in moveInField field unit next
    else (unit, field)


attackInField :: Field -> Unit -> Field

attackInField field (Unit pos faction hp) =
    let attackPower = 3
        newHp = hp - attackPower
        fieldWithout = M.delete pos field
    in if newHp <= 0 
       then fieldWithout
       else let newUnit = Unit pos faction newHp 
            in M.insert pos newUnit fieldWithout


simulateAttack :: Cavern -> Field -> Unit -> Field

simulateAttack cavern field unit =
    let opponents = findOpponents cavern field unit
    in if opponents == []
       then field
       else let victim = E.minimumOn (hp) $ L.sort opponents
            in attackInField field victim


simulateTurn :: Cavern -> Field -> Unit -> Field

simulateTurn cavern field unit =
    let (nextUnit, nextField) = simulateMove cavern field unit
    in simulateAttack cavern nextField nextUnit


simulate :: Int -> Cavern -> Field -> [Position] -> [(Int, Field)]

simulate n cavern field [] =
    let positions = M.keys field
    in simulate (n+1) cavern field positions

simulate n cavern field (position : positions) =
    case M.lookup position field of
        Nothing -> simulate n cavern field positions
        Just unit ->
            let newField = simulateTurn cavern field unit
            in (n, field) : (simulate n cavern newField positions)


outcome :: Cavern -> Field -> Int

outcome cavern field =
    let fields = simulate 0 cavern field []
        Just (finalRound, finalField) = L.find (uncurry over) fields
        totalHp = sum $ map (hp) $ M.elems finalField
    in (finalRound-1) * totalHp


readLines = do
    done <- isEOF
    if done
    then return []
    else do
        line <- getLine
        rest <- readLines
        return (line : rest)


parseChar :: Char -> Maybe Entity

parseChar char =
    case char of
        'E'  -> Just (Creature Elf)
        'G'  -> Just (Creature Goblin)
        '#'  -> Just (Wall)
        _    -> Nothing


parseLine :: Int -> Int -> String -> (Cavern, Field)

parseLine row column [] =
    (S.empty, M.empty)

parseLine row column (char : chars) =
    let (cavern, field) = parseLine row (column+1) chars
    in case parseChar char of
        Just (Creature faction) -> 
            let newPosition = Position column row
                newUnit = Unit newPosition faction 200
                newField = M.insert newPosition newUnit field
            in (cavern, newField)
        Just Wall ->
            let newPosition = Position column row
                newCavern = S.insert newPosition cavern
            in (newCavern, field)
        Nothing -> (cavern, field)


parseLines :: Int -> [String] -> (Cavern, Field)

parseLines row  [] =
    (S.empty, M.empty)

parseLines row (line : lines) =
    let (newCavern, newField) = parseLine row 0 line
        (otherCavern, otherField) = parseLines (row+1) lines
        finalCavern = S.union newCavern otherCavern
        finalField = M.union newField otherField
    in (finalCavern, finalField)


renderLineUpTo row (-1) cavern field =
    return ()

renderLineUpTo row column cavern field = do
    renderLineUpTo row (column-1) cavern field
    let position = Position column row
    if S.member position cavern
    then putStr "#"
    else case M.lookup position field of
              Just (Unit _ faction _) -> putStr $ symbolFor faction
              Nothing -> putStr "."


symbolFor :: Faction -> String

symbolFor faction =
    case faction of
        Elf -> "E"
        Goblin -> "G"


renderHitpoints :: Unit -> String

renderHitpoints (Unit _ faction hp) =
    let symbol = symbolFor faction
    in symbol ++ "(" ++ (show hp) ++ ")"


renderHitpointsOn :: Int -> Field -> IO()

renderHitpointsOn south field =
    let thisline (Unit (Position _ y) _ _) = y == south
        units = filter (thisline) $ M.elems field
        rendered = map (renderHitpoints) units
    in putStr $ L.intercalate ", " rendered


renderUpTo :: Int -> Int -> Cavern -> Field -> IO ()

renderUpTo (-1) east cavern field =
    return ()

renderUpTo south east cavern field = do
    renderUpTo (south-1) east cavern field
    renderLineUpTo south east cavern field
    putStr "   "
    renderHitpointsOn south field
    putStrLn ""


render :: Cavern -> Field -> IO ()

render cavern field =
    let Position south east = S.findMax cavern
    in renderUpTo south east cavern field


main = do
    lines <- readLines
    let (cavern, field) = parseLines 0 lines
    print $ outcome cavern field
