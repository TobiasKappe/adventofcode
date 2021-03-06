import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Tuple as T
import qualified Data.List.Extra as E
import qualified Data.Sequence as Sq
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
    in L.filter ((flip S.notMember) cavern) candidates


neighbourhood :: Cavern -> Field -> Position -> [Position]

neighbourhood cavern field position =
    let adj = adjacent cavern position
    in L.filter ((flip M.notMember) field) adj


findEnemies :: Field -> Unit -> [Unit]

findEnemies field unit =
    L.filter (enemy unit) $ M.elems field
    where enemy u1 u2 = faction u1 /= faction u2


findOpponents :: Cavern -> Field -> Unit -> [Unit]

findOpponents cavern field unit =
    let adj = S.fromList $ adjacent cavern (position unit)
        enemies = findEnemies field unit
        inRange u = S.member (position u) adj
    in L.filter (inRange) enemies


floodFill :: Cavern -> Field -> Position -> Distances

floodFill cavern field start =
    let initialDistances = M.singleton start 0
        initialTodo = Sq.singleton start
    in floodFill' cavern field initialDistances initialTodo
    where floodFill' cavern field distances Sq.Empty = distances
          floodFill' cavern field distances (position Sq.:<| todo) =
              let hood = neighbourhood cavern field position
                  distance = distances M.! position
                  unexplored = L.filter ((flip M.notMember) distances) hood
                  updateDistance m position = M.insert position (distance+1) m
                  newDistances = foldl (updateDistance) distances unexplored
                  newTodo = foldl (Sq.|>) todo unexplored
              in floodFill' cavern field newDistances newTodo


preferenceCompare :: Distances -> Position -> Position -> Ordering

preferenceCompare distances pos1 pos2 =
    let dist1 = M.lookup pos1 distances
        dist2 = M.lookup pos2 distances
    in case compare dist1 dist2 of
        LT -> LT
        GT -> GT
        EQ -> compare pos1 pos2


findTarget :: Cavern -> Field -> Unit -> Maybe Position

findTarget cavern field unit =
    let enemies = findEnemies field unit
        inrange = concat $ map (neighbourhood cavern field . position) enemies
        distances = floodFill cavern field (position unit)
        reachable = L.filter ((flip M.member) distances) inrange
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
                let distances = floodFill cavern field target
                    hood = neighbourhood cavern field (position unit)
                    valid = L.filter ((flip M.member) distances) hood
                    next = L.minimumBy (preferenceCompare distances) valid
                in moveInField field unit next
    else (unit, field)


attackInField :: Field -> [Position] -> Int -> Unit -> ([Position], Field)

attackInField field positions elfPowerup (Unit pos faction hp) =
    let damage = case faction of
                          Elf -> 3
                          Goblin -> 3 + elfPowerup
        newHp = hp - damage
        fieldWithout = M.delete pos field
    in if newHp <= 0 
       then let newPositions = L.filter (/= pos) positions
            in (newPositions, fieldWithout)
       else let newUnit = Unit pos faction newHp 
                newField = M.insert pos newUnit fieldWithout
            in (positions, newField)


victimCompare u1 u2 =
    case compare (hp u1) (hp u2) of
        LT -> LT
        GT -> GT
        EQ -> compare u1 u2


simulateAttack :: Cavern -> Int -> Field -> [Position] -> Unit -> ([Position], Field)

simulateAttack cavern elfPowerup field positions unit =
    let opponents = findOpponents cavern field unit
    in if opponents == []
       then (positions, field)
       else let victim = E.minimumBy (victimCompare) opponents
            in attackInField field positions elfPowerup victim


simulateTurn :: Cavern -> Int -> Field -> [Position] -> Unit -> ([Position], Field)

simulateTurn cavern elfPowerup field positions unit =
    let (nextUnit, nextField) = simulateMove cavern field unit
    in simulateAttack cavern elfPowerup nextField positions nextUnit


simulate :: Cavern -> Int -> Field -> [(Int, Field)]

simulate cavern elfPowerup field =
    simulate' cavern elfPowerup field 0 []
    where simulate' cavern elfPowerup field n [] =
              let positions = M.keys field
              in simulate' cavern elfPowerup field (n+1) positions
          simulate' cavern elfPowerup field n (position : positions) =
              case M.lookup position field of
                  Nothing -> simulate' cavern elfPowerup field n positions
                  Just unit ->
                      let (newPositions, newField) =
                              simulateTurn cavern elfPowerup field positions unit
                          remainder = simulate' cavern elfPowerup newField n newPositions
                      in (n, field) : remainder


isFaction :: Faction -> Unit -> Bool

isFaction faction (Unit _ theFaction _) = faction == theFaction


elfCount :: Field -> Int

elfCount field = M.size $ M.filter (isFaction Elf) field


goblinCount :: Field -> Int

goblinCount field = M.size $ M.filter (isFaction Goblin) field


elfDown :: Int -> Field -> Bool

elfDown elves field = elfCount field < elves


over :: Int -> (Int, Field) -> Bool

over elves (n, field) =
    elfDown elves field || goblinCount field == 0


outcomeIfWinWithoutLosses :: Cavern -> Int -> Field -> Maybe Int

outcomeIfWinWithoutLosses cavern elfPowerup field =
    let elves = elfCount field
        states = simulate cavern elfPowerup field
        Just (finalRound, finalField) = L.find (over elves) states
    in if elfDown elves finalField
       then Nothing
       else let totalHp = sum $ map (hp) $ M.elems finalField
            in Just ((finalRound-1) * totalHp)


search :: Int -> Cavern -> Field -> Int

search n cavern field =
    case outcomeIfWinWithoutLosses cavern n field of
          Just outcome -> outcome
          Nothing -> search (n+1) cavern field


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
        units = L.filter (thisline) $ M.elems field
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
    print $ search 1 cavern field
