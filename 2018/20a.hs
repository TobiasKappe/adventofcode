import qualified Data.Map.Strict as M
import qualified Data.Set as S

import System.IO


data Direction = North | South | East | West deriving (Show, Eq, Ord)

data Instr = Follow Instr Instr | Choice Instr Instr | Step Direction | Empty

data Tree = Branch Tree Tree | Action Direction Tree | Leaf


parse ('^' : expr) = 
    fst $ parseFull expr
    where parseFull ('(' : expr) = parseChoice expr
          parseFull expr = parseFollow expr
          
          parseStep 'N' = Step North
          parseStep 'S' = Step South
          parseStep 'E' = Step East
          parseStep 'W' = Step West
          parseStep c = error [c]
          
          parseUnit ('(' : expr) = parseChoice expr
          parseUnit (char : expr) = (parseStep char, expr)
          
          parseFollow ('$' : expr) = (Empty, '$' : expr)
          parseFollow (')' : expr) = (Empty, ')' : expr)
          parseFollow ('|' : expr) = (Empty, '|' : expr)
          parseFollow expr =
              let (first, remainder) = parseUnit expr
                  (next, finalRemainder) = parseFollow remainder
              in (Follow first next, finalRemainder)
          
          parseChoice (')' : expr) = (Empty, expr)
          parseChoice ('|' : expr) = parseChoice expr
          parseChoice expr =  
              let (first, remainder) = parseFull expr
                  (next, finalRemainder) = parseChoice remainder
              in (Choice first next, finalRemainder)


unfold Empty = Leaf

unfold instr =
    unfold' instr Empty
    where unfold' Empty Empty =
              Leaf
          unfold' Empty remainder =
              unfold remainder
          unfold' (Step direction) remainder =
              Action direction (unfold remainder)
          unfold' (Follow Empty second) remainder =
              unfold' second remainder
          unfold' (Follow first second) remainder =
              unfold' first (Follow second remainder)
          unfold' (Choice Empty right) remainder =
              unfold' right remainder
          unfold' (Choice left Empty) remainder =
              unfold' left remainder
          unfold' (Choice left right) remainder =
              Branch (unfold' left remainder) (unfold' right remainder)


move (x, y) North = (x, y-1)
move (x, y) South = (x, y+1)
move (x, y) East = (x+1, y)
move (x, y) West = (x-1, y)


walk tree =
    walk' tree (0, 0)
    where walk' Leaf position =
              M.empty
          walk' (Action direction remainder) position =
              let newPosition = move position direction
                  doors = walk' remainder newPosition
                  thisDoor = M.fromList [(position, S.singleton newPosition),
                                         (newPosition, S.singleton position)]
              in M.unionWith (S.union) thisDoor doors
          walk' (Branch left right) position =
              let planLeft = walk' left position
                  planRight = walk' right position
              in M.unionWith (S.union) planLeft planRight


fill :: M.Map (Int, Int) (S.Set (Int, Int)) -> M.Map (Int, Int) Int

fill doors =
    update 0 M.empty (0, 0)
    where visit distance visited position =
              case M.lookup position visited of
                  Nothing -> update distance visited position
                  Just currentDistance ->
                      if distance < currentDistance
                      then update distance visited position
                      else visited
          update distance visited position =
              let newVisited = M.insert position distance visited
                  hood = M.findWithDefault S.empty position doors
              in foldl (visit (distance+1)) newVisited hood


main = do
    line <- getLine
    let instr = parse line
        tree = unfold instr
        plan = walk tree
    print $ maximum $ M.elems $ fill plan
