import qualified Data.Set as S
import qualified Data.Map.Strict as M


powerLevel serial x y =
    let powerLevel = ((x + 10) * y + serial) * (x + 10)
        hundreds = (powerLevel `mod` 1000) `div` 100
    in hundreds - 5


powerLevels serial size =
    let coordinates = S.fromList [(i, j) | i <- [1..size], j <- [1..size]]
    in M.fromSet (uncurry $ powerLevel serial) coordinates


totalPower grid x y size = 
    let square = [(i, j) | i <- [x..(x+size-1)], j <- [y..(y+size-1)]]
    in fmap sum $ sequence $ map (\s -> M.lookup s grid) square
    

totalPowers grid size =
    M.mapWithKey (\(x, y) v -> totalPower grid x y size) grid


optimum map =
    let optimumValue = maximum $ M.elems map
        optimumKeys = M.keys $ M.filter (== optimumValue) map
    in minimum optimumKeys


solve serial =
    let size = 300
        grid = powerLevels serial size
        powers = totalPowers grid 3
    in optimum powers
        

main = 
    interact (unlines . map (show . solve . read) . lines)
