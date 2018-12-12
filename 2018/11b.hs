import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List.Extra


powerLevel serial x y =
    let powerLevel = ((x + 10) * y + serial) * (x + 10)
        hundreds = (powerLevel `mod` 1000) `div` 100
    in hundreds - 5


powerLevels serial size =
    let coordinates = S.fromList [(i, j) | i <- [1..size], j <- [1..size]]
    in M.fromSet (uncurry $ powerLevel serial) coordinates


integral grid 0 =
    M.empty

integral grid s =
    updatePositions positions
    where updatePositions [] = integral grid (s-1)
          updatePositions ((x, y) : ps) = 
              let remote  = updatePositions ps
                  above   = M.findWithDefault 0 (x  , y-1) remote
                  left    = M.findWithDefault 0 (x-1, y  ) remote
                  double  = M.findWithDefault 0 (x-1, y-1) remote
                  current = M.findWithDefault 0 (x  , y  ) grid
                  value   = current + above + left - double
              in M.insert (x, y) value remote
          positions = [(s, s)] ++
                      reverse [(i, s) | i <- [1..(s-1)]] ++
                      reverse [(s, i) | i <- [1..(s-1)]]


squareValue integrated (x, y, s) =
    let north  = M.findWithDefault 0 (x+s-1, y  -1) integrated
        west   = M.findWithDefault 0 (x  -1, y+s-1) integrated
        double = M.findWithDefault 0 (x  -1, y  -1) integrated
        all    = M.findWithDefault 0 (x+s-1, y+s-1) integrated
    in all - north - west + double


bestSquare integrated size =
    let params = [(i, j, s) | i <- [1..size], 
                              j <- [1..size], 
                              s <- [1..minimum [size-i, size-j]]]
    in maximumOn (squareValue integrated) params


solve serial =
    let size = 300
        grid = powerLevels serial size
        integrated = integral grid size
    in bestSquare integrated size


main = do
    interact (unlines . map (show . solve . read) . lines)
