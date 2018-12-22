import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Bits
import System.IO (isEOF)
import Text.Regex.Posix


type Registers = M.Map Int Int

type Args = (Int, Int, Int)

type Getter = Registers -> Int -> Int

type Actor = Int -> Int -> Int

type Instruction = (String, Args)


operation :: Getter -> Getter -> Actor -> Registers -> Args -> Registers

operation getLeft getRight actor register (left, right, dst) =
    let vLeft = getLeft register left
        vRight = getRight register right
        res = actor vLeft vRight
    in M.insert dst res register


getRegister reg num = M.findWithDefault 0 num reg
getImmediate reg num = num
        

testGreater x y = if x > y then 1 else 0
testEquality x y = if x == y then 1 else 0


operations = M.fromList [("addr", operation (getRegister)  (getRegister) (+)),
                         ("addi", operation (getRegister)  (getImmediate) (+)),
                         ("mulr", operation (getRegister)  (getRegister) (*)),
                         ("muli", operation (getRegister)  (getImmediate) (*)),
                         ("banr", operation (getRegister)  (getRegister) (.&.)),
                         ("bani", operation (getRegister)  (getImmediate) (.&.)),
                         ("borr", operation (getRegister)  (getRegister) (.|.)),
                         ("bori", operation (getRegister)  (getImmediate) (.|.)),
                         ("setr", operation (getRegister)  (getImmediate) (curry fst)),
                         ("seti", operation (getImmediate) (getImmediate) (curry fst)),
                         ("gtir", operation (getImmediate) (getRegister) (testGreater)),
                         ("gtri", operation (getRegister)  (getImmediate) (testGreater)),
                         ("gtrr", operation (getRegister)  (getRegister) (testGreater)),
                         ("eqir", operation (getImmediate) (getRegister) (testEquality)),
                         ("eqri", operation (getRegister)  (getImmediate) (testEquality)),
                         ("eqrr", operation (getRegister)  (getRegister) (testEquality))]


simulate initial trap bind instructions = 
    simulate' 0 M.empty initial
    where simulate' ip registers state =
              let (modIp, modRegisters, modState) = trap ip registers state
              in case M.lookup modIp instructions of
                  Nothing -> modState
                  Just (opcode, args) -> 
                      let effect = operations M.! opcode
                          preRegisters = M.insert bind modIp modRegisters
                          postRegisters = effect preRegisters args
                          newIp = getRegister postRegisters bind
                      in simulate' (newIp+1) postRegisters modState


readLines = do
    done <- isEOF
    if done
    then return []
    else do
        line <- getLine
        rest <- readLines
        return (line : rest)


parseInstruction :: String -> Instruction

parseInstruction line =
    let pattern = "(.*) (.*) (.*) (.*)"
        parts = tail $ head (line =~ pattern) :: [String]
        [opcode, left, right, dst] = parts
    in (opcode, (read left, read right, read dst))
    

parseInstructions lines =
    parseInstructions' 0 lines
    where parseInstructions' ip [] = M.empty
          parseInstructions' ip (line : lines) =
              let instruction = parseInstruction line
                  instructions = parseInstructions' (ip+1) lines
              in M.insert ip instruction instructions


parseBind :: String -> Int

parseBind line =
    let pattern = "#ip (.)"
        parts = tail $ head (line =~ pattern) :: [String]
    in (read $ head parts)


shiftSim 17 registers state =
    let shifted = (getRegister registers 4) `shiftR` 8
        newRegisters = M.insert 4 shifted registers
    in (8, newRegisters, state)


shiftSim 28 registers (seen, lastValue) = 
    let newValue = getRegister registers 5
    in if S.member newValue seen
       then (-1, registers, (seen, lastValue))
       else let newSeen = S.insert newValue seen
            in (28, registers, (newSeen, newValue))

shiftSim ip registers state = (ip, registers, state)
    

main = do
    (bindLine : instructionLines) <- readLines
    let bind = parseBind bindLine
        instructions = parseInstructions instructionLines
        (seen, lastValue) = simulate (S.empty, -1) shiftSim bind instructions
    print $ lastValue
