import qualified Data.Map.Strict as M
import Data.Bits
import System.IO (isEOF)
import Text.Regex.Posix


type Registers = M.Map Int Int

type Args = (Int, Int, Int)

type Operation = Registers -> Args -> Registers

type Getter = Registers -> Int -> Int

type Actor = Int -> Int -> Int

type Instruction = (Int, Args)

data Trace = Trace Registers Instruction Registers deriving Show


operation :: Getter -> Getter -> Actor -> Registers -> Args -> Registers

operation getLeft getRight actor register (left, right, dst) =
    let vLeft = getLeft register left
        vRight = getRight register right
        res = actor vLeft vRight
    in M.insert dst res register


getRegister reg num = reg M.! num
getImmediate reg num = num
        

testGreater x y = if x > y then 1 else 0
testEquality x y = if x == y then 1 else 0


op_addr = operation (getRegister) (getRegister) (+)
op_addi = operation (getRegister) (getImmediate) (+)

op_mulr = operation (getRegister) (getRegister) (*)
op_muli = operation (getRegister) (getImmediate) (*)

op_banr = operation (getRegister) (getRegister) (.&.)
op_bani = operation (getRegister) (getImmediate) (.&.)

op_borr = operation (getRegister) (getRegister) (.|.)
op_bori = operation (getRegister) (getImmediate) (.|.)

op_setr = operation (getRegister) (getImmediate) (curry fst)
op_seti = operation (getImmediate) (getImmediate) (curry fst)

op_gtir = operation (getImmediate) (getRegister) (testGreater)
op_gtri = operation (getRegister) (getImmediate) (testGreater)
op_gtrr = operation (getRegister) (getRegister) (testGreater)

op_eqir = operation (getImmediate) (getRegister) (testEquality)
op_eqri = operation (getRegister) (getImmediate) (testEquality)
op_eqrr = operation (getRegister) (getRegister) (testEquality)


operations = [op_addr, op_addi,
              op_mulr, op_muli,
              op_banr, op_bani,
              op_borr, op_bori,
              op_setr, op_seti,
              op_gtir, op_gtri, op_gtrr,
              op_eqir, op_eqri, op_eqrr]


matches :: Trace -> Operation -> Bool

matches (Trace before (opcode, args) after) operation =
    operation before args == after


candidates :: Trace -> Int

candidates trace =
    length $ filter (matches trace) operations


readLines = do
    done <- isEOF
    if done
    then return []
    else do
        line <- getLine
        rest <- readLines
        return (line : rest)


parseRegisters :: String -> Registers

parseRegisters line =
    let pattern = ".*: *\\[(.*), (.*), (.*), (.*)\\]"
        parts = tail $ head (line =~ pattern) :: [String]
    in M.fromList $ zip [0..3] (map (read) parts)


parseInstruction :: String -> Instruction

parseInstruction line =
    let pattern = "(.*) (.*) (.*) (.*)"
        parts = tail $ head (line =~ pattern) :: [String]
        [opcode, left, right, dst] = parts
    in (read opcode, (read left, read right, read dst))
    

parseTraces ("" : lines) = []

parseTraces (beforeRaw : instructionRaw : afterRaw : "" : lines) =
    let before = parseRegisters beforeRaw
        instruction = parseInstruction instructionRaw
        after = parseRegisters afterRaw
    in (Trace before instruction after) : parseTraces lines


main = do
    lines <- readLines
    let traces = parseTraces lines
        matchesThreeOrMore = length $ filter (>= 3) $ map (candidates) traces
    print matchesThreeOrMore
