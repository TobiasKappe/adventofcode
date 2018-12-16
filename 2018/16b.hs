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

type Instruction = (Int, Args)

data Trace = Trace Registers Instruction Registers deriving Show

data Operation = Operation String (Registers -> Args -> Registers)

instance Eq Operation where
    (Operation n1 _) == (Operation n2 _) = n1 == n2

instance Ord Operation where
    compare (Operation n1 _) (Operation n2 _) = compare n1 n2

instance Show Operation where
    show (Operation n _) = "<" ++ n ++ ">"


operation :: String -> Getter -> Getter -> Actor -> Operation

operation name getLeft getRight actor  =
    let effect register (left, right, dst) =
            let vLeft = getLeft register left
                vRight = getRight register right
                res = actor vLeft vRight
            in M.insert dst res register
    in Operation name effect


getRegister reg num = M.findWithDefault 0 num reg
getImmediate reg num = num
        

testGreater x y = if x > y then 1 else 0
testEquality x y = if x == y then 1 else 0


op_addr = operation "addr" (getRegister) (getRegister) (+)
op_addi = operation "addi" (getRegister) (getImmediate) (+)

op_mulr = operation "mulr" (getRegister) (getRegister) (*)
op_muli = operation "muli" (getRegister) (getImmediate) (*)

op_banr = operation "banr" (getRegister) (getRegister) (.&.)
op_bani = operation "bani" (getRegister) (getImmediate) (.&.)

op_borr = operation "borr" (getRegister) (getRegister) (.|.)
op_bori = operation "bori" (getRegister) (getImmediate) (.|.)

op_setr = operation "setr" (getRegister) (getImmediate) (curry fst)
op_seti = operation "seti" (getImmediate) (getImmediate) (curry fst)

op_gtir = operation "gtir" (getImmediate) (getRegister) (testGreater)
op_gtri = operation "gtri" (getRegister) (getImmediate) (testGreater)
op_gtrr = operation "gtrr" (getRegister) (getRegister) (testGreater)

op_eqir = operation "eqir" (getImmediate) (getRegister) (testEquality)
op_eqri = operation "eqri" (getRegister) (getImmediate) (testEquality)
op_eqrr = operation "eqrr" (getRegister) (getRegister) (testEquality)


operations = S.fromList [op_addr, op_addi,
                         op_mulr, op_muli,
                         op_banr, op_bani,
                         op_borr, op_bori,
                         op_setr, op_seti,
                         op_gtir, op_gtri, op_gtrr,
                         op_eqir, op_eqri, op_eqrr]


matches :: Trace -> Operation -> Bool

matches (Trace before (opcode, args) after) (Operation name effect) =
    effect before args == after


eliminate :: [Trace] -> M.Map Int (S.Set Operation)

eliminate [] = M.empty

eliminate (trace : traces) =
    let others = eliminate traces
        candidates = S.filter (matches trace) operations
        Trace _ (opcode, _) _ = trace
    in M.insertWith (S.intersection) opcode candidates others


simulate codemap registers [] = registers

simulate codemap registers (instruction : instructions) =
    let (opcode, args) = instruction
        Operation name effect = codemap M.! opcode
        newRegisters = effect registers args
    in simulate codemap newRegisters instructions


solve :: M.Map Int (S.Set Operation) -> M.Map Int Operation

solve candidates =
    let isSingleton set = S.size set == 1
        determined = M.filter (isSingleton) candidates
        fixed = M.map (S.findMin) determined
        remainder = M.difference candidates determined
        removeSolved set = set S.\\ (S.fromList $ M.elems fixed)
        reduced = M.map (removeSolved) remainder
    in if M.null determined
       then M.empty
       else M.union fixed (solve reduced)


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
    

parseTraces [] = []

parseTraces ([beforeRaw, instructionRaw, afterRaw] : lines) =
    let before = parseRegisters beforeRaw
        instruction = parseInstruction instructionRaw
        after = parseRegisters afterRaw
    in (Trace before instruction after) : parseTraces lines


parseGroups lines =
    if elem "" lines 
    then let (block, remainder) = span (/= "") lines
         in block : parseGroups (tail remainder)
    else [lines]


main = do
    lines <- readLines
    let groups = parseGroups lines
        isTrace group = length group == 3
        traces = parseTraces $ filter (isTrace) groups
        isInstructions group = length group > 3
        Just instructionsRaw = L.find (isInstructions) groups
        instructions = map (parseInstruction) $ instructionsRaw
        codemap = solve $ eliminate traces
        registers = simulate codemap M.empty instructions
    print $ registers M.! 0
