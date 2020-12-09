{- After some careful analysis, you believe that exactly one instruction is
 - corrupted.
 -
 - Somewhere in the program, either a jmp is supposed to be a nop, or a nop is
 - supposed to be a jmp. (No acc instructions were harmed in the corruption of
 - this boot code.)
 -
 - The program is supposed to terminate by attempting to execute an instruction
 - immediately after the last instruction in the file. By changing exactly one
 - jmp or nop, you can repair the boot code and make it terminate correctly.
 -
 - For example, consider the same program from above:
 -
 -    nop +0
 -    acc +1
 -    jmp +4
 -    acc +3
 -    jmp -3
 -    acc -99
 -    acc +1
 -    jmp -4
 -    acc +6
 -
 - If you change the first instruction from nop +0 to jmp +0, it would create a
 - single-instruction infinite loop, never leaving that instruction. If you
 - change almost any of the jmp instructions, the program will still eventually
 - find another jmp instruction and loop forever.
 -
 - However, if you change the second-to-last instruction (from jmp -4 to nop
 - -4), the program terminates! The instructions are visited in this order:
 -
 -    nop +0  | 1
 -    acc +1  | 2
 -    jmp +4  | 3
 -    acc +3  |
 -    jmp -3  |
 -    acc -99 |
 -    acc +1  | 4
 -    nop -4  | 5
 -    acc +6  | 6
 -
 - After the last instruction (acc +6), the program terminates by attempting to
 - run the instruction below the last instruction in the file. With this change,
 - after the program terminates, the accumulator contains the value 8 (acc +1,
 - acc +1, acc +6).
 -
 - Fix the program so that it terminates normally by changing exactly one jmp
 - (to nop) or nop (to jmp). What is the value of the accumulator after the
 - program terminates?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Vector as V
import qualified Data.Set as Set
import qualified Data.Either as E

main :: IO ()
main = defaultMain parseInput handleInput

data Instruction = Acc Int | Nop Int | Jmp Int deriving (Show, Eq, Ord)

data Program = Program !(V.Vector Instruction) deriving (Show, Eq, Ord)

data ExecutionState = ExecutionState
    -- The program we are executing.
    { _program     :: !Program
    -- The length of the program.
    , _length      :: !Int
    -- The current program counter.
    , _pc          :: !Int
    -- The accumulator value.
    , _accumulator :: !Int
    -- Previously visited program counters.
    , _pcs         :: !(Set.Set Int)
    } deriving (Show, Eq, Ord)

data ExecutionError = InfiniteLoop deriving (Show, Eq, Ord)

handleInput :: Program -> IO ()
handleInput program = case results of
    [] -> print "No candidate programs ended without an infinite loop."
    (x:_) -> print . _accumulator $ x
  where
    results = E.rights . map execute . candidatePrograms $ program

execute :: Program -> Either ExecutionError ExecutionState
execute program@(Program instructions) = execute' initialState
  where
    len = V.length instructions
    initialState = ExecutionState program len 0 0 Set.empty

execute' :: ExecutionState -> Either ExecutionError ExecutionState
execute' state
    | pc `Set.member` pcs = Left InfiniteLoop
    | pc >= len = Right state'
    | otherwise = execute' state'
  where
    state'@(ExecutionState _ len pc _ pcs) = executeStatement state

executeStatement :: ExecutionState -> ExecutionState
executeStatement ex@(ExecutionState _ _ pc accumulator pcs) =
    case getStatement ex of
        Nop _ -> ex { _pc = pc + 1,  _pcs = pcs'}
        Acc n -> ex { _pc = pc + 1, _accumulator = accumulator + n, _pcs = pcs' }
        Jmp n -> ex { _pc = pc + n, _pcs = pcs' }
  where
    pcs' = Set.insert pc pcs

getStatement :: ExecutionState -> Instruction
getStatement (ExecutionState (Program program) _ pc _ _) = program V.! pc

candidatePrograms :: Program -> [Program]
candidatePrograms program@(Program instructions) =
    map (swapInstructionAt program) indices
  where
    len = V.length instructions
    indices = take len [0..]


swapInstructionAt :: Program -> Int -> Program
swapInstructionAt (Program instructions) n =
    Program $ instructions V.// [(n, replacement)]
  where
    instruction = instructions V.! n
    replacement = swapInstruction instruction

swapInstruction :: Instruction -> Instruction
swapInstruction (Nop n) = Jmp n
swapInstruction (Jmp n) = Nop n
swapInstruction (Acc n) = Acc n

parseInput :: T.Text -> Either P.ParseError Program
parseInput = P.parse (parseProgram <* P.eof) ""

parseProgram :: P.Parsec T.Text () Program
parseProgram = Program . V.fromList <$> parseInstructions

parseInstructions :: P.Parsec T.Text () [Instruction]
parseInstructions = parseInstruction `P.endBy` P.newline

parseInstruction :: P.Parsec T.Text () Instruction
parseInstruction = P.choice [parseAcc, parseNop, parseJmp]
  where
    parseAcc = Acc <$> (P.string "acc " *> P.int)
    parseNop = Nop <$> (P.string "nop " *> P.int)
    parseJmp = Jmp <$> (P.string "jmp " *> P.int)
