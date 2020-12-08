{- --- Day 8: Handheld Halting ---
 - Your flight to the major airline hub reaches cruising altitude without
 - incident. While you consider checking the in-flight menu for one of those
 - drinks that come with a little umbrella, you are interrupted by the kid
 - sitting next to you.
 -
 - Their handheld game console won't turn on! They ask if you can take a look.
 -
 - You narrow the problem down to a strange infinite loop in the boot code (your
 - puzzle input) of the device. You should be able to fix it, but first you need
 - to be able to run the code in isolation.
 -
 - The boot code is represented as a text file with one instruction per line of
 - text. Each instruction consists of an operation (acc, jmp, or nop) and an
 - argument (a signed number like +4 or -20).
 -
 - * acc increases or decreases a single global value called the accumulator by
 -   the value given in the argument. For example, acc +7 would increase the
 -   accumulator by 7. The accumulator starts at 0. After an acc instruction,
 -   the instruction immediately below it is executed next.
 - * jmp jumps to a new instruction relative to itself. The next instruction to
 -   execute is found using the argument as an offset from the jmp instruction;
 -   for example, jmp +2 would skip the next instruction, jmp +1 would continue
 -   to the instruction immediately below it, and jmp -20 would cause the
 -   instruction 20 lines above to be executed next.
 - * nop stands for No OPeration - it does nothing. The instruction immediately
 -   below it is executed next.
 -
 - For example, consider the following program:
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
 - These instructions are visited in this order:
 -
 -    nop +0  | 1
 -    acc +1  | 2, 8(!)
 -    jmp +4  | 3
 -    acc +3  | 6
 -    jmp -3  | 7
 -    acc -99 |
 -    acc +1  | 4
 -    jmp -4  | 5
 -    acc +6  |
 -
 - First, the nop +0 does nothing. Then, the accumulator is increased from 0 to
 - 1 (acc +1) and jmp +4 sets the next instruction to the other acc +1 near the
 - bottom. After it increases the accumulator from 1 to 2, jmp -4 executes,
 - setting the next instruction to the only acc +3. It sets the accumulator to
 - 5, and jmp -3 causes the program to continue back at the first acc +1.
 -
 - This is an infinite loop: with this sequence of jumps, the program will run
 - forever. The moment the program tries to run any instruction a second time,
 - you know it will never terminate.
 -
 - Immediately before the program would run an instruction a second time, the
 - value in the accumulator is 5.
 -
 - Run your copy of the boot code. Immediately before any instruction is
 - executed a second time, what value is in the accumulator?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Vector as V
import qualified Data.Set as Set

main :: IO ()
main = defaultMain parseInput handleInput

data Instruction = Acc Int | Nop | Jmp Int deriving (Show, Eq, Ord)

data Program = Program !(V.Vector Instruction) deriving (Show, Eq, Ord)

data ExecutionState = ExecutionState
    -- The program we are executing.
    { _program     :: !Program
    -- The current program counter.
    , _pc          :: !Int
    -- The accumulator value.
    , _accumulator :: !Int
    -- Previously visited program counters.
    , _pcs         :: !(Set.Set Int)
    } deriving (Show, Eq, Ord)

handleInput :: Program -> IO ()
handleInput = print . _accumulator . executeUntilLoop

executeUntilLoop :: Program -> ExecutionState
executeUntilLoop = head . dropWhile uniqueStatement . execute
  where
    uniqueStatement (ExecutionState _ pc _ pcs) =
        not $ pc `Set.member` pcs

execute :: Program -> [ExecutionState]
execute program = iterate executeStatement initialState
  where
    initialState = ExecutionState program 0 0 Set.empty

executeStatement :: ExecutionState -> ExecutionState
executeStatement ex@(ExecutionState _ pc accumulator pcs) =
    case getStatement ex of
        Nop -> ex { _pc = pc + 1,  _pcs = pcs'}
        Acc n -> ex { _pc = pc + 1, _accumulator = accumulator + n, _pcs = pcs' }
        Jmp n -> ex { _pc = pc + n, _pcs = pcs' }
  where
    pcs' = Set.insert pc pcs

getStatement :: ExecutionState -> Instruction
getStatement (ExecutionState (Program program) pc _ _) = program V.! pc

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
    parseNop = P.string "nop " *> P.int *> pure Nop
    parseJmp = Jmp <$> (P.string "jmp " *> P.int)
