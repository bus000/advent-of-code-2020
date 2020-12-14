{- --- Day 14: Docking Data ---
 -
 - As your ferry approaches the sea port, the captain asks for your help again.
 - The computer system that runs this port isn't compatible with the docking
 - program on the ferry, so the docking parameters aren't being correctly
 - initialized in the docking program's memory.
 -
 - After a brief inspection, you discover that the sea port's computer system
 - uses a strange bitmask system in its initialization program. Although you
 - don't have the correct decoder chip handy, you can emulate it in software!
 -
 - The initialization program (your puzzle input) can either update the bitmask
 - or write a value to memory. Values and memory addresses are both 36-bit
 - unsigned integers. For example, ignoring bitmasks for a moment, a line like
 - mem[8] = 11 would write the value 11 to memory address 8.
 -
 - The bitmask is always given as a string of 36 bits, written with the most
 - significant bit (representing 2^35) on the left and the least significant bit
 - (2^0, that is, the 1s bit) on the right. The current bitmask is applied to
 - values immediately before they are written to memory: a 0 or 1 overwrites the
 - corresponding bit in the value, while an X leaves the bit in the value
 - unchanged.
 -
 - For example, consider the following program:
 -
 -    mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
 -    mem[8] = 11
 -    mem[7] = 101
 -    mem[8] = 0
 -
 - This program starts by specifying a bitmask (mask = ....). The mask it
 - specifies will overwrite two bits in every written value: the 2s bit is
 - overwritten with 0, and the 64s bit is overwritten with 1.
 -
 - The program then attempts to write the value 11 to memory address 8. By
 - expanding everything out to individual bits, the mask is applied as follows:
 -
 -    value:  000000000000000000000000000000001011  (decimal 11)
 -    mask:   XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
 -    result: 000000000000000000000000000001001001  (decimal 73)
 -
 - So, because of the mask, the value 73 is written to memory address 8 instead.
 - Then, the program tries to write 101 to address 7:
 -
 -    value:  000000000000000000000000000001100101  (decimal 101)
 -    mask:   XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
 -    result: 000000000000000000000000000001100101  (decimal 101)
 -
 - This time, the mask has no effect, as the bits it overwrote were already the
 - values the mask tried to set. Finally, the program tries to write 0 to
 - address 8:
 -
 -    value:  000000000000000000000000000000000000  (decimal 0)
 -    mask:   XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
 -    result: 000000000000000000000000000001000000  (decimal 64)
 -
 - 64 is written to address 8 instead, overwriting the value that was there
 - previously.
 -
 - To initialize your ferry's docking program, you need the sum of all values
 - left in memory after the initialization program completes. (The entire 36-bit
 - address space begins initialized to the value 0 at every address.) In the
 - above example, only two values in memory are not zero - 101 (at address 7)
 - and 64 (at address 8) - producing a sum of 165.
 -
 - Execute the initialization program. What is the sum of all values left in
 - memory after it completes?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Bits as B
import qualified Data.Map as Map
import Data.Int (Int64)

main :: IO ()
main = defaultMain parseInput handleInput

data BitMask = BitMask ![(Int, Bool)] deriving (Show, Eq, Ord)

data Program = Program ![Instruction] deriving (Show, Eq, Ord)

data Instruction
    = SetMask !BitMask
    | SetMemory !Int64 !Int64
  deriving (Show, Eq, Ord)

data ExecutionState = ExecutionState
    { _memory  :: !(Map.Map Int64 Int64)
    , _mask    :: !BitMask
    } deriving (Show, Eq, Ord)

handleInput :: Program -> IO ()
handleInput = print . sum . Map.elems . _memory . execute

execute :: Program -> ExecutionState
execute (Program instructions) = foldl executeInstruction state instructions
  where
    memory = Map.empty
    mask = BitMask []
    state = ExecutionState memory mask

executeInstruction :: ExecutionState -> Instruction -> ExecutionState
executeInstruction state (SetMask mask) = state { _mask = mask }
executeInstruction state@(ExecutionState memory mask) (SetMemory address value) =
    state { _memory = memory' }
  where
    memory' = Map.insert address (applyMask mask value) memory

applyMask :: BitMask -> Int64 -> Int64
applyMask (BitMask []) n = n
applyMask (BitMask ((idx, True):replacements)) n =
    applyMask (BitMask replacements) $ B.setBit n idx
applyMask (BitMask ((idx, False):replacements)) n =
    applyMask (BitMask replacements) $ B.clearBit n idx

parseInput :: T.Text -> Either P.ParseError Program
parseInput = P.parse (parseProgram <* P.eof) ""

parseProgram :: P.Parsec T.Text () Program
parseProgram = Program <$> parseInstruction `P.endBy` P.newline

parseInstruction :: P.Parsec T.Text () Instruction
parseInstruction = P.char 'm' *> P.choice [parseSetMask, parseSetMem]
  where
    parseSetMask = SetMask <$> (P.string "ask = " *> parseBitMask)
    parseSetMem = SetMemory <$>
        (P.string "em[" *> P.int) <*>
        (P.string "] = " *> P.int)

parseBitMask :: P.Parsec T.Text () BitMask
parseBitMask = BitMask . map g . filter f . zip [35, 34..]
    <$> P.count 36 parseReplacement
  where
    parseReplacement = P.choice
        [ P.char '0' *> (pure $ Just False)
        , P.char '1' *> (pure $ Just True)
        , P.char 'X' *> (pure Nothing)
        ]

    f (_, Nothing) = False
    f _ = True

    g (n, Just replacement) = (n, replacement)
    g _ = error "Impossible case"
