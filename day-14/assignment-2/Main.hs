{- For some reason, the sea port's computer system still can't communicate with
 - your ferry's docking program. It must be using version 2 of the decoder chip!
 -
 - A version 2 decoder chip doesn't modify the values being written at all.
 - Instead, it acts as a memory address decoder. Immediately before a value is
 - written to memory, each bit in the bitmask modifies the corresponding bit of
 - the destination memory address in the following way:
 -
 - * If the bitmask bit is 0, the corresponding memory address bit is unchanged.
 - * If the bitmask bit is 1, the corresponding memory address bit is
 -   overwritten with 1.
 - * If the bitmask bit is X, the corresponding memory address bit is floating.
 -
 - A floating bit is not connected to anything and instead fluctuates
 - unpredictably. In practice, this means the floating bits will take on all
 - possible values, potentially causing many memory addresses to be written all
 - at once!
 -
 - For example, consider the following program:
 -
 -    mask = 000000000000000000000000000000X1001X
 -    mem[42] = 100
 -    mask = 00000000000000000000000000000000X0XX
 -    mem[26] = 1
 -
 - When this program goes to write to memory address 42, it first applies the
 - bitmask:
 -
 -    address: 000000000000000000000000000000101010  (decimal 42)
 -    mask:    000000000000000000000000000000X1001X
 -    result:  000000000000000000000000000000X1101X
 -
 - After applying the mask, four bits are overwritten, three of which are
 - different, and two of which are floating. Floating bits take on every
 - possible combination of values; with two floating bits, four actual memory
 - addresses are written:
 -
 -    000000000000000000000000000000011010  (decimal 26)
 -    000000000000000000000000000000011011  (decimal 27)
 -    000000000000000000000000000000111010  (decimal 58)
 -    000000000000000000000000000000111011  (decimal 59)
 -
 - Next, the program is about to write to memory address 26 with a different
 - bitmask:
 -
 -    address: 000000000000000000000000000000011010  (decimal 26)
 -    mask:    00000000000000000000000000000000X0XX
 -    result:  00000000000000000000000000000001X0XX
 -
 - This results in an address with three floating bits, causing writes to eight
 - memory addresses:
 -
 -    000000000000000000000000000000010000  (decimal 16)
 -    000000000000000000000000000000010001  (decimal 17)
 -    000000000000000000000000000000010010  (decimal 18)
 -    000000000000000000000000000000010011  (decimal 19)
 -    000000000000000000000000000000011000  (decimal 24)
 -    000000000000000000000000000000011001  (decimal 25)
 -    000000000000000000000000000000011010  (decimal 26)
 -    000000000000000000000000000000011011  (decimal 27)
 -
 - The entire 36-bit address space still begins initialized to the value 0 at
 - every address, and you still need the sum of all values left in memory at the
 - end of the program. In this example, the sum is 208.
 -
 - Execute the initialization program using an emulator for a version 2 decoder
 - chip. What is the sum of all values left in memory after it completes?
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

data Action = DoNothing | SetOne | Floating deriving (Show, Eq, Ord)

data BitMask = BitMask ![(Int, Action)] deriving (Show, Eq, Ord)

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
    addrs = addresses mask address
    memory' = foldr (\k mem -> Map.insert k value mem) memory addrs

addresses :: BitMask -> Int64 -> [Int64]
addresses (BitMask actions) n = applyFloats floats $ applyOnes ones n
  where
    ones = map fst . filter (\(_, ac) -> ac == SetOne) $ actions
    floats = map fst . filter (\(_, ac) -> ac == Floating) $ actions

applyOnes :: [Int] -> Int64 -> Int64
applyOnes xs n = foldl B.setBit n xs

applyFloats :: [Int] -> Int64 -> [Int64]
applyFloats [] n = [n]
applyFloats (x:xs) n = applyFloats xs zero ++ applyFloats xs one
  where
    zero = B.clearBit n x
    one = B.setBit n x

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
parseBitMask = BitMask . zip [35, 34..] <$> P.count 36 parseReplacement
  where
    parseReplacement = P.choice
        [ P.char '0' *> pure DoNothing
        , P.char '1' *> pure SetOne
        , P.char 'X' *> pure Floating
        ]
