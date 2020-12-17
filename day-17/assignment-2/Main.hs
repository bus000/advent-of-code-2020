{- For some reason, your simulated results don't match what the experimental
 - energy source engineers expected. Apparently, the pocket dimension actually
 - has four spatial dimensions, not three.
 -
 - The pocket dimension contains an infinite 4-dimensional grid. At every
 - integer 4-dimensional coordinate (x,y,z,w), there exists a single cube
 - (really, a hypercube) which is still either active or inactive.
 -
 - Each cube only ever considers its neighbors: any of the 80 other cubes where
 - any of their coordinates differ by at most 1. For example, given the cube at
 - x=1,y=2,z=3,w=4, its neighbors include the cube at x=2,y=2,z=3,w=3, the cube
 - at x=0,y=2,z=3,w=4, and so on.
 -
 - The initial state of the pocket dimension still consists of a small flat
 - region of cubes. Furthermore, the same rules for cycle updating still apply:
 - during each cycle, consider the number of active neighbors of each cube.
 -
 - For example, consider the same initial state as in the example above. Even
 - though the pocket dimension is 4-dimensional, this initial state represents a
 - small 2-dimensional slice of it. (In particular, this initial state defines a
 - 3x3x1x1 region of the 4-dimensional space.)
 -
 - Simulating a few cycles from this initial state produces the following
 - configurations, where the result of each cycle is shown layer-by-layer at
 - each given z and w coordinate:
 -
 - Before any cycles:
 -
 -    z=0, w=0
 -    .#.
 -    ..#
 -    ###
 -
 -
 - After 1 cycle:
 -
 -    z=-1, w=-1
 -    #..
 -    ..#
 -    .#.
 -
 -    z=0, w=-1
 -    #..
 -    ..#
 -    .#.
 -
 -    z=1, w=-1
 -    #..
 -    ..#
 -    .#.
 -
 -    z=-1, w=0
 -    #..
 -    ..#
 -    .#.
 -
 -    z=0, w=0
 -    #.#
 -    .##
 -    .#.
 -
 -    z=1, w=0
 -    #..
 -    ..#
 -    .#.
 -
 -    z=-1, w=1
 -    #..
 -    ..#
 -    .#.
 -
 -    z=0, w=1
 -    #..
 -    ..#
 -    .#.
 -
 -    z=1, w=1
 -    #..
 -    ..#
 -    .#.
 -
 -
 - After 2 cycles:
 -
 -    z=-2, w=-2
 -    .....
 -    .....
 -    ..#..
 -    .....
 -    .....
 -
 -    z=-1, w=-2
 -    .....
 -    .....
 -    .....
 -    .....
 -    .....
 -
 -    z=0, w=-2
 -    ###..
 -    ##.##
 -    #...#
 -    .#..#
 -    .###.
 -
 -    z=1, w=-2
 -    .....
 -    .....
 -    .....
 -    .....
 -    .....
 -
 -    z=2, w=-2
 -    .....
 -    .....
 -    ..#..
 -    .....
 -    .....
 -
 -    z=-2, w=-1
 -    .....
 -    .....
 -    .....
 -    .....
 -    .....
 -
 -    z=-1, w=-1
 -    .....
 -    .....
 -    .....
 -    .....
 -    .....
 -
 -    z=0, w=-1
 -    .....
 -    .....
 -    .....
 -    .....
 -    .....
 -
 -    z=1, w=-1
 -    .....
 -    .....
 -    .....
 -    .....
 -    .....
 -
 -    z=2, w=-1
 -    .....
 -    .....
 -    .....
 -    .....
 -    .....
 -
 -    z=-2, w=0
 -    ###..
 -    ##.##
 -    #...#
 -    .#..#
 -    .###.
 -
 -    z=-1, w=0
 -    .....
 -    .....
 -    .....
 -    .....
 -    .....
 -
 -    z=0, w=0
 -    .....
 -    .....
 -    .....
 -    .....
 -    .....
 -
 -    z=1, w=0
 -    .....
 -    .....
 -    .....
 -    .....
 -    .....
 -
 -    z=2, w=0
 -    ###..
 -    ##.##
 -    #...#
 -    .#..#
 -    .###.
 -
 -    z=-2, w=1
 -    .....
 -    .....
 -    .....
 -    .....
 -    .....
 -
 -    z=-1, w=1
 -    .....
 -    .....
 -    .....
 -    .....
 -    .....
 -
 -    z=0, w=1
 -    .....
 -    .....
 -    .....
 -    .....
 -    .....
 -
 -    z=1, w=1
 -    .....
 -    .....
 -    .....
 -    .....
 -    .....
 -
 -    z=2, w=1
 -    .....
 -    .....
 -    .....
 -    .....
 -    .....
 -
 -    z=-2, w=2
 -    .....
 -    .....
 -    ..#..
 -    .....
 -    .....
 -
 -    z=-1, w=2
 -    .....
 -    .....
 -    .....
 -    .....
 -    .....
 -
 -    z=0, w=2
 -    ###..
 -    ##.##
 -    #...#
 -    .#..#
 -    .###.
 -
 -    z=1, w=2
 -    .....
 -    .....
 -    .....
 -    .....
 -    .....
 -
 -    z=2, w=2
 -    .....
 -    .....
 -    ..#..
 -    .....
 -    .....
 -
 - After the full six-cycle boot process completes, 848 cubes are left in the
 - active state.
 -
 - Starting with your given initial configuration, simulate six cycles in a
 - 4-dimensional space. How many cubes are left in the active state after the
 - sixth cycle?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.Set as Set
import qualified Control.Monad as M
import qualified Data.List as L

main :: IO ()
main = defaultMain parseInput handleInput

data Position = Position
    { px :: !Int
    , py :: !Int
    , pz :: !Int
    , pw :: !Int
    } deriving (Show, Eq, Ord)

data ConwayCubes = ConwayCubes
    { ccActive :: !(Set.Set Position)
    } deriving (Show, Eq, Ord)

handleInput :: ConwayCubes -> IO ()
handleInput = print . Set.size . ccActive . runSimulation 6

runSimulation :: Int -> ConwayCubes -> ConwayCubes
runSimulation n = head . drop n . iterate step

step :: ConwayCubes -> ConwayCubes
step conway@(ConwayCubes active) = ConwayCubes active'
  where
    inactive = inactiveNeighbours conway
    active' = Set.union
        (applyInactive conway inactive) (applyActive conway active)

applyInactive :: ConwayCubes -> Set.Set Position -> Set.Set Position
applyInactive conway inactive = Set.filter (inactivePredicate conway) inactive

inactivePredicate :: ConwayCubes -> Position -> Bool
inactivePredicate conway position = len == 3
  where
    activeNeighbours = filter (isActive conway) . neighbours $ position
    len = length activeNeighbours

applyActive :: ConwayCubes -> Set.Set Position -> Set.Set Position
applyActive conway active = Set.filter (activePredicate conway) active

activePredicate :: ConwayCubes -> Position -> Bool
activePredicate conway position = len == 2 || len == 3
  where
    activeNeighbours = filter (isActive conway) . neighbours $ position
    len = length activeNeighbours

neighbours :: Position -> [Position]
neighbours (Position x y z w) = do
    x' <- [x-1, x, x+1]
    y' <- [y-1, y, y+1]
    z' <- [z-1, z, z+1]
    w' <- [w-1, w, w+1]

    -- Don't include the original position.
    M.guard $ x' /= x || y' /= y || z' /= z || w' /= w

    return $! Position x' y' z' w'

inactiveNeighbours :: ConwayCubes -> Set.Set Position
inactiveNeighbours (ConwayCubes active) = Set.difference allNeighbours active
  where
    allNeighbours
        = Set.fromList
        . concatMap neighbours
        . Set.toList
        $ active

isActive :: ConwayCubes -> Position -> Bool
isActive (ConwayCubes active) pos = pos `Set.member` active

parseInput :: T.Text -> Either P.ParseError ConwayCubes
parseInput = P.parse (parseSheet <* P.eof) ""

parseSheet :: P.Parsec T.Text () ConwayCubes
parseSheet = do
    conwayLines <- parseLine `P.endBy` P.newline
    M.guard $ not $ null conwayLines
    M.guard $ (length $ L.nub $ map length conwayLines) == 1

    let linesNumbered = zip [0..] conwayLines
        charsNumbered = concatMap (\(y, line) -> zip (zip [0..] (repeat y)) line) linesNumbered

    return $! fromChars charsNumbered
  where
    fromChars
        = ConwayCubes
        . Set.fromList
        . map (liftPosition . fst)
        . filter (\x -> snd x == '#')

parseLine :: P.Parsec T.Text () String
parseLine = P.many (P.choice [P.char '#', P.char '.'])

liftPosition :: (Int, Int) -> Position
liftPosition (x, y) = Position x y 0 0
