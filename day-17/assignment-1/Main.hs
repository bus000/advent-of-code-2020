{- --- Day 17: Conway Cubes ---
 -
 - As your flight slowly drifts through the sky, the Elves at the Mythical
 - Information Bureau at the North Pole contact you. They'd like some help
 - debugging a malfunctioning experimental energy source aboard one of their
 - super-secret imaging satellites.
 -
 - The experimental energy source is based on cutting-edge technology: a set of
 - Conway Cubes contained in a pocket dimension! When you hear it's having
 - problems, you can't help but agree to take a look.
 -
 - The pocket dimension contains an infinite 3-dimensional grid. At every
 - integer 3-dimensional coordinate (x,y,z), there exists a single cube which is
 - either active or inactive.
 -
 - In the initial state of the pocket dimension, almost all cubes start
 - inactive. The only exception to this is a small flat region of cubes (your
 - puzzle input); the cubes in this region start in the specified active (#) or
 - inactive (.) state.
 -
 - The energy source then proceeds to boot up by executing six cycles.
 -
 - Each cube only ever considers its neighbors: any of the 26 other cubes where
 - any of their coordinates differ by at most 1. For example, given the cube at
 - x=1,y=2,z=3, its neighbors include the cube at x=2,y=2,z=2, the cube at
 - x=0,y=2,z=3, and so on.
 -
 - During a cycle, all cubes simultaneously change their state according to the
 - following rules:
 -
 - * If a cube is active and exactly 2 or 3 of its neighbors are also active,
 -   the cube remains active. Otherwise, the cube becomes inactive.
 - * If a cube is inactive but exactly 3 of its neighbors are active, the cube
 -   becomes active. Otherwise, the cube remains inactive.
 -
 - The engineers responsible for this experimental energy source would like you
 - to simulate the pocket dimension and determine what the configuration of
 - cubes should be at the end of the six-cycle boot process.
 -
 - For example, consider the following initial state:
 -
 -    .#.
 -    ..#
 -    ###
 -
 - Even though the pocket dimension is 3-dimensional, this initial state
 - represents a small 2-dimensional slice of it. (In particular, this initial
 - state defines a 3x3x1 region of the 3-dimensional space.)
 -
 - Simulating a few cycles from this initial state produces the following
 - configurations, where the result of each cycle is shown layer-by-layer at
 - each given z coordinate (and the frame of view follows the active cells in
 - each cycle):
 -
 - Before any cycles:
 -
 -    z=0
 -    .#.
 -    ..#
 -    ###
 -
 -
 - After 1 cycle:
 -
 -    z=-1
 -    #..
 -    ..#
 -    .#.
 -
 -    z=0
 -    #.#
 -    .##
 -    .#.
 -
 -    z=1
 -    #..
 -    ..#
 -    .#.
 -
 -
 - After 2 cycles:
 -
 -    z=-2
 -    .....
 -    .....
 -    ..#..
 -    .....
 -    .....
 -
 -    z=-1
 -    ..#..
 -    .#..#
 -    ....#
 -    .#...
 -    .....
 -
 -    z=0
 -    ##...
 -    ##...
 -    #....
 -    ....#
 -    .###.
 -
 -    z=1
 -    ..#..
 -    .#..#
 -    ....#
 -    .#...
 -    .....
 -
 -    z=2
 -    .....
 -    .....
 -    ..#..
 -    .....
 -    .....
 -
 -
 - After 3 cycles:
 -
 -    z=-2
 -    .......
 -    .......
 -    ..##...
 -    ..###..
 -    .......
 -    .......
 -    .......
 -
 -    z=-1
 -    ..#....
 -    ...#...
 -    #......
 -    .....##
 -    .#...#.
 -    ..#.#..
 -    ...#...
 -
 -    z=0
 -    ...#...
 -    .......
 -    #......
 -    .......
 -    .....##
 -    .##.#..
 -    ...#...
 -
 -    z=1
 -    ..#....
 -    ...#...
 -    #......
 -    .....##
 -    .#...#.
 -    ..#.#..
 -    ...#...
 -
 -    z=2
 -    .......
 -    .......
 -    ..##...
 -    ..###..
 -    .......
 -    .......
 -    .......
 -
 - After the full six-cycle boot process completes, 112 cubes are left in the
 - active state.
 -
 - Starting with your given initial configuration, simulate six cycles. How many
 - cubes are left in the active state after the sixth cycle?
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
neighbours (Position x y z) = do
    x' <- [x-1, x, x+1]
    y' <- [y-1, y, y+1]
    z' <- [z-1, z, z+1]

    -- Don't include the original position.
    M.guard $ x' /= x || y' /= y || z' /= z

    return $! Position x' y' z'

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
liftPosition (x, y) = Position x y 0
