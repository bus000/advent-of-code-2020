{- --- Day 21: Allergen Assessment ---
 -
 - You reach the train's last stop and the closest you can get to your vacation
 - island without getting wet. There aren't even any boats here, but nothing can
 - stop you now: you build a raft. You just need a few days' worth of food for
 - your journey.
 -
 - You don't speak the local language, so you can't read any ingredients lists.
 - However, sometimes, allergens are listed in a language you do understand. You
 - should be able to use this information to determine which ingredient contains
 - which allergen and work out which foods are safe to take with you on your
 - trip.
 -
 - You start by compiling a list of foods (your puzzle input), one food per
 - line. Each line includes that food's ingredients list followed by some or all
 - of the allergens the food contains.
 -
 - Each allergen is found in exactly one ingredient. Each ingredient contains
 - zero or one allergen. Allergens aren't always marked; when they're listed (as
 - in (contains nuts, shellfish) after an ingredients list), the ingredient that
 - contains each listed allergen will be somewhere in the corresponding
 - ingredients list. However, even if an allergen isn't listed, the ingredient
 - that contains that allergen could still be present: maybe they forgot to
 - label it, or maybe it was labeled in a language you don't know.
 -
 - For example, consider the following list of foods:
 -
 -    mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
 -    trh fvjkl sbzzf mxmxvkd (contains dairy)
 -    sqjhc fvjkl (contains soy)
 -    sqjhc mxmxvkd sbzzf (contains fish)
 -
 - The first food in the list has four ingredients (written in a language you
 - don't understand): mxmxvkd, kfcds, sqjhc, and nhms. While the food might
 - contain other allergens, a few allergens the food definitely contains are
 - listed afterward: dairy and fish.
 -
 - The first step is to determine which ingredients can't possibly contain any
 - of the allergens in any food in your list. In the above example, none of the
 - ingredients kfcds, nhms, sbzzf, or trh can contain an allergen. Counting the
 - number of times any of these ingredients appear in any ingredients list
 - produces 5: they all appear once each except sbzzf, which appears twice.
 -
 - Determine which ingredients cannot possibly contain any of the allergens in
 - your list. How many times do any of those ingredients appear?
 -}
module Main where

import AdventOfCode
import Util (converge)
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = defaultMain parseInput handleInput

type Ingredient = T.Text
type Allergen = T.Text

data Label = Label
    { labelIngredients :: ![Ingredient]
    , labelAllergens   :: ![Allergen]
    } deriving (Show, Eq, Ord)

data State = State
    { statePossible :: !(Map.Map Allergen [Set.Set Ingredient])
    , stateDefinite :: !(Map.Map Allergen Ingredient)
    } deriving (Show, Eq, Ord)

handleInput :: [Label] -> IO ()
handleInput labels
    = print
    . length
    . filter (`notElem` allergens)
    . allIngredients
    $ labels
  where
    allergens = dangerousIngredients labels

dangerousIngredients :: [Label] -> [Ingredient]
dangerousIngredients labels = Map.elems finalState
  where
    definite = Map.empty
    possible = Map.fromListWith (++) . concatMap mapKey $ labels
    mapKey (Label ings alls) = map (\al -> (al, [Set.fromList ings])) alls
    initialState = State possible definite
    State _ finalState = converge collapseSingles initialState

collapseSingles :: State -> State
collapseSingles (State possible definite) = State possible'' definite'
  where
    singles = Map.mapMaybe findSingle possible
    definite' = Map.union singles definite
    finishedAllergens = Map.keysSet singles
    finishedIngredients = Set.fromList $ Map.elems singles
    possible' = Map.withoutKeys possible finishedAllergens
    possible'' = Map.map (map (`Set.difference` finishedIngredients)) possible'

findSingle :: [Set.Set Ingredient] -> Maybe Ingredient
findSingle [] = Nothing
findSingle ings = toMaybe . Set.elems . foldr1 Set.intersection $ ings
  where
    toMaybe [x] = Just x
    toMaybe _ = Nothing

allIngredients :: [Label] -> [Ingredient]
allIngredients = concatMap labelIngredients

parseInput :: T.Text -> Either P.ParseError [Label]
parseInput = P.parse (parseLabels <* P.eof) ""

parseLabels :: P.Parsec T.Text () [Label]
parseLabels = parseLabel `P.endBy` P.newline

parseLabel :: P.Parsec T.Text () Label
parseLabel = Label
    <$> parseIngredients <* P.string "(contains "
    <*> parseAllergens <* P.char ')'
  where
    parseIngredients = parseWord `P.endBy` P.char ' '
    parseAllergens = parseWord `P.sepBy` P.string ", "
    parseWord = T.pack <$> P.many1 P.letter
