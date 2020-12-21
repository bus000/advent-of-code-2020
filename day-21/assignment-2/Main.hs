{- Now that you've isolated the inert ingredients, you should have enough
 - information to figure out which ingredient contains which allergen.
 -
 - In the above example:
 -
 -    mxmxvkd contains dairy.
 -    sqjhc contains fish.
 -    fvjkl contains soy.
 -
 - Arrange the ingredients alphabetically by their allergen and separate them by
 - commas to produce your canonical dangerous ingredient list. (There should not
 - be any spaces in your canonical dangerous ingredient list.) In the above
 - example, this would be mxmxvkd,sqjhc,fvjkl.
 -
 - Time to stock your raft with supplies. What is your canonical dangerous
 - ingredient list?
 -}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import AdventOfCode
import Util (converge)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Parsec as P
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as L

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
handleInput
    = T.putStrLn
    . canonicalIngredients
    . dangerousIngredients

canonicalIngredients :: [(Allergen, Ingredient)] -> T.Text
canonicalIngredients = T.intercalate "," . map snd . L.sortOn fst

dangerousIngredients :: [Label] -> [(Allergen, Ingredient)]
dangerousIngredients labels = Map.toList finalState
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
