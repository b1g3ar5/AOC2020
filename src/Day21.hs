
{-# LANGUAGE FlexibleContexts #-}

module Day21 where


import Utils ( getLines, splitOnStr, intersections )
import Data.List ( foldl', intercalate, sort, sortOn )
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Bifunctor ( Bifunctor(first, second) )


type Ingredient = String
type Allergen = String
type Food = (S.Set Ingredient, S.Set Allergen)

parseLine :: String -> Food
parseLine s = (S.fromList $ words $ head pieces, S.fromList $ splitOnStr ", " $ init $ pieces!!1) 
  where
    pieces = splitOnStr " (contains " s


-- The set of ingredients that might contain the allergen
-- ie. the intersection of all the ingredients of each food
traceAllergen:: Allergen -> [Food] -> S.Set Ingredient
traceAllergen a foods = intersections ingredients
  where
    ingredients = fst <$> filter (\(is, as) -> a `S.member` as) foods


-- Given a list of potential ingredients for each allergen this works out
-- an allocation of allergens to ingredients
-- I should write a llibrary function for this - given a matrix of possibles
-- work out a set of allocations that work...
allocate :: [(Allergen, S.Set Ingredient)] -> [(Allergen, Ingredient)]
allocate = go [] 
  where
    go :: [(Allergen, Ingredient)] ->  [(Allergen, S.Set Ingredient)] -> [(Allergen, Ingredient)]
    go acc [] = acc
    go acc as = go ((a, head $ S.toList is):acc) newSets 
      where
        ((a, is):ts) = sortOn (S.size . snd) as
        newIngredient = head $ S.toList is 
        newAssoc = (a, newIngredient)
        newSets = second (S.delete newIngredient) <$> ts
  


day21 :: IO ()
day21 = do
  ls <- getLines 21
  let foods = parseLine <$> ls
      allergens = S.unions $ snd <$> foods
      ingredients = S.unions $ fst <$> foods

      -- These are the potentially unsafe ingredients, they could include an allergen
      ingredientWithAllergen = S.unions $ S.map (`traceAllergen` foods) allergens
      safeIngredients = ingredients S.\\ ingredientWithAllergen
      -- How many foods is an ingredient in
      countIngredient :: Ingredient-> Int
      countIngredient i = foldl' (\acc (is, _) -> if i `elem` is then acc+1 else acc) 0 foods

  putStrLn $ "Day21: part1: " ++ show (sum $ countIngredient <$> S.toList safeIngredients)

  let -- foods' is all the foods with the safe ingredients removed
      foods' = first (S.\\ safeIngredients) <$> foods  
      -- calculate the potential ingredients for each allergen again
      ingredientWithAllergen' :: S.Set (String, S.Set Ingredient)
      ingredientWithAllergen' = S.map (\a -> (a, a `traceAllergen` foods')) allergens

  putStrLn $ "Day21: part2: " ++ show (intercalate "," $ snd <$> sort (allocate $ S.toList ingredientWithAllergen'))

  return ()