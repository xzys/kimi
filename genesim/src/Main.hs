module Main where

import qualified  System.Random      as R
import qualified  Data.Map.Strict    as M
import            Data.Maybe            (isNothing, fromJust, fromMaybe, maybeToList)
import            Data.List             (sort, group)
import            Numeric               (log1p)


type Molecule = String

data Reaction = Reaction {
  gene ::       String,
  reactants ::  [Molecule],
  catalysts ::  [Molecule],
  products ::   [Molecule],
  rate ::       Double
} deriving (Eq, Show)

type MoleculeState = M.Map Molecule Int

data CellState = CellState {
  time ::       Double,
  molecules ::  MoleculeState 
} deriving (Eq, Show)

-- helpers
dedup :: (Ord a) => [a] -> [a]
dedup = map head . group . sort


-- calculate the product of reactant and catalyst molecule counts
-- TODO account for when you have two same molecules reacting
propensity :: MoleculeState -> Reaction -> Int 
propensity mols (Reaction _ rs cs _ r) =
  product $ map (mols M.!) (rs++cs)


-- TODO possible speed improvement 
-- https://jaspervdj.be/posts/2013-11-21-random-element-frequency-list.html 
react :: [Reaction] -> CellState -> IO CellState
react rxs c@(CellState t mols) =
      -- calculate propensities
  let props = propensity mols `map` rxs
      total = sum props
      -- iterate through frequency list and select, expects r in (0, total)
      indexFreqs _  [] = error "no reactions"
      indexFreqs r1 ((rx, f):xs)
          | r1 < f = rx
          | otherwise = indexFreqs (r1 - f) xs
      -- add all products, subtract all reactants
      applyReaction (Reaction _ rs _ ps _) mols = 
          flip (foldr (M.adjust (-1 +))) rs
               (foldr (M.adjust (1 +)) mols ps)
      -- update time, expects r in (0, total)
      updateTime r2 = t + ((log1p $ 1.0 / r2) / fromIntegral total)
  in do
      r1 <- R.randomRIO (0, total - 1) :: IO Int
      r2 <- R.randomRIO (0, 1) :: IO Double
      putStrLn (show r1)
      rx <- return $ indexFreqs r1 (zip rxs props)
      mols' <- return $ applyReaction rx mols
      t' <- return $ updateTime r2
      return (CellState t' mols')


-- run until stop time
simulate' :: [Reaction] -> CellState -> Double -> IO CellState 
simulate' rxs c@(CellState t mols) stop
    | t < stop = do
        c' <- react rxs c
        simulate' rxs c' stop
    | otherwise = return c

-- assign stop time based on multiple of steady state
simulate :: [Reaction] -> Int -> IO CellState 
simulate rxs n =
  let mols = dedup $ foldl (\agg (Reaction _ rs cs ps _) -> agg++rs++cs++ps) [] rxs
      c = CellState 0 $ M.fromList $ map (\a -> (a, 0)) mols
      tstop = (fromIntegral n) / (minimum $ map rate rxs)
  in  simulate' rxs c tstop


reactions = [
   Reaction { gene = "gene1", reactants = ["g1"],  catalysts = ["s1"],  products = ["ga1"],  rate = 0.1  }
  ,Reaction { gene = "gene1", reactants = ["ga1"], catalysts = [],      products = ["g1"],   rate = 0.05 }
  ,Reaction { gene = "gene1", reactants = [],      catalysts = ["ga1"], products = ["m1"],   rate = 0.1  }
  ,Reaction { gene = "gene1", reactants = ["m1"],  catalysts = [],      products = [],       rate = 0.01 }
  ,Reaction { gene = "gene1", reactants = ["m1"],  catalysts = [],      products = ["p1"],   rate = 0.2  }
  ,Reaction { gene = "gene1", reactants = ["p1"],  catalysts = [],      products = [],       rate = 0.01 }
  ]


-- TODO check performance of deterministric stdgen vs IO stdgen
main :: IO ()
main = do
  c <- simulate reactions 2
  putStrLn $ show c

