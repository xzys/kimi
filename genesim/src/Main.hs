module Main where

import qualified  System.Random      as R
import qualified  Data.Map.Strict    as M
import            Data.Maybe            (isNothing, fromJust, fromMaybe, maybeToList)
import            Data.List             (sort)
import            Numeric               (log1p)


data Molecule = String deriving (Eq, Show, Ord)

data Reaction = Reaction {
  gene ::       String,
  reactants ::  [Molecule],
  catalysts ::  [Molecule],
  products ::   [Molecule],
  rate ::       Double
} deriving (Eq, Show)

data CellState = CellState {
  time ::       Double,
  molecules ::  M.Map Molecule Int
}


-- calculate the product of reactant and catalyst molecule counts
-- TODO account for when you have two same molecules reacting
propensity :: M.Map Molecule Int -> Reaction -> Int 
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
      indexFreqs r1 ((x, f):xs)
          | r1 < f = x
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
simulate :: [Reaction] -> CellState -> Int -> IO CellState 
simulate rxs c n = do
  simulate' rxs c $ (fromIntegral n) / (minimum $ map rate rxs)


-- TODO check performance of deterministric stdgen vs IO stdgen
main :: IO ()
main = do
  -- stdgen <- R.mkStdGen $ lowerBits timestamp
  putStrLn "hello world"
