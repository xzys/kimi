{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Gillepsie where

import qualified System.Random        as R
import qualified Data.Map.Strict      as M
import           Data.Maybe              (isNothing, fromJust, fromMaybe)
import           Data.List               (sort, group)
import           Numeric                 (log1p)
import           Debug.Trace             (trace)
import           GHC.Generics            (Generic)

import           Data.Aeson           as A


type Molecule = String

data Reaction = Reaction {
  gene ::       String,
  reactants ::  [Molecule],
  catalysts ::  [Molecule],
  products ::   [Molecule],
  signals ::    [Molecule],
  rate ::       Double
} deriving (Generic, FromJSON, ToJSON, Eq, Show)

type MoleculeState = M.Map Molecule Int

type SignalState = M.Map Molecule Double

data CellState = CellState {
  time ::       Double,
  molecules ::  MoleculeState
} deriving (Generic, FromJSON, ToJSON, Eq, Show)


-- helpers
dedup :: (Ord a) => [a] -> [a]
dedup = map head . group . sort


--------------------------------------------------------------------------------
-- calculate the product of reactant and catalyst molecule counts
-- TODO account for when you have two same molecules reacting
propensity :: SignalState -> MoleculeState -> Reaction -> Double
propensity signals mols (Reaction _ rs cs _ xs rate) =
  rate * sp * (fromIntegral mp) where
    sp = product $ map (signals M.!) xs
    mp = product $ map (mols M.!) (rs++cs)


-- TODO possible speed improvement 
-- https://jaspervdj.be/posts/2013-11-21-random-element-frequency-list.html 
react :: [Reaction] -> SignalState -> CellState -> IO CellState
react rxs signals c@(CellState t mols) =
      -- calculate propensities
  let props = propensity signals mols `map` rxs
      total = sum props
      -- iterate through frequency list and select reaction that occured
      indexFreqs _  [] = error "no reactions"
      indexFreqs r1 ((rx, f):fs)
          | r1 < f = rx
          | otherwise = indexFreqs (r1 - f) fs
      -- add all products, subtract all reactants
      applyReaction (Reaction _ rs _ ps _ _) mols = 
          flip (foldr (M.adjust (-1 +))) rs
               (foldr (M.adjust (1 +)) mols ps)
      -- update time, expects r in (0, total)
      updateTime r2 = t + ((log1p $ 1.0 / r2) / total)
  in do
      r1 <- R.randomRIO (0, total) :: IO Double
      r2 <- R.randomRIO (0, 1) :: IO Double
      -- putStrLn $ (show r1 ++ "\t " ++ show props)
      rx <- return $ indexFreqs r1 (zip rxs props)
      mols' <- return $ applyReaction rx mols
      t' <- return $ updateTime r2
      return (CellState t' mols')


--------------------------------------------------------------------------------
-- run until stop time
simulate' :: [Reaction] -> SignalState -> CellState -> Double -> IO CellState 
simulate' rxs signals c@(CellState t mols) stop
    | t < stop = do
        c' <- react rxs signals c
        simulate' rxs signals c' stop
    | otherwise = return c

-- assign stop time based on multiple of steady state
-- TODO calculate t_stop based on ode simulator
simulate :: [Reaction] -> MoleculeState -> SignalState -> Int -> IO CellState 
simulate rxs initMols signals nss =
  let molset = dedup $ foldl (\agg (Reaction _ rs cs ps _ _) -> agg++rs++cs++ps) [] rxs
      c = CellState 0 $ M.fromList $ map (\a -> (a, M.findWithDefault 0 a initMols)) molset
      tstop = (fromIntegral nss) / (minimum $ map rate rxs)
  in  simulate' rxs signals c tstop

-- simulate x times
duplicate :: [Reaction] -> MoleculeState -> SignalState -> Int -> Int -> IO [CellState]
duplicate rxs initMols signals nss reps = sequence $ run []
  where run results
          | length results < reps = run $ (simulate rxs initMols signals nss):results
          | otherwise = results


--------------------------------------------------------------------------------
-- setup and run
reactions = 
  [ Reaction { gene = "gene1", reactants = ["g1"],       catalysts = [],      products = ["ga1"],      signals = ["s1"], rate = 0.1  }
  , Reaction { gene = "gene1", reactants = ["ga1"],      catalysts = [],      products = ["g1"],       signals = [],     rate = 0.05 }
  , Reaction { gene = "gene1", reactants = [],           catalysts = ["ga1"], products = ["m1"],       signals = [],     rate = 0.1  }
  , Reaction { gene = "gene1", reactants = ["m1"],       catalysts = [],      products = [],           signals = [],     rate = 0.01 }
  , Reaction { gene = "gene1", reactants = ["m1"],       catalysts = [],      products = ["p1"],       signals = [],     rate = 0.2  }
  , Reaction { gene = "gene1", reactants = ["p1"],       catalysts = [],      products = [],           signals = [],     rate = 0.01 }

  , Reaction { gene = "gene2", reactants = ["g2", "p1"], catalysts = [],      products = ["ga2"],      signals = [],     rate = 0.1  }
  , Reaction { gene = "gene2", reactants = ["ga2"],      catalysts = [],      products = ["p1", "g2"], signals = [],     rate = 0.05 }
  , Reaction { gene = "gene2", reactants = [],           catalysts = ["ga2"], products = ["m2"],       signals = [],     rate = 0.1  }
  , Reaction { gene = "gene2", reactants = ["m2"],       catalysts = [],      products = [],           signals = [],     rate = 0.01 }
  , Reaction { gene = "gene2", reactants = [],           catalysts = ["m2"],  products = ["p2"],       signals = [],     rate = 0.2  }
  , Reaction { gene = "gene2", reactants = ["p2"],       catalysts = [],      products = [],           signals = [],     rate = 0.01 }
  ]

initMols = M.fromList [("g1", 1), ("g2", 1)]

signals1 = M.fromList [("s1", 0.1)]
signals2 = M.fromList [("s1", 0.1)]


{-
-- TODO check performance of deterministric stdgen vs IO stdgen
main :: IO ()
main = do
  putStrLn "starting run..."
  cs <- duplicate reactions initMols signals1 3 10000
  putStrLn $ "completed " ++ (show $ length cs) ++ " iterations."
  let dist = map (\(CellState _ mols) -> mols M.! "p2") cs
  let mean = (fromIntegral $ sum dist) / (fromIntegral $ length cs)
  putStrLn $ "mean p2: " ++ show mean
-}
