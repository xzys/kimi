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
  signals ::    [Molecule],
  rate ::       Double
} deriving (Eq, Show)

type MoleculeState = M.Map Molecule Int

type SignalState = M.Map Molecule Double

data CellState = CellState {
  time ::       Double,
  molecules ::  MoleculeState
} deriving (Eq, Show)


-- helpers
dedup :: (Ord a) => [a] -> [a]
dedup = map head . group . sort


-- calculate the product of reactant and catalyst molecule counts
-- TODO account for when you have two same molecules reacting
propensity :: SignalState -> MoleculeState -> Reaction -> Double
propensity signals mols (Reaction _ rs cs _ xs r) =
  sp * (fromIntegral mp) where
    sp = product $ map (signals M.!) xs
    mp = product $ map (mols M.!) (rs++cs)


-- TODO possible speed improvement 
-- https://jaspervdj.be/posts/2013-11-21-random-element-frequency-list.html 
react :: [Reaction] -> SignalState -> CellState -> IO CellState
react rxs signals c@(CellState t mols) =
      -- calculate propensities
  let props = propensity signals mols `map` rxs
      total = sum props
      -- iterate through frequency list and select, expects r in (0, total)
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
      putStrLn $ (show mols ++ " " ++ show r1)
      rx <- return $ indexFreqs r1 (zip rxs props)
      mols' <- return $ applyReaction rx mols
      t' <- return $ updateTime r2
      return (CellState t' mols')


-- run until stop time
simulate' :: [Reaction] -> SignalState -> CellState -> Double -> IO CellState 
simulate' rxs signals c@(CellState t mols) stop
    | t < stop = do
        c' <- react rxs signals c
        simulate' rxs signals c' stop
    | otherwise = return c

-- assign stop time based on multiple of steady state
simulate :: [Reaction] -> MoleculeState -> SignalState -> Int -> IO CellState 
simulate rxs initMols signals n =
  let molset = dedup $ foldl (\agg (Reaction _ rs cs ps _ _) -> agg++rs++cs++ps) [] rxs
      c = CellState 0 $ M.fromList $ map (\a -> (a, M.findWithDefault 0 a initMols)) molset
      tstop = (fromIntegral n) / (minimum $ map rate rxs)
  in  simulate' rxs signals c tstop


reactions = 
  [ Reaction { gene = "gene1", reactants = ["g1"],       catalysts = [],      products = ["ga1"],      signals = ["s1"], rate = 0.1  }
  , Reaction { gene = "gene1", reactants = ["ga1"],      catalysts = [],      products = ["g1"],       signals = [],     rate = 0.05 }
  , Reaction { gene = "gene1", reactants = [],           catalysts = ["ga1"], products = ["m1"],       signals = [],     rate = 0.1  }
  , Reaction { gene = "gene1", reactants = ["m1"],       catalysts = [],      products = [],           signals = [],     rate = 0.01 }
  , Reaction { gene = "gene1", reactants = ["m1"],       catalysts = [],      products = ["p1"],       signals = [],     rate = 0.2  }
  , Reaction { gene = "gene1", reactants = ["p1"],       catalysts = [],      products = [],           signals = [],     rate = 0.01 }

  , Reaction { gene = "gene2", reactants = ["g2", "p1"], catalysts = [],      products = ["ga2"],      signals = ["s1"], rate = 0.1  }
  , Reaction { gene = "gene2", reactants = ["ga2"],      catalysts = [],      products = ["p1", "g2"], signals = [],     rate = 0.05 }
  , Reaction { gene = "gene2", reactants = [],           catalysts = ["ga2"], products = ["m2"],       signals = [],     rate = 0.1  }
  , Reaction { gene = "gene2", reactants = ["m2"],       catalysts = [],      products = [],           signals = [],     rate = 0.01 }
  , Reaction { gene = "gene2", reactants = ["m2"],       catalysts = [],      products = ["p2"],       signals = [],     rate = 0.2  }
  , Reaction { gene = "gene2", reactants = ["p2"],       catalysts = [],      products = [],           signals = [],     rate = 0.01 }
  ]

initMols = M.fromList [("g1", 1), ("g2", 1)]

signals1 = M.fromList [("s1", 0.1)]
signals2 = M.fromList [("s1", 0.1)]

-- TODO check performance of deterministric stdgen vs IO stdgen
main :: IO ()
main = do
  c <- simulate reactions initMols signals1 2
  putStrLn $ show c

