-- |

module Main where

import Data.Array.Repa
import Prelude hiding (zipWith)

import Criterion.Main

main :: IO ()
main = defaultMain [bench "parallel" (nfIO (buySell datas))
                   ,bench "sequential" (nf buySellS datas)]
  where datas = test2

test :: Array U DIM1 Int
test = fromListUnboxed (Z :. 8) [0,0,2,9,8,10,1,10]

test2 :: Array U DIM1 Int
test2 = let l = [12, 11, 10, 8, 5, 8, 9, 6, 7, 7, 10, 7, 4, 2]
        in fromListUnboxed (Z :. length l) l

-- * Parallell solution

-- | Solve the problem
buySell :: Monad m => Array U DIM1 Int -> m (Int, Int, Int)
buySell prices = do mat <- buildMatrix prices
                    foundSell <- getSell mat
                    foundBuy <- getBuy foundSell
                    return (foundBuy ! Z)

-- | Build matrix of all possible (buy, sell, profit) tuples
buildMatrix :: (Monad m) => Array U DIM1 Int -> m (Array U DIM2 (Int, Int, Int))
buildMatrix inp =
  computeP
    (traverse inp
              (\(Z :. x) -> ix2 x x)
              (\idx (Z :. b :. s) ->
                 if s < b
                    then (b,s,-1) -- -1 profit if sell if before buy day
                    else (b
                         ,s
                         ,idx (ix1 s) - idx (ix1 b))))

-- | For each buy day find the earliest sell day with largest profit
getSell :: (Monad m)
        => Array U DIM2 (Int,Int,Int)
        -> m (Array U DIM1 (Int,Int,Int))
getSell inp =
  foldP (\c1@(_,_,p1) c2@(_,_,p2) ->
           if p1 >= p2 -- Pick the earlier sell days
              then c1
              else c2)
        (0,0,0)
        inp

-- | For find the best and latest day to buy
getBuy :: (Monad m)
       => Array U DIM1 (Int,Int,Int)
       -> m (Array U DIM0 (Int,Int,Int))
getBuy inp =
  foldP (\c1@(_,_,p1) c2@(_,_,p2) ->
           if p1 > p2 -- Pick the earlier sell days
              then c1
              else c2)
        (0,0,0)
        inp

-- * Sequential solution

buySellS :: Array U DIM1 Int -> (Int, Int, Int)
buySellS prices = let mat = buildMatrixS prices
                      foundSell = getSellS mat
                      foundBuy = getBuyS foundSell
                  in foundBuy ! Z


-- | Build matrix of all possible (buy, sell, profit) tuples
buildMatrixS :: Array U DIM1 Int -> (Array U DIM2 (Int, Int, Int))
buildMatrixS inp =
  computeS
    (traverse inp
              (\(Z :. x) -> ix2 x x)
              (\idx (Z :. b :. s) ->
                 if s < b
                    then (b,s,-1) -- -1 profit if sell if before buy day
                    else (b
                         ,s
                         ,idx (ix1 s) - idx (ix1 b))))

-- | For each buy day find the earliest sell day with largest profit
getSellS :: Array U DIM2 (Int,Int,Int) -> (Array U DIM1 (Int,Int,Int))
getSellS inp =
  foldS (\c1@(_,_,p1) c2@(_,_,p2) ->
           if p1 >= p2 -- Pick the earlier sell days
              then c1
              else c2)
        (0,0,0)
        inp

-- | For find the best and latest day to buy
getBuyS :: Array U DIM1 (Int,Int,Int) -> (Array U DIM0 (Int,Int,Int))
getBuyS inp =
  foldS (\c1@(_,_,p1) c2@(_,_,p2) ->
           if p1 > p2 -- Pick the earlier sell days
              then c1
              else c2)
        (0,0,0)
        inp
