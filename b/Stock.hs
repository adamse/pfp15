-- |

module Main where

import Data.Array.Repa
import Prelude hiding (zipWith)

main :: IO ()
main = undefined


test :: Array U DIM1 Int
test = fromListUnboxed (Z :. 8) [0,0,2,9,8,10,1,10]

test2 :: Array U DIM1 Int
test2 = let l = [12, 11, 10, 8, 5, 8, 9, 6, 7, 7, 10, 7, 4, 2]
        in fromListUnboxed (Z :. length l) l

-- | Build matrix of all possible (buy, sell, profit) tuples
buildMatrix :: (Monad m) => Array U DIM1 Int -> m (Array U DIM2 (Int, Int, Int))
buildMatrix inp =
  computeP
    (traverse inp
              (\(Z :. x) -> ix2 x x)
              (\idx (Z :. b :. s) ->
                 if s < b
                    then (b,s,-1)
                    else (b
                         ,s
                         ,idx (ix1 s) - idx (ix1 b))))

-- | For each buy day find the earliest sell day with largest profit
getSell :: (Monad m)
        => Array U DIM2 (Int,Int,Int)
        -> m (Array U DIM1 (Int,Int,Int))
getSell inp =
  foldP (\(b1,s1,p1) (b2,s2,p2) ->
           if p1 >= p2 -- Pick the earlier sell days
              then (b1,s1,p1)
              else (b2,s2,p2))
        (0,0,0)
        inp

-- | For find the best and latest day to buy
getBuy :: (Monad m)
       => Array U DIM1 (Int,Int,Int)
       -> m (Array U DIM0 (Int,Int,Int))
getBuy inp =
  foldP (\(b1,s1,p1) (b2,s2,p2) ->
           if p1 > p2 -- pick the later buy days
              then (b1,s1,p1)
              else (b2,s2,p2))
        (0,0,0)
        inp

-- | Solve the problem
buySell :: Monad m => Array U DIM1 Int -> m (Int, Int, Int)
buySell prices = do mat <- buildMatrix prices
                    foundSell <- getSell mat
                    foundBuy <- getBuy foundSell
                    return (foundBuy ! Z)
