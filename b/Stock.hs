-- |

module Main where

import Data.Array.Repa
import Prelude hiding (zipWith)

main :: IO ()
main = undefined

type BuyDay = Int
type SellDay = Int

test :: Array U DIM1 Int
test = fromListUnboxed (Z :. 8) [0,0,2,9,8,10,1,10]

buildMatrix :: Array U DIM1 Int -> Array D DIM2 (BuyDay, SellDay, Int)
buildMatrix inp =
  traverse inp
           (\(Z :. x) -> ix2 x x)
           (\idx (Z :. b :. s) ->
              if s < b
                 then (b,s,-1)
                 else (b
                      ,s
                      ,idx (ix1 s) - idx (ix1 b)))

getSell :: (Monad m)
        => Array D DIM2 (BuyDay,SellDay,Int)
        -> m (Array U DIM1 (BuyDay,SellDay,Int))
getSell inp =
  foldP (\(b1,s1,p1) (b2,s2,p2) ->
           if p1 >= p2 -- Pick the earlier sell days
              then (b1,s1,p1)
              else (b2,s2,p2))
        (0,0,0)
        inp

getBuy inp =
  foldP (\(b1,s1,p1) (b2,s2,p2) ->
           if p1 > p2 -- pick the later buy days
              then (b1,s1,p1)
              else (b2,s2,p2))
        (0,0,0)
        inp

buySell :: Monad m => Array U DIM1 Int -> m (BuyDay, SellDay, Int)
buySell prices = do let mat = buildMatrix prices
                    foundSell <- getSell mat
                    foundBuy <- getBuy foundSell
                    return (foundBuy ! Z)


--buySell l = maximum (zipWith (-) l (scanl1 min l))
