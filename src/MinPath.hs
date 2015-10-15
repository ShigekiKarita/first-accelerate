module MinPath where

import Data.Array.Accelerate ((:.)((:.)), (!))
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as I

type Weight = A.Int32
type Graph = A.Array A.DIM2 Weight

-- |
-- >>> step (A.unit (A.constant 2)) (A.use (A.fromList (A.Z :.3 :.3) [1..9]))
-- let a0 = use (Array (Z :. 3 :. 3) [1,2,3,4,5,6,7,8,9]) in
-- let a1 = unit 2
-- in generate
--      (shape a0)
--      (\x0 -> let x1 = indexHead x0 in
--              let x2 = Z in
--              let x3 = indexHead (indexTail x0)
--              in min (a0!(x2 :. x3 :. x1)
--                     ,let x4 = a1!x2
--                      in (a0!(x2 :. x3 :. x4)) + (a0!(x2 :. x4 :. x1))))
step :: A.Acc (A.Scalar Int) -> A.Acc Graph -> A.Acc Graph
step k g = A.generate (A.shape g) sp
    where
      k' = A.the k
      sp :: A.Exp A.DIM2 -> A.Exp Weight
      sp ix =
          let (A.Z :. i :. j) = A.unlift ix
          in min (g ! (A.index2 i j))
                 (g ! (A.index2 i k') + g ! (A.index2 k' j))

shortestPathsAcc :: Int -> A.Acc Graph -> A.Acc Graph
shortestPathsAcc n g0 = foldl1 (A.>->) steps g0
    where
      steps = [ step (A.unit (A.constant k)) | k <- [0 .. n-1]]

shortestPaths :: Graph -> Graph
shortestPaths g0 = I.run $ shortestPathsAcc n (A.use g0)
    where A.Z :. _ :. n = A.arrayShape g0
