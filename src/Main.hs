module Main where

import Data.Array.Accelerate ((:.)((:.)), (!))
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.CUDA as I


--- <Accelerate EDSL の仕組み>
-- 6.1
-- Haskell コード -> 独自の内部データ構造 -> accelerate-cuda -> cuda コード
-- GPU がなくても Accelerate のインタプリタが動く (遅い)



--- 6.2 配列と添字
_   = A.fromList (A.Z :. 10)   [1..10] :: A.Vector Int          -- 10 次元ベクタ
arr = A.fromList (A.Z :. 3 :. 5 ) [1..]   :: A.Array A.DIM2 Int -- 3 x 5 行列
i   = A.indexArray arr (A.Z :. 2 :. 1)     :: Int               -- 添字アクセス
-- Array の Array は作れないが ... Tuple は OK
_   = A.fromList (A.Z :. 2 :. 3) (Prelude.zip [1..] [1..]) :: A.Array A.DIM2 (Int, Int)



sec6_3 = do -- 単純な Accelerate 計算を実行する
  print $ I.run $ A.map (+1) (A.use arr)
  print $ I.run $ A.map (^2) (A.use arr)
  -- <Interpreter 上で実行>
  -- I.run :: Arrays a => Acc a -> a -- A.use の逆

  -- <Acc (GPU) 配列を map>
  -- A.map
  --   :: (Elt b, Elt a, Shape ix) =>
  --      (Exp a -> Exp b) -> -- GPU 上での計算, 整数定数やNumはCPUの計算がそのまま動く
  --      Acc (Array ix a) -> -- GPU 上での配列
  --      Acc (Array ix b)    -- GPU 上での配列

  -- <Haskell (CPU) 配列を Acc (GPU) 配列に変換>
  -- A.use :: Arrays arrays => arrays -> Acc arrays


sec6_4 = -- スカラ配列 (配列ではなく単一の値)
  print $ I.run $ A.unit (3 :: A.Exp Int)
  -- A.unit :: A.Elt e => A.Exp e -> A.Acc (A.Scalar e)
  -- A.the  :: A.Elt e => A.Acc (A.Scalar e) -> A.Exp e


sec6_5 = do  -- 配列に添字でアクセスする
  print $ I.run $ A.unit (A.use arr ! A.index2 2 2)
  let arr = A.fromList (A.Z :. 10) [1..] :: A.Array A.DIM1 Int
  print $ I.run $ A.unit (A.use arr ! A.index1 3)
  -- (!)
  -- :: (A.Elt e, A.Shape ix) =>
  --    A.Acc (A.Array ix e) -> A.Exp ix -> A.Exp e

  -- TODO: 範囲アクセスはないのか ?


sec6_6 = do -- Acc の中で配列を作る
  let garr = I.run $ A.fill (A.index2 3 5) 13 :: A.Array A.DIM2 Int
  print garr
  -- A.fill
  -- :: (A.Elt e, A.Shape sh) =>
  --    A.Exp sh -> A.Exp e -> A.Acc (A.Array sh e)
  let garr = I.run $ A.enumFromN (A.index2 3 5) 7 :: A.Array A.DIM2 Int
  print garr
  -- A.enumFromN
  -- :: (A.Elt e, A.Shape sh, A.IsNum e) =>
  --    A.Exp sh -> A.Exp e -> A.Acc (A.Array sh e)
  let garr = I.run $ A.enumFromStepN (A.index2 3 5) 15 (-1) :: A.Array A.DIM2 Int
  print garr
  -- A.enumFromStepN
  -- :: (A.Elt e, A.Shape sh, A.IsNum e) =>
  --    A.Exp sh -> A.Exp e -> A.Exp e -> A.Acc (A.Array sh e)

  -- 一般化された方法
  let garr = I.run $ A.generate (A.index2 3 5)
             (\ix -> let A.Z :. y :. x = A.unlift ix in x * (y + 1))
                 :: A.Array A.DIM2 Int
  print garr
  -- A.generate
  -- :: (A.Elt a, A.Shape ix) =>
  --    A.Exp ix -> (A.Exp ix -> A.Exp a) -> A.Acc (A.Array ix a)
  -- A.unlift :: A.Unlift c e => c (A.Plain e) -> e
  -- A.lift   :: A.Lift   c e => e -> c (A.Plain e)


sec6_7 = do -- Array の zip
  let a = A.enumFromN (A.index2 2 3) 1           :: A.Acc (A.Array A.DIM2 Int)
      b = A.enumFromStepN (A.index2 2 3) 6  (-1) :: A.Acc (A.Array A.DIM2 Int)
  print $ I.run $ a
  print $ I.run $ b
  print $ I.run $ A.zipWith (+) a b
  -- 長さが違うと短いほうに揃う

sec6_8 = do
  let i = 7 :: Int
      c = A.constant i :: A.Exp Int
  print $ c



main = do
  -- sec6_3
  -- sec6_4
  -- sec6_5
  -- sec6_6
  sec6_7
  sec6_8
