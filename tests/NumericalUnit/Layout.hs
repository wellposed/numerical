{-# LANGUAGE DataKinds, TypeOperators #-}
module NumericalUnit.Layout (unitTestLayout) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.List (subsequences, find)
import Data.Maybe (listToMaybe)
import qualified Control.NumericalMonad.State.Strict as State
import Data.Proxy (Proxy(..))
import qualified Data.Vector.Generic as U
import Numerical.Array.Address
import Numerical.Array.Layout.Dense
import Numerical.Array.Layout.Sparse
import Numerical.Array.Layout.Builder
import Numerical.Array.Range
import Numerical.Array.Shape (Shape(..))
import qualified Numerical.Array.Shape as Shape
import Numerical.Array.Storage
import Numerical.Nat
import System.Timeout (timeout)
import Test.Hspec

type Row2 = Format Row 'Contiguous ('S ('S 'Z)) Unboxed
type Col2 = Format Column 'Contiguous ('S ('S 'Z)) Unboxed
type Sparse1 = Format DirectSparse 'Contiguous ('S 'Z) Unboxed
type Sparse2 = Format CompressedSparseRow 'Contiguous ('S ('S 'Z)) Unboxed

csr :: Int -> Int -> [Int] -> [Int] -> Sparse2
csr rows cols indices pointers = FormatContiguousCompressedSparseRow $
  FormatContiguousCompressedSparseInternal rows cols (U.fromList indices) (U.fromList pointers)

unitTestLayout :: Spec
unitTestLayout = do
  it "preserves noncommutative foldr1 order" $
    Shape.foldr1 (++) ("a" :* "b" :* "c" :* Nil) `shouldBe` "abc"
  it "State pure terminates and preserves state" $ do
    result <- timeout 1000000 (evaluate (State.runState (pure (7 :: Int)) (3 :: Int)))
    result `shouldBe` Just (7, 3)
  it "dense successor does not invoke partial numeric literals" $ do
    let f = FormatDirectContiguous 3 :: Format Direct 'Contiguous ('S 'Z) Unboxed
    nextAddr f (Address 0) `shouldBe` Just (Address 1)
    nextAddr f (Address 2) `shouldBe` Nothing
  it "includes singleton dense extents and counts endpoints" $ do
    let f = FormatRowContiguous (1 :* 2 :* Nil) :: Row2
    fmap (addressPopCount f) (addressRange f) `shouldBe` Just 2
    addressPopCount f (Range (Address 0) (Address 0)) `shouldBe` 1
    addressRange (FormatRowContiguous (0 :* 2 :* Nil) :: Row2) `shouldBe` Nothing
  it "dense seek includes first and final coordinates" $ do
    let f = FormatDirectStrided 3 2 :: Format Direct 'Strided ('S 'Z) Unboxed
    seek f (0 :* Nil) Nothing `shouldBe` Just (0 :* Nil, Address 0)
    seek f (2 :* Nil) Nothing `shouldBe` Just (2 :* Nil, Address 4)
  it "row and column order follow their actual addresses" $ do
    let row = FormatRowContiguous (2 :* 2 :* Nil) :: Row2
        col = FormatColumnContiguous (2 :* 2 :* Nil) :: Col2
    toAddress row (1 :* 0 :* Nil) `shouldBe` Just (Address 1)
    toAddress row (0 :* 1 :* Nil) `shouldBe` Just (Address 2)
    toAddress col (0 :* 1 :* Nil) `shouldBe` Just (Address 1)
    toAddress col (1 :* 0 :* Nil) `shouldBe` Just (Address 2)
    compareIndex (Proxy :: Proxy Row2) (1 :* 0 :* Nil) (0 :* 1 :* Nil) `shouldBe` LT
    compareIndex (Proxy :: Proxy Col2) (0 :* 1 :* Nil) (1 :* 0 :* Nil) `shouldBe` LT
  it "allocates the full row and column shape product" $ do
    (_, row) <- buildFormatPure (2 :* 3 :* Nil) (Proxy :: Proxy Row2) (7 :: Int) Nothing
    (_, col) <- buildFormatPure (2 :* 3 :* Nil) (Proxy :: Proxy Col2) (7 :: Int) Nothing
    U.toList row `shouldBe` replicate 6 7
    U.toList col `shouldBe` replicate 6 7
  it "rank-one sparse handles zero, final entry, empty support and inclusive seek" $ do
    let f = FormatDirectSparseContiguous 5 0 (U.fromList [0,2,4]) :: Sparse1
        empty = FormatDirectSparseContiguous 5 0 U.empty :: Sparse1
    toAddress f (0 :* Nil) `shouldBe` Just (Address 0)
    nextAddr f (Address 2) `shouldBe` Nothing
    seek f (2 :* Nil) Nothing `shouldBe` Just (2 :* Nil, Address 1)
    seek f (4 :* Nil) Nothing `shouldBe` Just (4 :* Nil, Address 2)
    seek empty (0 :* Nil) Nothing `shouldBe` Nothing
    toAddress empty (0 :* Nil) `shouldBe` Nothing
    addressPopCount f (Range (Address 0) (Address 2)) `shouldBe` 3
  it "CSR lookup stays within the row and rejects negative coordinates" $ do
    let f = csr 2 5 [1,3] [0,1,2]
    toAddress f (3 :* 0 :* Nil) `shouldBe` Nothing
    toAddress f (4 :* 1 :* Nil) `shouldBe` Nothing
    toAddress f (0 :* (-1) :* Nil) `shouldBe` Nothing
    toAddress f ((-1) :* 0 :* Nil) `shouldBe` Nothing
    toAddress f (3 :* 1 :* Nil) `shouldBe` Just (SparseAddress 1 1)
    addressPopCount f (Range (SparseAddress 0 0) (SparseAddress 1 1)) `shouldBe` 2
  it "CSR seek ignores hints beyond the true lower bound" $ do
    let f = csr 1 5 [1,3] [0,2]
    seek f (1 :* 0 :* Nil) (Just (SparseAddress 0 1))
      `shouldBe` seek f (1 :* 0 :* Nil) Nothing
  it "CSR seek crosses long nonmonotone empty-row stretches" $ do
    let f = csr 201 1 [0] (replicate 101 0 ++ replicate 101 1)
    seek f (0 :* 1 :* Nil) Nothing `shouldBe` Just (0 :* 100 :* Nil, SparseAddress 100 0)
    seek f (0 :* 101 :* Nil) Nothing `shouldBe` Nothing
    seek f (0 :* 200 :* Nil) Nothing `shouldBe` Nothing
  it "CSR cumulative-count search handles nonzero offsets and a final-row match" $ do
    let f = csr 201 4 [1,2,3] ([0] ++ replicate 200 2 ++ [3])
    seek f (0 :* 1 :* Nil) Nothing `shouldBe` Just (3 :* 200 :* Nil, SparseAddress 200 2)
    seek f (0 :* 199 :* Nil) Nothing `shouldBe` Just (3 :* 200 :* Nil, SparseAddress 200 2)
    let empty = csr 201 4 [] (replicate 202 0)
    seek empty (0 :* 0 :* Nil) Nothing `shouldBe` Nothing
  it "CSR hybrid search works after buffer offset 97" $ do
    let f = csr 2 200 ([0..99] ++ [100,101]) [0,100,102]
    seek f (199 :* 1 :* Nil) Nothing `shouldBe` Nothing
    seek f (101 :* 1 :* Nil) Nothing `shouldBe` Just (101 :* 1 :* Nil, SparseAddress 1 101)
  it "all 3x3 CSR supports agree with an ordered-list oracle, with every valid hint" $
    forM_ (subsequences [(x,y) | y <- [0..2], x <- [0..2]]) $ \support -> do
      let counts = [length [() | (_,y') <- support, y' == y] | y <- [0..2]]
          f = csr 3 3 (Prelude.map fst support) (scanl (+) 0 counts)
          entries = [(x :* y :* Nil, SparseAddress y off) | (off,(x,y)) <- zip [0..] support]
          hints = Nothing : Prelude.map (Just . snd) entries
      forM_ [(x,y) | y <- [0..2], x <- [0..2]] $ \(x,y) -> do
        let query = x :* y :* Nil
            expected = listToMaybe [e | (xy,e) <- zip support entries, let (a,b) = xy, (b,a) >= (y,x)]
        toAddress f query `shouldBe` fmap snd (find ((== query) . fst) entries)
        forM_ hints $ \hint -> seek f query hint `shouldBe` expected
      forM_ (zip entries (Prelude.map Just (drop 1 entries) ++ [Nothing])) $ \((ix,addr),successor) -> do
        toIndex f addr `shouldBe` ix
        nextAddr f addr `shouldBe` fmap snd successor
      fmap (addressPopCount f) (addressRange f)
        `shouldBe` (if null entries then Nothing else Just (length entries))
