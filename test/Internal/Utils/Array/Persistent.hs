{-# LANGUAGE TemplateHaskell #-}

module Internal.Utils.Array.Persistent ( tests ) where

import Test.QuickCheck

import Agda.Utils.Array.Persistent (Array)
import Agda.Utils.Array.Persistent qualified as A

import Internal.Helpers

------------------------------------------------------------------------
-- Arbitrary
------------------------------------------------------------------------

-- instance (Arbitrary a) => Arbitrary (Array a) where
--   arbitrary = A.fromList <$> (chooseInt (0, 100)) <*> arbitrary <*> arbitrary
--   -- [TODO: Reed M, 27/03/2026] This shrink instance is not
--   -- optimal, but it doesn't impact the runtime nor memory usage
--   -- of the test suite very much: if all goes well, it will never
--   -- get called :)
--   shrink xs =
--     fmap (\xs' -> A.fromList (length xs') xs' (A.def xs)) $ shrink $ A.toList xs

arrayOf :: Gen Int -> Gen a -> Gen (Array a)
arrayOf genLen genA = A.fromList <$> genLen <*> listOf genA <*> genA

-- [TODO: Reed M, 27/03/2026] This is not
-- optimal, but it doesn't impact the runtime nor memory usage
-- of the test suite very much: if all goes well, it will never
-- get called :)
shrinkArray :: (a -> [a]) -> Array a -> [Array a]
shrinkArray f xs =
  [ A.fromList (length xs') xs' d
  | xs' <- shrinkList f (A.toList xs)
  , d <- f (A.getDefault xs)
  ]

forAllInBounds :: (Testable prop) => Array a -> (Int -> prop) -> Property
forAllInBounds xs p
  | null xs = property True
  | otherwise = forAll (chooseInt (0, length xs - 1)) p

------------------------------------------------------------------------
-- Basic Validity
------------------------------------------------------------------------

prop_valid_fromList :: Property
prop_valid_fromList =
  forAllShrink (chooseInt (0, 10)) (\i -> [0..(i `div` 2) - 1]) \len ->
  forAllShrink (listOf (chooseInt (0, 10))) shrink \xs ->
  forAllShrink (chooseInt (0, 10)) shrink \def ->
  property $ A.valid (A.fromList len xs def)

prop_length_fromList :: Property
prop_length_fromList =
  forAllShrink (chooseInt (0, 100)) (\i -> [0..(i `div` 2) - 1]) \len ->
  forAllShrink (listOf (chooseInt (0, 10))) shrink \xs ->
  forAllShrink (chooseInt (0, 10)) shrink \def ->
  length (A.fromList len xs def) === len

------------------------------------------------------------------------
-- Equality
------------------------------------------------------------------------

prop_eq_refl :: Property
prop_eq_refl =
  forAll (arrayOf (chooseInt (1, 10)) (chooseInt (0, 10))) \xs ->
  xs === xs

prop_eq_get :: Property
prop_eq_get =
  forAll (arrayOf (chooseInt (1, 10)) (chooseInt (0, 10))) \xs ->
  forAllInBounds xs \i ->
  A.get xs i === A.get xs i

prop_eq_set :: Property
prop_eq_set =
  forAll (arrayOf (chooseInt (1, 10)) (chooseInt (0, 10))) \xs ->
  forAllInBounds xs \i ->
  forAll (chooseInt (0, 10)) \x ->
  A.set xs i x === A.set xs i x

------------------------------------------------------------------------
-- Gets and Sets
------------------------------------------------------------------------

prop_get_set_same :: Property
prop_get_set_same =
  forAllShrink (arrayOf (chooseInt (1, 10)) (chooseInt (0, 10))) (shrinkArray pure) \xs ->
  forAllInBounds xs \i ->
  forAll (chooseInt (0, 10)) \x ->
  A.get (A.set xs i x) i === x

prop_get_set_other :: Property
prop_get_set_other =
  forAllShrink (arrayOf (chooseInt (1, 10)) (chooseInt (0, 10))) (shrinkArray pure) \xs ->
  forAllInBounds xs \i ->
  forAllInBounds xs \j ->
  forAll (chooseInt (0, 10)) \x ->
  (i /= j) ==> A.get (A.set xs i x) j === A.get xs j

prop_set_set :: Property
prop_set_set =
  forAll (arrayOf (chooseInt (1, 10)) (chooseInt (0, 10))) \xs ->
  forAllInBounds xs \i ->
  forAll (chooseInt (0, 10)) \x ->
  forAll (chooseInt (0, 10)) \y ->
  A.set (A.set xs i x) i y === A.set xs i y

prop_set_get :: Property
prop_set_get =
  forAll (arrayOf (chooseInt (1, 10)) (chooseInt (0, 10))) \xs ->
  forAllInBounds xs \i ->
  whenFail' (putStrLn (A.unsafeDebugShow xs)) $
  A.set xs i (A.get xs i) === xs

------------------------------------------------------------------------
-- All tests
------------------------------------------------------------------------

-- Template Haskell hack to make the following $allProperties work
-- under ghc-7.8.
return [] -- KEEP!

tests :: TestTree
tests = testProperties "Internal.Utils.Array.Persistent" $allProperties
