{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PolyKinds #-}

-- | Persistent arrays.
module Agda.Utils.Array.Persistent
  ( Array
  -- * Creation
  , generate
  , fromList
  -- * Operations
  , get
  , set
  -- * Folds
  , toList
  -- * Debugging
  , valid
  ) where

import GHC.Base

import Agda.Utils.Impossible

import Data.Vector.Mutable qualified as V
import Agda.Utils.IORef.Strict

import System.IO.Unsafe

data Array a = Array
  { arrayDefault :: ~a
  , arraySize    :: Int#
  , arrayData    :: MutVar# RealWorld (Data a)
  }

data Data a :: UnliftedType where
  Root :: MVar# RealWorld (MutableArray# RealWorld a) -> Data a
  -- ^ The underlying array, locked behind an MVar.
  Set   :: Int# -> a -> MutVar# RealWorld (Data a) -> Data a
  -- ^ A suspended call to @Array.set@.

instance (Show a) => Show (Array a) where
  showsPrec prec xs =
    showParen (prec > 10) $
    showString "fromList "
    . shows (I# (arraySize xs)) . showString " "
    . showList (toList xs) . showString " "
    . showsPrec 10 (arrayDefault xs)

--------------------------------------------------------------------------------
-- Creation

generate :: Int -> (Int -> a) -> a -> Array a
generate (I# len) f def =
  let !ref = unsafeGenerate# len f def
  in Array
  { arrayDefault = def
  , arraySize = len
  , arrayData = ref
  }
{-# INLINE generate #-}

unsafeGenerate# :: Int# -> (Int -> a) -> a -> MutVar# RealWorld (Data a)
unsafeGenerate# len f def =
  let loop i buff s0
        | isTrue# (i <=# len) =
          let !s1 = writeArray# buff i (f (I# i))
          in loop (i +# 1#) buff s0
        | otherwise = s0
      k = oneShot \s0 ->
        let !(# s1, buff #) = newArray# len def s0
            !(# s2, lock #) = newMVar# s1
            !s3 = loop 0# buff s2
            !s4 = putMVar# lock buff s3
        in newMutVar# (Root lock) s4
  in case runRW# k of
    (# _, newRef #) -> newRef
{-# NOINLINE unsafeGenerate# #-}

fromList :: Int -> [a] -> a -> Array a
fromList (I# len) xs def = Array
  { arrayDefault = def
  , arraySize = len
  , arrayData = unsafeFromList# len xs def
  }
{-# INLINE fromList #-}

unsafeFromList# :: Int# -> [a] -> a -> MutVar# RealWorld (Data a)
unsafeFromList# len xs def =
  let loop xs i buff s0
        | isTrue# (i <# len) =
          case xs of
            (x:xs) ->
              let !s1 = writeArray# buff i x s0
              in loop xs (i +# 1#) buff s1
            [] -> s0
        | otherwise = s0
      k = oneShot \s0 ->
        let !(# s1, buff #) = newArray# len def s0
            !(# s2, lock #) = newMVar# s1
            !s3 = loop xs 0# buff s2
            !s4 = putMVar# lock buff s3
        in newMutVar# (Root lock) s2
  -- We don't really care if multiple threads try to create an array
  -- at the same time.
  in case runRW# k of
    (# _, newRef #) -> newRef

--------------------------------------------------------------------------------
-- Operations

reroot#
  :: forall {rep :: RuntimeRep} (a :: Type) (r :: TYPE rep)
  . MutVar# RealWorld (Data a)
  -> (MVar# RealWorld (MutableArray# RealWorld a) -> MutableArray# RealWorld a -> State# RealWorld -> (# State# RealWorld, r #))
  -> State# RealWorld
  -> (# State# RealWorld, r #)
reroot# ref k s0 =
  case readMutVar# ref s0 of
    (# s1, Root lockedBuff #) ->
      -- Take the lock here; continuation is responsible for unlocking.
      let !(# s2, buff #) = takeMVar# lockedBuff s1
      in k lockedBuff buff s2
    (# s1, Set i new oldRef #) ->
      let k' = oneShot \lock buff s3 ->
            let -- We have the lock here, so it is safe to manipulate pointers.
              !(# s4, old #) = readArray# buff i s3
              !s5 = writeArray# buff i new s4
              !(# s6, oldData #) = readMutVar# oldRef s5
              !s7 = writeMutVar# ref oldData s6
              !s8 = writeMutVar# oldRef (Set i old ref) s7
             in k lock buff s8
      in reroot# oldRef k' s1

get :: Array a -> Int -> a
get xs (I# i)
  | isTrue# (i <# arraySize xs) = unsafeGet# (arrayData xs) i
  | otherwise = arrayDefault xs
{-# INLINE get #-}

unsafeGet# :: MutVar# RealWorld (Data a) -> Int# -> a
unsafeGet# ref i =
  let k = oneShot \lock buff s0 ->
        let !(# s1, x #) = readArray# buff i s0
            !s2 = putMVar# lock buff s1
        in (# s2, x #)
  in case runRW# (reroot# ref k) of
    (# _, r #) -> lazy r
{-# NOINLINE unsafeGet# #-}

unsafeSet :: MutVar# RealWorld (Data a) -> Int# -> a -> MutVar# RealWorld (Data a)
unsafeSet ref i new =
  let k = oneShot \lock buff s0 ->
        let !(# s1, old #) = readArray# buff i s0
            !s2 = writeArray# buff i new s1
            !(# s3, newRef #) = newMutVar# (Root lock) s2
            !s4 = writeMutVar# ref (Set i old newRef) s3
            !s5 = putMVar# lock buff s4
        in (# s5, newRef #)
  in case runRW# (reroot# ref k) of
    (# _, newRef #) -> newRef
{-# NOINLINE unsafeSet #-}

set :: Array a -> Int -> a -> Array a
set xs (I# i) x
  | isTrue# (i <# arraySize xs) =
    let !newRef = unsafeSet (arrayData xs) i x
    in xs { arrayData = newRef }
  | otherwise = xs
{-# INLINE set #-}

--------------------------------------------------------------------------------
-- Folds

unsafeToList# :: MutVar# RealWorld (Data a) -> Int# -> [a]
unsafeToList# ref len =
  let loop i !acc lock buff s0
        | isTrue# (i >=# 0#) =
          let !(# s1, x #) = readArray# buff i s0
          in loop (i -# 1#) (x:acc) lock buff s1
        | otherwise =
          let !s1 = putMVar# lock buff s0
          in (# s1, acc #)
  in case runRW# (reroot# ref (loop (len -# 1#) [])) of
    (# _, xs #) -> lazy xs
{-# NOINLINE unsafeToList# #-}

toList :: Array a -> [a]
toList xs = unsafeToList# (arrayData xs) (arraySize xs)
{-# INLINE toList #-}

--------------------------------------------------------------------------------
-- Debugging

-- | Check that all invariants of an 'Array' are upheld.
valid :: Array a -> Bool
valid xs =
  -- We don't really care if multiple threads try to validate an
  -- array at the same time, so no need to mark this as noDuplicate.
  case runRW# (unsafeValid# (arraySize xs) (arrayData xs)) of
    (# _, ans #) -> ans
{-# NOINLINE valid #-}

unsafeValid# :: Int# -> MutVar# RealWorld (Data a) -> State# RealWorld -> (# State# RealWorld, Bool #)
unsafeValid# len ref s0 =
  let !(# s1, d #) = readMutVar# ref s0
  in case d of
    Root lock ->
      let !(# s2, buff #) = readMVar# lock s1
      in (# s2, isTrue# (sizeofMutableArray# buff ==# len) #)
    Set i _ oldRef
      | isTrue# (0# <=# i) && isTrue# (i <# len) -> unsafeValid# len oldRef s1
      | otherwise -> (# s1, False #)
{-# NOINLINE unsafeValid# #-}
