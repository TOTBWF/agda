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
  Array# :: MutableArray# RealWorld a -> Data a
  Set#   :: Int# -> a -> MutVar# RealWorld (Data a) -> Data a

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
            !s2 = loop 0# buff s1
        in newMutVar# (Array# buff) s2
  -- We don't really care if multiple threads try to create an array
  -- at the same time.
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
            !s2 = loop xs 0# buff s1
        in newMutVar# (Array# buff) s2
  -- We don't really care if multiple threads try to create an array
  -- at the same time.
  in case runRW# k of
    (# _, newRef #) -> newRef

--------------------------------------------------------------------------------
-- Operations

reroot#
  :: forall {rep :: RuntimeRep} (a :: Type) (r :: TYPE rep)
  . MutVar# RealWorld (Data a)
  -> (MutableArray# RealWorld a -> State# RealWorld -> (# State# RealWorld, r #))
  -> State# RealWorld
  -> (# State# RealWorld, r #)
reroot# ref k s0 =
  -- We don't want this IO action being duplicated across
  -- multiple threads!
  case noDuplicate# s0 of
    s1 ->
      let !(# s2 , t #) = readMutVar# ref s0
      in case t of
        Array# buff -> k buff s2
        Set# i new oldRef ->
          let k' = oneShot \buff s3 ->
                let !(# s4, old #) = readArray# buff i s3
                    !s5 = writeArray# buff i new s4
                    !(# s6, oldData #) = readMutVar# oldRef s5
                    !s7 = writeMutVar# ref oldData s6
                    !s8 = writeMutVar# oldRef (Set# i old ref) s7
                 in k buff s8
          in reroot# oldRef k' s2

unsafeGet# :: MutVar# RealWorld (Data a) -> Int# -> a
unsafeGet# ref i =
  let k = oneShot \buff s0 -> readArray# buff i s0
  in case runRW# (reroot# ref \buff s0 -> readArray# buff i s0) of
    (# _, r #) -> lazy r
{-# NOINLINE unsafeGet# #-}

get :: Array a -> Int -> a
get xs (I# i)
  | isTrue# (i <# arraySize xs) = unsafeGet# (arrayData xs) i
  | otherwise = arrayDefault xs
{-# INLINE get #-}

unsafeSet# :: MutVar# RealWorld (Data a) -> Int# -> a -> MutVar# RealWorld (Data a)
unsafeSet# ref i new =
  let k = oneShot \buff s0 ->
        let !(# s1, old #) = readArray# buff i s0
            !s2 = writeArray# buff i new s1
            !(# s3, newRef #) = newMutVar# (Array# buff) s2
            !s4 = writeMutVar# ref (Set# i old newRef) s3
        in (# s4, newRef #)
  in case runRW# (reroot# ref k) of
    (# _, newRef #) -> newRef
{-# NOINLINE unsafeSet# #-}

set :: Array a -> Int -> a -> Array a
set xs (I# i) x
  | isTrue# (i <# arraySize xs) =
    let !newRef = unsafeSet# (arrayData xs) i x
    in xs { arrayData = newRef }
  | otherwise = xs
{-# INLINE set #-}

--------------------------------------------------------------------------------
-- Folds

unsafeToList# :: MutVar# RealWorld (Data a) -> Int# -> [a]
unsafeToList# ref len =
  let loop i !acc buff s0
        | isTrue# (i >=# 0#) =
          let !(# s1, x #) = readArray# buff i s0
          in loop (i -# 1#) (x:acc) buff s1
        | otherwise = (# s0, acc #)
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
    Array# buff -> (# s1, isTrue# (sizeofMutableArray# buff ==# len) #)
    Set# i _ oldRef
      | isTrue# (0# <=# i) && isTrue# (i <# len) -> unsafeValid# len oldRef s1
      | otherwise -> (# s1, False #)
{-# NOINLINE unsafeValid# #-}
