{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}

-- | Persistent arrays.
module Agda.Utils.Array.Persistent
  ( Array
  -- * Creation
  , generate
  , fromList
  -- * Operations
  , get
  , set
  , getDefault
  , setDefault
  -- * Folds
  , toList
  , foldl'
  , foldr'
  -- * Debugging
  , valid
  , unsafeDebugShow
  ) where

import Prelude hiding (foldl')

import Agda.Utils.Impossible

import Data.Foldable (toList)
import Data.Kind

import GHC.Exts hiding (toList, fromList)
import GHC.IO

--------------------------------------------------------------------------------
-- Arrays

data Array a = Array
  { arrayDefault :: ~a
  -- ^ Default value to use for out-of-bounds reads.
  , arraySize    :: Int#
  -- ^ Size of the array.
  , arrayData    :: MutVar# RealWorld (ArrayData a)
  -- ^ Pointer into the persistence tree of the array.
  }

data ArrayData a :: UnliftedType where
  Root :: MVar# RealWorld (MutableArray# RealWorld a) -> ArrayData a
  -- ^ The underlying array, locked behind an MVar.
  Set   :: Int# -> ~a -> MutVar# RealWorld (ArrayData a) -> ArrayData a
  -- ^ A suspended call to @Array.set@.

instance (Show a) => Show (Array a) where
  showsPrec prec xs =
    showParen (prec > 10) $
    showString "fromList "
    . shows (I# (arraySize xs)) . showString " "
    . showList (toList xs) . showString " "
    . showsPrec 10 (arrayDefault xs)

instance (Eq a) => Eq (Array a) where
  xs == ys =
    arrayDefault xs == arrayDefault ys &&
    isTrue# (arraySize xs ==# arraySize ys) &&
    unsafeEq# (arraySize xs) (arrayData xs) (arrayData ys)

unsafeEq# :: forall a. (Eq a) => Int# -> MutVar# RealWorld (ArrayData a) -> MutVar# RealWorld (ArrayData a) -> Bool
unsafeEq# len ref1 ref2 =
  case runRW# rerootBoth of
    (# _, b #) -> lazy (isTrue# b)
  where
    rerootBoth =
      reroot# ref1 takeMVar# $ oneShot $ \lock1 buff1 ->
      let acquireLock2 = oneShot \lock2 s0 ->
            -- If lock1 and lock2 are aliased, then trying to acquire a lock will
            -- result in a deadlock.
            if isTrue# (sameMVar# lock1 lock2) then
              -- We can't just bail out and return true here, as we are working
              -- underneath a stack of @Set@ constructors. Instead, we clone
              -- the buffer that both stacks of @Set@ constructors point to.
              -- This means that all of the delayed calls to @Set@ from @ref2@
              -- will be performed on a fresh buffer, and won't interfere with
              -- the sets from @ref1@.
              cloneMutableArray# buff1 0# len s0
            else
              takeMVar# lock2 s0
      in reroot# ref2 acquireLock2 (loop 0# 1# lock1 buff1)

    -- Check if two buffers contain the same elements, and share the
    -- two arrays if their contents are intensionally identical.
    loop i shouldShare lock1 buff1 lock2 buff2 s0
      | isTrue# (i <# len) =
        let !(# s1, x #) = readArray# buff1 i s0
            !(# s2, y #) = readArray# buff1 i s1
        -- This call to @reallyUnsafePtrEquality#@ is safe, as we only
        -- use it to determine if we should introduce extra sharing.
        in if (isTrue# (reallyUnsafePtrEquality# x y)) then
          loop (i +# 1#) shouldShare lock1 buff1 lock2 buff2 s2
        else if x == y then
          loop (i +# 1#) 0# lock1 buff1 lock2 buff2 s2
        else
          if isTrue# (sameMVar# lock1 lock2) then
          -- If lock1 and lock2 are aliased, then we should unlock
          -- the lock with @buff2@, as we moved the root to
          -- @ref2@ last.
          (# putMVar# lock1 buff2 s2, 0# #)
        else
          (# putMVar# lock1 buff1 (putMVar# lock2 buff2 s2), 0# #)
      | otherwise =
        if isTrue# (sameMVar# lock1 lock2) then
          -- Same idea as before, @buff2@ wins when we've aliased.
          (# putMVar# lock1 buff2 s0, 1# #)
        else if isTrue# shouldShare then
          -- Contents of the arrays are intensionally equal:
          -- introduce sharing by unlocking both non-aliased locks
          -- with the same array.
          (# putMVar# lock1 buff1 (putMVar# lock2 buff1 s0), 1# #)
        else
          -- The arrays are extensionally equal, but intensional distinct;
          -- don't introduce sharing.
          (# putMVar# lock1 buff1 (putMVar# lock2 buff2 s0), 1# #)

--------------------------------------------------------------------------------
-- Creation

generate :: forall a. Int -> (Int -> a) -> a -> Array a
generate (I# len) f def =
  let !ref = unsafeGenerate# len f def
  in Array
  { arrayDefault = def
  , arraySize = len
  , arrayData = ref
  }
{-# INLINE generate #-}

unsafeGenerate# :: forall a. Int# -> (Int -> a) -> a -> MutVar# RealWorld (ArrayData a)
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

fromList :: forall a. Int -> [a] -> a -> Array a
fromList (I# len) xs def = Array
  { arrayDefault = def
  , arraySize = len
  , arrayData = unsafeFromList# len xs def
  }
{-# INLINE fromList #-}

unsafeFromList# :: forall a. Int# -> [a] -> a -> MutVar# RealWorld (ArrayData a)
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
  . MutVar# RealWorld (ArrayData a)
  -> (MVar# RealWorld (MutableArray# RealWorld a) -> State# RealWorld -> (# State# RealWorld, MutableArray# RealWorld a #))
  -- ^ Called to unlock lock on the underlying array.
  -> (MVar# RealWorld (MutableArray# RealWorld a) -> MutableArray# RealWorld a -> State# RealWorld -> (# State# RealWorld, r #))
  -> State# RealWorld
  -> (# State# RealWorld, r #)
reroot# ref unlock k s0 =
  case readMutVar# ref s0 of
    (# s1, Root lockedBuff #) ->
      -- Take the lock here; continuation is responsible for unlocking.
      let !(# s2, buff #) = unlock lockedBuff s1
      in k lockedBuff buff s2
    (# s1, Set i new oldRef #) ->
      let unwind = oneShot \lock buff s3 ->
            let -- We have the lock here, so it is safe to manipulate pointers.
              !(# s4, old #) = readArray# buff i s3
              !s5 = writeArray# buff i new s4
              !(# s6, oldData #) = readMutVar# oldRef s5
              !s7 = writeMutVar# ref oldData s6
              !s8 = writeMutVar# oldRef (Set i old ref) s7
             in k lock buff s8
      in reroot# oldRef unlock unwind s1

get :: forall a. Array a -> Int -> a
get xs (I# i)
  | isTrue# (i <# arraySize xs) = unsafeGet# (arrayData xs) i
  | otherwise = arrayDefault xs
{-# INLINE get #-}

unsafeGet# :: forall a. MutVar# RealWorld (ArrayData a) -> Int# -> a
unsafeGet# ref i =
  let k = oneShot \lock buff s0 ->
        let !(# s1, x #) = readArray# buff i s0
            !s2 = putMVar# lock buff s1
        in (# s2, x #)
  in case runRW# (reroot# ref takeMVar# k) of
    (# _, r #) -> lazy r
{-# NOINLINE unsafeGet# #-}

-- | Set index @i@ to @x@.
set :: forall a. Array a -> Int -> a -> Array a
set xs (I# i) x
  | isTrue# (0# <=# i) && isTrue# (i <# arraySize xs) =
    let !newRef = unsafeSet (arrayData xs) i x
    in xs { arrayData = newRef }
  | otherwise = xs
{-# INLINE set #-}

unsafeSet :: forall a. MutVar# RealWorld (ArrayData a) -> Int# -> a -> MutVar# RealWorld (ArrayData a)
unsafeSet ref i new =
  let k = oneShot \lock buff s0 ->
        let !(# s1, old #) = readArray# buff i s0
            !s2 = writeArray# buff i new s1
            !(# s3, newRef #) = newMutVar# (Root lock) s2
            !s4 = writeMutVar# ref (Set i old newRef) s3
            !s5 = putMVar# lock buff s4
        in (# s5, newRef #)
  in case runRW# (reroot# ref takeMVar# k) of
    (# _, newRef #) -> newRef
{-# NOINLINE unsafeSet #-}

setDefault :: forall a. a -> Array a -> Array a
setDefault x xs = xs { arrayDefault = x }
{-# INLINE setDefault #-}

getDefault :: forall a. Array a -> a
getDefault = arrayDefault
{-# INLINE getDefault #-}

--------------------------------------------------------------------------------
-- Folds

instance Foldable Array where
  foldr f b xs = unsafeFoldr# f b (arraySize xs) (arrayData xs)
  {-# INLINE foldr #-}

  foldl f b xs = unsafeFoldl# f b (arraySize xs) (arrayData xs)
  {-# INLINE foldl #-}

  length xs = I# (arraySize xs)
  {-# INLINE length #-}

  null xs = isTrue# (arraySize xs ==# 0#)
  {-# INLINE null #-}

-- | Perform a strict right fold over an array.
foldr' :: (a -> b -> b) -> b -> Array a -> b
foldr' f b xs = unsafeFoldr'# f b (arraySize xs) (arrayData xs)
{-# INLINE foldr' #-}

-- | Perform a strict left fold over an array.
foldl' :: (b -> a -> b) -> b -> Array a -> b
foldl' f b xs = unsafeFoldl'# f b (arraySize xs) (arrayData xs)
{-# INLINE foldl' #-}

-- | Perform a lazy right fold over an array of a specified
-- length.
--
-- Precondition: the specified length is less than or equal to the length
-- of the underlying array.
unsafeFoldr# :: forall a b. (a -> b -> b) -> b -> Int# -> MutVar# RealWorld (ArrayData a) -> b
unsafeFoldr# f b len xs = loop 0#
  where
    -- Repeatedly calling unsafeGet# lets us produce the result
    -- of the fold lazily. If we used a similar strategy to
    -- @unsafeFoldr'#@, then we would have to produce the result all
    -- at once while holding the lock. This would result in a space leak,
    -- as we would have to build up a large thunk in our accumulator.
    loop i
      | isTrue# (i <# len) = f (unsafeGet# xs i) (loop (i +# 1#))
      | otherwise = b
{-# INLINE unsafeFoldr# #-}

-- | Perform a lazy left fold over an array of a specified length.
--
-- Precondition: the specified length is less than or equal to the length
-- of the underlying array.
unsafeFoldl# :: forall a b. (b -> a -> b) -> b -> Int# -> MutVar# RealWorld (ArrayData a) -> b
unsafeFoldl# f b len xs = loop (len -# 1#)
  where
    -- Similar idea to @unsafeFoldr#@: only acquire the lock and read
    -- a single element at a time.
    loop i
      | isTrue# (i >=# 0#) = f (loop (i -# 1#)) (unsafeGet# xs i)
      | otherwise = b
{-# INLINE unsafeFoldl# #-}

-- | Perform a strict right fold over the contents of an array of a specified
-- length.
--
-- Precondition: the specified length is less than or equal to the length
-- of the underlying array.
unsafeFoldr'# :: forall a b. (a -> b -> b) -> b -> Int# -> MutVar# RealWorld (ArrayData a) -> b
unsafeFoldr'# f b len ref =
  case runRW# (reroot# ref takeMVar# (loop (len -# 1#) b)) of
    (# _, xs #) -> lazy xs

  where
    loop i !acc lock buff s0
      | isTrue# (i >=# 0#) =
        let !(# s1, a #) = readArray# buff i s0
        in loop (i -# 1#) (f a b) lock buff s1
      | otherwise =
        let !s1 = putMVar# lock buff s0
        in (# s1, acc #)
{-# NOINLINE unsafeFoldr'# #-}

-- | Perform a strict left fold over the contents of an array of a specified
-- length.
--
-- Precondition: the specified length is less than or equal to the length
-- of the underlying array.
unsafeFoldl'# :: forall a b. (b -> a -> b) -> b -> Int# -> MutVar# RealWorld (ArrayData a) -> b
unsafeFoldl'# f b len ref =
  case runRW# (reroot# ref takeMVar# (loop 0# b)) of
    (# _, xs #) -> lazy xs

  where
    loop i !acc lock buff s0
      | isTrue# (i <# len) =
        let !(# s1, a #) = readArray# buff i s0
        in loop (i +# 1#) (f b a) lock buff s1
      | otherwise =
        let !s1 = putMVar# lock buff s0
        in (# s1, acc #)
{-# NOINLINE unsafeFoldl'# #-}

--------------------------------------------------------------------------------
-- Debugging

-- | Check that all invariants of an 'Array' are upheld.
valid :: forall a. Array a -> Bool
valid xs =
  case runRW# (unsafeValid# (arraySize xs) (arrayData xs)) of
    (# _, ans #) -> lazy (isTrue# ans)
{-# NOINLINE valid #-}

unsafeValid# :: forall a. Int# -> MutVar# RealWorld (ArrayData a) -> State# RealWorld -> (# State# RealWorld, Int# #)
unsafeValid# len ref s0 =
  let !(# s1, d #) = readMutVar# ref s0
  in case d of
    Root lock ->
      let !(# s2, buff #) = readMVar# lock s1
      in (# s2, sizeofMutableArray# buff ==# len #)
    Set i _ oldRef
      | isTrue# (0# <=# i) && isTrue# (i <# len) -> unsafeValid# len oldRef s1
      | otherwise -> (# s1, 0# #)

-- | Show the internal representation of an array.
--
-- This function is impure, and lets us observe the internal
-- mutable structure of an array. It is intended purely for debugging
-- purposes.
unsafeDebugShow :: forall a. (Show a) => Array a -> String
unsafeDebugShow xs =
  case runRW# (unsafeDebugShow# (arraySize xs) (arrayData xs) 0 id) of
    (# _, str #) -> lazy str
{-# NOINLINE unsafeDebugShow #-}

unsafeDebugShow# :: forall a. (Show a) => Int# -> MutVar# RealWorld (ArrayData a) -> Int -> (String -> String) -> State# RealWorld -> (# State# RealWorld, String #)
unsafeDebugShow# len ref prec k s0 =
  let !(# s1, d #) = readMutVar# ref s0
  in case d of
    Root lock ->
      let !(# s2, buff #) = takeMVar# lock s1
          !(# s3, contents #) = debugShowContents (len -# 1#) buff (showString "]") s2
      in (# putMVar# lock buff s2, k $ showParen (prec > 10) (showString "Root " . contents) $ "" #)
    Set i x oldRef ->
      let k' = showParen (prec > 10) $ showString "Set " . shows (I# i) . showString " " . shows x . showString " "
      in unsafeDebugShow# len oldRef 10 k' s1
  where
    debugShowContents i buff !acc s0
      | isTrue# (i >=# 0#) =
        let !(# s1, x #) = readArray# buff i s0
            commaAcc = if isTrue# (i ==# len -# 1#) then acc else (showString ", " . acc)
        in debugShowContents (i -# 1#) buff (shows x . commaAcc) s1
      | otherwise = (# s0, showString "[" . acc #)
