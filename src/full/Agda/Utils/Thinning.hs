-- | Thinnings.
module Agda.Utils.Thinning
  ( Thinning(..)
  , thin
  -- * Constructing thinnings
  , idT
  , takeT
  , liftT
  , composeT
  ) where

import Prelude hiding (null)

import Control.DeepSeq

import Agda.Utils.Null
import Agda.Utils.Size
import Agda.Utils.VarSet (VarSet)
import Agda.Utils.VarSet qualified as VarSet

-- | Thinnings of de Bruijn levels.
--
-- A 'Thinning' encodes an order-preserving embedding @Fin m -> Fin n@.
data Thinning = Thin
  { thinRange :: {-# UNPACK #-} !Int
  -- ^ The codomain of the thinning.
  , thinPicks :: !VarSet
  -- ^ A bitmap that describes how to scatter on to the codomain.
  }
  deriving (Show, Eq, Ord)

instance NFData Thinning where
  rnf (Thin _ _) = ()

instance Null Thinning where
  empty = Thin 0 empty
  null (Thin _ picks) = null picks

instance Sized Thinning where
  size (Thin _ th) = size th
  natSize (Thin _ th) = natSize th

-- | Apply a 'Thinning' to a list of things.
thin :: Thinning -> [a] -> [a]
thin (Thin _ picks) = VarSet.thin picks

--------------------------------------------------------------------------------
-- Constructing thinnings

-- | Identity thinning.
idT :: Int -> Thinning
idT n = Thin n (VarSet.full n)

-- | Restrict a thinning to work on @n@ elements.
takeT :: Int -> Thinning -> Thinning
takeT n (Thin m picks) = Thin n (VarSet.filterLT n picks)

-- | @liftT k@ takes a @Thinning {m} n@ to a @Thinning {m+k} (n+k)@.
liftT :: Int -> Thinning -> Thinning
liftT n (Thin m picks) = Thin (m + n) (VarSet.union picks (VarSet.weaken m (VarSet.full n)))

-- | @thin (composeT th1 th2) == thin th1 . thin th2@.
composeT :: Thinning -> Thinning -> Thinning
composeT (Thin _ th1) (Thin n th2) = Thin n (VarSet.compose th1 th2)
