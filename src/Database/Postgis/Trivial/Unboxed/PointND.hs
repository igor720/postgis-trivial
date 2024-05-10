{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Postgis.Trivial.Unboxed.PointND
( P2DU (..)
, P3DZU (..)
, P3DMU (..)
, P4DU (..)
) where

import GHC.Base
import GHC.Show ( Show )
import Control.Applicative ( (<$>) )
import Data.Tuple ( uncurry )
import Control.Exception ( throw )
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU

import Database.Postgis.Trivial.Types
import Database.Postgis.Trivial.Cast


uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

-- P2D =========================================================================

-- | Inner Unbox 2D point
data P2DU = Point2DU
        {-# UNPACK #-} !Double
        {-# UNPACK #-} !Double
        deriving (Show, Eq)

fromP2D :: P2DU -> (Double, Double)
fromP2D (Point2DU x y) = (x, y)

newtype instance VU.MVector s P2DU = MV_P2D (VU.MVector s (Double, Double))
newtype instance VU.Vector    P2DU = V_P2D  (VU.Vector    (Double, Double))

instance VGM.MVector VU.MVector P2DU where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicInitialize #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    {-# INLINE basicClear #-}
    {-# INLINE basicSet #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE basicUnsafeGrow #-}
    basicLength (MV_P2D v) = VGM.basicLength v
    basicUnsafeNew n = MV_P2D <$> VGM.basicUnsafeNew n
    basicUnsafeSlice i n (MV_P2D v) = MV_P2D $ VGM.basicUnsafeSlice i n v
    basicOverlaps (MV_P2D v1) (MV_P2D v2) = VGM.basicOverlaps v1 v2
    basicInitialize (MV_P2D v) = VGM.basicInitialize v
    basicUnsafeReplicate n p = MV_P2D <$> VGM.basicUnsafeReplicate n (fromP2D p)
    basicUnsafeRead (MV_P2D v) i = uncurry Point2DU <$> VGM.basicUnsafeRead v i
    basicUnsafeWrite (MV_P2D v) i p = VGM.basicUnsafeWrite v i (fromP2D p)
    basicClear (MV_P2D v) = VGM.basicClear v
    basicSet (MV_P2D v) p = VGM.basicSet v (fromP2D p)
    basicUnsafeCopy (MV_P2D v1) (MV_P2D v2) = VGM.basicUnsafeCopy v1 v2
    basicUnsafeMove (MV_P2D v1) (MV_P2D v2) = VGM.basicUnsafeMove v1 v2
    basicUnsafeGrow (MV_P2D v) n = MV_P2D <$> VGM.basicUnsafeGrow v n

instance VG.Vector VU.Vector P2DU where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicLength (V_P2D v) = VG.basicLength v
    basicUnsafeFreeze (MV_P2D v) = V_P2D <$> VG.basicUnsafeFreeze v
    basicUnsafeThaw (V_P2D v) = MV_P2D <$> VG.basicUnsafeThaw v
    basicUnsafeSlice i n (V_P2D v) = V_P2D $ VG.basicUnsafeSlice i n v
    basicUnsafeIndexM (V_P2D v) i = uncurry Point2DU <$> VG.basicUnsafeIndexM v i
    basicUnsafeCopy (MV_P2D mv) (V_P2D v) = VG.basicUnsafeCopy mv v

instance VU.Unbox P2DU

instance PointND P2DU where
    dimProps = (False, False)
    components (Point2DU x y) = (x, y, Nothing, Nothing)
    fromComponents (x, y, Nothing, Nothing) = Point2DU x y
    fromComponents _ = throw $
        GeometryError "invalid transition from user data type to P2D"

-- P3DZ ========================================================================

-- | Inner Unbox 3D point with Z component
data P3DZU = Point3DZU
        {-# UNPACK #-} !Double
        {-# UNPACK #-} !Double
        {-# UNPACK #-} !Double
        deriving (Show, Eq)

fromP3DZ :: P3DZU -> (Double, Double, Double)
fromP3DZ (Point3DZU x y z) = (x, y, z)

newtype instance VU.MVector s P3DZU =
    MV_P3DZ (VU.MVector s (Double, Double, Double))
newtype instance VU.Vector    P3DZU =
    V_P3DZ  (VU.Vector    (Double, Double, Double))

instance VGM.MVector VU.MVector P3DZU where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicInitialize #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    {-# INLINE basicClear #-}
    {-# INLINE basicSet #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE basicUnsafeGrow #-}
    basicLength (MV_P3DZ v) = VGM.basicLength v
    basicUnsafeNew n = MV_P3DZ <$> VGM.basicUnsafeNew n
    basicUnsafeSlice i n (MV_P3DZ v) = MV_P3DZ $ VGM.basicUnsafeSlice i n v
    basicOverlaps (MV_P3DZ v1) (MV_P3DZ v2) = VGM.basicOverlaps v1 v2
    basicInitialize (MV_P3DZ v) = VGM.basicInitialize v
    basicUnsafeReplicate n p = MV_P3DZ <$> VGM.basicUnsafeReplicate n (fromP3DZ p)
    basicUnsafeRead (MV_P3DZ v) i = uncurry3 Point3DZU <$> VGM.basicUnsafeRead v i
    basicUnsafeWrite (MV_P3DZ v) i p = VGM.basicUnsafeWrite v i (fromP3DZ p)
    basicClear (MV_P3DZ v) = VGM.basicClear v
    basicSet (MV_P3DZ v) p = VGM.basicSet v (fromP3DZ p)
    basicUnsafeCopy (MV_P3DZ v1) (MV_P3DZ v2) = VGM.basicUnsafeCopy v1 v2
    basicUnsafeMove (MV_P3DZ v1) (MV_P3DZ v2) = VGM.basicUnsafeMove v1 v2
    basicUnsafeGrow (MV_P3DZ v) n = MV_P3DZ <$> VGM.basicUnsafeGrow v n

instance VG.Vector VU.Vector P3DZU where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicLength (V_P3DZ v) = VG.basicLength v
    basicUnsafeFreeze (MV_P3DZ v) = V_P3DZ <$> VG.basicUnsafeFreeze v
    basicUnsafeThaw (V_P3DZ v) = MV_P3DZ <$> VG.basicUnsafeThaw v
    basicUnsafeSlice i n (V_P3DZ v) = V_P3DZ $ VG.basicUnsafeSlice i n v
    basicUnsafeIndexM (V_P3DZ v) i = uncurry3 Point3DZU <$> VG.basicUnsafeIndexM v i
    basicUnsafeCopy (MV_P3DZ mv) (V_P3DZ v) = VG.basicUnsafeCopy mv v

instance VU.Unbox P3DZU

instance PointND P3DZU where
    dimProps = (False, True)
    components (Point3DZU x y z) = (x, y, Just z, Nothing)
    fromComponents (x, y, Just z, Nothing) = Point3DZU x y z
    fromComponents _ = throw $
        GeometryError "invalid transition from user data type to P3DZ"

-- P3DM ========================================================================

-- | Inner Unbox 3D point with M component
data P3DMU = Point3DMU
        {-# UNPACK #-} !Double
        {-# UNPACK #-} !Double
        {-# UNPACK #-} !Double
        deriving (Show, Eq)

fromP3DM :: P3DMU -> (Double, Double, Double)
fromP3DM (Point3DMU x y z) = (x, y, z)

newtype instance VU.MVector s P3DMU =
    MV_P3DM (VU.MVector s (Double, Double, Double))
newtype instance VU.Vector    P3DMU =
    V_P3DM  (VU.Vector    (Double, Double, Double))

instance VGM.MVector VU.MVector P3DMU where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicInitialize #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    {-# INLINE basicClear #-}
    {-# INLINE basicSet #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE basicUnsafeGrow #-}
    basicLength (MV_P3DM v) = VGM.basicLength v
    basicUnsafeNew n = MV_P3DM <$> VGM.basicUnsafeNew n
    basicUnsafeSlice i n (MV_P3DM v) = MV_P3DM $ VGM.basicUnsafeSlice i n v
    basicOverlaps (MV_P3DM v1) (MV_P3DM v2) = VGM.basicOverlaps v1 v2
    basicInitialize (MV_P3DM v) = VGM.basicInitialize v
    basicUnsafeReplicate n p = MV_P3DM <$> VGM.basicUnsafeReplicate n (fromP3DM p)
    basicUnsafeRead (MV_P3DM v) i = uncurry3 Point3DMU <$> VGM.basicUnsafeRead v i
    basicUnsafeWrite (MV_P3DM v) i p = VGM.basicUnsafeWrite v i (fromP3DM p)
    basicClear (MV_P3DM v) = VGM.basicClear v
    basicSet (MV_P3DM v) p = VGM.basicSet v (fromP3DM p)
    basicUnsafeCopy (MV_P3DM v1) (MV_P3DM v2) = VGM.basicUnsafeCopy v1 v2
    basicUnsafeMove (MV_P3DM v1) (MV_P3DM v2) = VGM.basicUnsafeMove v1 v2
    basicUnsafeGrow (MV_P3DM v) n = MV_P3DM <$> VGM.basicUnsafeGrow v n

instance VG.Vector VU.Vector P3DMU where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicLength (V_P3DM v) = VG.basicLength v
    basicUnsafeFreeze (MV_P3DM v) = V_P3DM <$> VG.basicUnsafeFreeze v
    basicUnsafeThaw (V_P3DM v) = MV_P3DM <$> VG.basicUnsafeThaw v
    basicUnsafeSlice i n (V_P3DM v) = V_P3DM $ VG.basicUnsafeSlice i n v
    basicUnsafeIndexM (V_P3DM v) i = uncurry3 Point3DMU <$> VG.basicUnsafeIndexM v i
    basicUnsafeCopy (MV_P3DM mv) (V_P3DM v) = VG.basicUnsafeCopy mv v

instance VU.Unbox P3DMU

instance PointND P3DMU where
    dimProps = (True, False)
    components (Point3DMU x y m) = (x, y, Just m, Nothing)
    fromComponents (x, y, Just m, Nothing) = Point3DMU x y m
    fromComponents _ = throw $
        GeometryError "invalid transition from user data type to P3DM"

-- P4D =========================================================================

-- | Inner Unbox point with Z and M component
data P4DU = Point4DU
        {-# UNPACK #-} !Double
        {-# UNPACK #-} !Double
        {-# UNPACK #-} !Double
        {-# UNPACK #-} !Double
        deriving (Show, Eq)

fromP4D :: P4DU -> (Double, Double, Double, Double)
fromP4D (Point4DU x y z m) = (x, y, z, m)

newtype instance VU.MVector s P4DU =
    MV_P4D (VU.MVector s (Double, Double, Double, Double))
newtype instance VU.Vector    P4DU =
    V_P4D  (VU.Vector    (Double, Double, Double, Double))

instance VGM.MVector VU.MVector P4DU where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicInitialize #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    {-# INLINE basicClear #-}
    {-# INLINE basicSet #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE basicUnsafeGrow #-}
    basicLength (MV_P4D v) = VGM.basicLength v
    basicUnsafeNew n = MV_P4D <$> VGM.basicUnsafeNew n
    basicUnsafeSlice i n (MV_P4D v) = MV_P4D $ VGM.basicUnsafeSlice i n v
    basicOverlaps (MV_P4D v1) (MV_P4D v2) = VGM.basicOverlaps v1 v2
    basicInitialize (MV_P4D v) = VGM.basicInitialize v
    basicUnsafeReplicate n p = MV_P4D <$> VGM.basicUnsafeReplicate n (fromP4D p)
    basicUnsafeRead (MV_P4D v) i = uncurry4 Point4DU <$> VGM.basicUnsafeRead v i
    basicUnsafeWrite (MV_P4D v) i p = VGM.basicUnsafeWrite v i (fromP4D p)
    basicClear (MV_P4D v) = VGM.basicClear v
    basicSet (MV_P4D v) p = VGM.basicSet v (fromP4D p)
    basicUnsafeCopy (MV_P4D v1) (MV_P4D v2) = VGM.basicUnsafeCopy v1 v2
    basicUnsafeMove (MV_P4D v1) (MV_P4D v2) = VGM.basicUnsafeMove v1 v2
    basicUnsafeGrow (MV_P4D v) n = MV_P4D <$> VGM.basicUnsafeGrow v n

instance VG.Vector VU.Vector P4DU where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicLength (V_P4D v) = VG.basicLength v
    basicUnsafeFreeze (MV_P4D v) = V_P4D <$> VG.basicUnsafeFreeze v
    basicUnsafeThaw (V_P4D v) = MV_P4D <$> VG.basicUnsafeThaw v
    basicUnsafeSlice i n (V_P4D v) = V_P4D $ VG.basicUnsafeSlice i n v
    basicUnsafeIndexM (V_P4D v) i = uncurry4 Point4DU <$> VG.basicUnsafeIndexM v i
    basicUnsafeCopy (MV_P4D mv) (V_P4D v) = VG.basicUnsafeCopy mv v

instance VU.Unbox P4DU

instance PointND P4DU where
    dimProps = (True, True)
    components (Point4DU x y z m) = (x, y, Just z, Just m)
    fromComponents (x, y, Just z, Just m) = Point4DU x y z m
    fromComponents _ = throw $
        GeometryError "invalid transition from user data type to P4D"

-- Cast ========================================================================

type instance Cast P2DU = P2DU
type instance Cast P3DZU = P3DZU
type instance Cast P3DMU = P3DMU
type instance Cast P4DU = P4DU

instance Castable P2DU where
    toPointND = coerce
    fromPointND = coerce
instance Castable P3DZU where
    toPointND = coerce
    fromPointND = coerce
instance Castable P3DMU where
    toPointND = coerce
    fromPointND = coerce
instance Castable P4DU where
    toPointND = coerce
    fromPointND = coerce





