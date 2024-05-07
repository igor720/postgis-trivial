{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Postgis.Trivial.Unboxed.PointND where

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

data P2D = Point2D
        {-# UNPACK #-} !Double
        {-# UNPACK #-} !Double
        deriving (Show, Eq)

fromP2D :: P2D -> (Double, Double)
fromP2D (Point2D x y) = (x, y)

newtype instance VU.MVector s P2D = MV_P2D (VU.MVector s (Double, Double))
newtype instance VU.Vector    P2D = V_P2D  (VU.Vector    (Double, Double))

instance VGM.MVector VU.MVector P2D where
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
    basicUnsafeRead (MV_P2D v) i = uncurry Point2D <$> VGM.basicUnsafeRead v i
    basicUnsafeWrite (MV_P2D v) i p = VGM.basicUnsafeWrite v i (fromP2D p)
    basicClear (MV_P2D v) = VGM.basicClear v
    basicSet (MV_P2D v) p = VGM.basicSet v (fromP2D p)
    basicUnsafeCopy (MV_P2D v1) (MV_P2D v2) = VGM.basicUnsafeCopy v1 v2
    basicUnsafeMove (MV_P2D v1) (MV_P2D v2) = VGM.basicUnsafeMove v1 v2
    basicUnsafeGrow (MV_P2D v) n = MV_P2D <$> VGM.basicUnsafeGrow v n

instance VG.Vector VU.Vector P2D where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicLength (V_P2D v) = VG.basicLength v
    basicUnsafeFreeze (MV_P2D v) = V_P2D <$> VG.basicUnsafeFreeze v
    basicUnsafeThaw (V_P2D v) = MV_P2D <$> VG.basicUnsafeThaw v
    basicUnsafeSlice i n (V_P2D v) = V_P2D $ VG.basicUnsafeSlice i n v
    basicUnsafeIndexM (V_P2D v) i = uncurry Point2D <$> VG.basicUnsafeIndexM v i
    basicUnsafeCopy (MV_P2D mv) (V_P2D v) = VG.basicUnsafeCopy mv v

instance VU.Unbox P2D

instance PointND P2D where
    dimProps = (False, False)
    components (Point2D x y) = (x, y, Nothing, Nothing)
    fromComponents (x, y, Nothing, Nothing) = Point2D x y
    fromComponents _ = throw $
        GeometryError "invalid transition from user data type to P2D"

-- P3DZ ========================================================================

data P3DZ = Point3DZ
        {-# UNPACK #-} !Double
        {-# UNPACK #-} !Double
        {-# UNPACK #-} !Double
        deriving (Show, Eq)

fromP3DZ :: P3DZ -> (Double, Double, Double)
fromP3DZ (Point3DZ x y z) = (x, y, z)

newtype instance VU.MVector s P3DZ = MV_P3DZ (VU.MVector s (Double, Double, Double))
newtype instance VU.Vector    P3DZ = V_P3DZ  (VU.Vector    (Double, Double, Double))

instance VGM.MVector VU.MVector P3DZ where
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
    basicUnsafeRead (MV_P3DZ v) i = uncurry3 Point3DZ <$> VGM.basicUnsafeRead v i
    basicUnsafeWrite (MV_P3DZ v) i p = VGM.basicUnsafeWrite v i (fromP3DZ p)
    basicClear (MV_P3DZ v) = VGM.basicClear v
    basicSet (MV_P3DZ v) p = VGM.basicSet v (fromP3DZ p)
    basicUnsafeCopy (MV_P3DZ v1) (MV_P3DZ v2) = VGM.basicUnsafeCopy v1 v2
    basicUnsafeMove (MV_P3DZ v1) (MV_P3DZ v2) = VGM.basicUnsafeMove v1 v2
    basicUnsafeGrow (MV_P3DZ v) n = MV_P3DZ <$> VGM.basicUnsafeGrow v n

instance VG.Vector VU.Vector P3DZ where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicLength (V_P3DZ v) = VG.basicLength v
    basicUnsafeFreeze (MV_P3DZ v) = V_P3DZ <$> VG.basicUnsafeFreeze v
    basicUnsafeThaw (V_P3DZ v) = MV_P3DZ <$> VG.basicUnsafeThaw v
    basicUnsafeSlice i n (V_P3DZ v) = V_P3DZ $ VG.basicUnsafeSlice i n v
    basicUnsafeIndexM (V_P3DZ v) i = uncurry3 Point3DZ <$> VG.basicUnsafeIndexM v i
    basicUnsafeCopy (MV_P3DZ mv) (V_P3DZ v) = VG.basicUnsafeCopy mv v

instance VU.Unbox P3DZ

instance PointND P3DZ where
    dimProps = (False, True)
    components (Point3DZ x y z) = (x, y, Just z, Nothing)
    fromComponents (x, y, Just z, Nothing) = Point3DZ x y z
    fromComponents _ = throw $
        GeometryError "invalid transition from user data type to P3DZ"

-- P3DM ========================================================================

data P3DM = Point3DM
        {-# UNPACK #-} !Double
        {-# UNPACK #-} !Double
        {-# UNPACK #-} !Double
        deriving (Show, Eq)

fromP3DM :: P3DM -> (Double, Double, Double)
fromP3DM (Point3DM x y z) = (x, y, z)

newtype instance VU.MVector s P3DM = MV_P3DM (VU.MVector s (Double, Double, Double))
newtype instance VU.Vector    P3DM = V_P3DM  (VU.Vector    (Double, Double, Double))

instance VGM.MVector VU.MVector P3DM where
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
    basicUnsafeRead (MV_P3DM v) i = uncurry3 Point3DM <$> VGM.basicUnsafeRead v i
    basicUnsafeWrite (MV_P3DM v) i p = VGM.basicUnsafeWrite v i (fromP3DM p)
    basicClear (MV_P3DM v) = VGM.basicClear v
    basicSet (MV_P3DM v) p = VGM.basicSet v (fromP3DM p)
    basicUnsafeCopy (MV_P3DM v1) (MV_P3DM v2) = VGM.basicUnsafeCopy v1 v2
    basicUnsafeMove (MV_P3DM v1) (MV_P3DM v2) = VGM.basicUnsafeMove v1 v2
    basicUnsafeGrow (MV_P3DM v) n = MV_P3DM <$> VGM.basicUnsafeGrow v n

instance VG.Vector VU.Vector P3DM where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicLength (V_P3DM v) = VG.basicLength v
    basicUnsafeFreeze (MV_P3DM v) = V_P3DM <$> VG.basicUnsafeFreeze v
    basicUnsafeThaw (V_P3DM v) = MV_P3DM <$> VG.basicUnsafeThaw v
    basicUnsafeSlice i n (V_P3DM v) = V_P3DM $ VG.basicUnsafeSlice i n v
    basicUnsafeIndexM (V_P3DM v) i = uncurry3 Point3DM <$> VG.basicUnsafeIndexM v i
    basicUnsafeCopy (MV_P3DM mv) (V_P3DM v) = VG.basicUnsafeCopy mv v

instance VU.Unbox P3DM

instance PointND P3DM where
    dimProps = (True, False)
    components (Point3DM x y m) = (x, y, Just m, Nothing)
    fromComponents (x, y, Just m, Nothing) = Point3DM x y m
    fromComponents _ = throw $
        GeometryError "invalid transition from user data type to P3DM"

-- P4D =========================================================================

data P4D = Point4D
        {-# UNPACK #-} !Double
        {-# UNPACK #-} !Double
        {-# UNPACK #-} !Double
        {-# UNPACK #-} !Double
        deriving (Show, Eq)

fromP4D :: P4D -> (Double, Double, Double, Double)
fromP4D (Point4D x y z m) = (x, y, z, m)

newtype instance VU.MVector s P4D = MV_P4D (VU.MVector s (Double, Double, Double, Double))
newtype instance VU.Vector    P4D = V_P4D  (VU.Vector    (Double, Double, Double, Double))

instance VGM.MVector VU.MVector P4D where
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
    basicUnsafeRead (MV_P4D v) i = uncurry4 Point4D <$> VGM.basicUnsafeRead v i
    basicUnsafeWrite (MV_P4D v) i p = VGM.basicUnsafeWrite v i (fromP4D p)
    basicClear (MV_P4D v) = VGM.basicClear v
    basicSet (MV_P4D v) p = VGM.basicSet v (fromP4D p)
    basicUnsafeCopy (MV_P4D v1) (MV_P4D v2) = VGM.basicUnsafeCopy v1 v2
    basicUnsafeMove (MV_P4D v1) (MV_P4D v2) = VGM.basicUnsafeMove v1 v2
    basicUnsafeGrow (MV_P4D v) n = MV_P4D <$> VGM.basicUnsafeGrow v n

instance VG.Vector VU.Vector P4D where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicLength (V_P4D v) = VG.basicLength v
    basicUnsafeFreeze (MV_P4D v) = V_P4D <$> VG.basicUnsafeFreeze v
    basicUnsafeThaw (V_P4D v) = MV_P4D <$> VG.basicUnsafeThaw v
    basicUnsafeSlice i n (V_P4D v) = V_P4D $ VG.basicUnsafeSlice i n v
    basicUnsafeIndexM (V_P4D v) i = uncurry4 Point4D <$> VG.basicUnsafeIndexM v i
    basicUnsafeCopy (MV_P4D mv) (V_P4D v) = VG.basicUnsafeCopy mv v

instance VU.Unbox P4D

instance PointND P4D where
    dimProps = (True, True)
    components (Point4D x y z m) = (x, y, Just z, Just m)
    fromComponents (x, y, Just z, Just m) = Point4D x y z m
    fromComponents _ = throw $
        GeometryError "invalid transition from user data type to P4D"

-- Cast ========================================================================

type instance Cast P2D = P2D
type instance Cast P3DZ = P3DZ
type instance Cast P3DM = P3DM
type instance Cast P4D = P4D

instance Castable P2D where
    toPointND = coerce
    fromPointND = coerce
instance Castable P3DZ where
    toPointND = coerce
    fromPointND = coerce
instance Castable P3DM where
    toPointND = coerce
    fromPointND = coerce
instance Castable P4D where
    toPointND = coerce
    fromPointND = coerce





