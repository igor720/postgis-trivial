{-# LANGUAGE TypeFamilies #-}

module Database.Postgis.Trivial.Storable.PointND
( P2DS (..)
, P3DZS (..)
, P3DMS (..)
, P4DS (..)
) where

import GHC.Base
import GHC.Show ( Show )
import Foreign.Storable ( Storable (..) )
import Foreign.Storable.Record as Store
import Control.Exception ( throw )

import Database.Postgis.Trivial.Types
import Database.Postgis.Trivial.Cast


-- | Four arguments LiftA
liftA4 :: Applicative f
        => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = liftA3 f a b c <*> d

-- P2DS =========================================================================

-- | Inner Storable 2D point
data P2DS = Point2DS
        { xP2DS :: {-# UNPACK #-} !Double
        , yP2DS :: {-# UNPACK #-} !Double
        } deriving (Show, Eq)

storeP2DS :: Store.Dictionary P2DS
storeP2DS = Store.run $
    liftA2 Point2DS
        (Store.element xP2DS)
        (Store.element yP2DS)

instance Storable P2DS where
   sizeOf = Store.sizeOf storeP2DS
   alignment = Store.alignment storeP2DS
   peek = Store.peek storeP2DS
   poke = Store.poke storeP2DS

instance PointND P2DS where
    dimProps = (False, False)
    components (Point2DS x y) = (x, y, Nothing, Nothing)
    fromComponents (x, y, Nothing, Nothing) = Point2DS x y
    fromComponents _ = throw $
        GeometryError "invalid transition from user data type to P2DS"

-- P3DZ ========================================================================

-- | Inner Storable 3D point with Z component
data P3DZS = Point3DZS
        { xP3DZ :: {-# UNPACK #-} !Double
        , yP3DZ :: {-# UNPACK #-} !Double
        , zP3DZ :: {-# UNPACK #-} !Double
        } deriving (Show, Eq)

storeP3DZ :: Store.Dictionary P3DZS
storeP3DZ = Store.run $
    liftA3 Point3DZS
        (Store.element xP3DZ)
        (Store.element yP3DZ)
        (Store.element zP3DZ)

instance Storable P3DZS where
   sizeOf = Store.sizeOf storeP3DZ
   alignment = Store.alignment storeP3DZ
   peek = Store.peek storeP3DZ
   poke = Store.poke storeP3DZ

instance PointND P3DZS where
    dimProps = (False, True)
    components (Point3DZS x y z) = (x, y, Just z, Nothing)
    fromComponents (x, y, Just z, Nothing) = Point3DZS x y z
    fromComponents _ = throw $
        GeometryError "invalid transition from user data type to P3DZ"

-- P3DM ========================================================================

-- | Inner Storable 3D point with M component
data P3DMS = Point3DMS
        { xP3DM :: {-# UNPACK #-} !Double
        , yP3DM :: {-# UNPACK #-} !Double
        , mP3DM :: {-# UNPACK #-} !Double
        } deriving (Show, Eq)

storeP3DM :: Store.Dictionary P3DMS
storeP3DM = Store.run $
    liftA3 Point3DMS
        (Store.element xP3DM)
        (Store.element yP3DM)
        (Store.element mP3DM)

instance Storable P3DMS where
   sizeOf = Store.sizeOf storeP3DM
   alignment = Store.alignment storeP3DM
   peek = Store.peek storeP3DM
   poke = Store.poke storeP3DM

instance PointND P3DMS where
    dimProps = (True, False)
    components (Point3DMS x y m) = (x, y, Just m, Nothing)
    fromComponents (x, y, Just m, Nothing) = Point3DMS x y m
    fromComponents _ = throw $
        GeometryError "invalid transition from user data type to P3DM"

-- P4D =========================================================================

-- | Inner Storable point with Z and M component
data P4DS = Point4DS
        { xP4D :: {-# UNPACK #-} !Double
        , yP4D :: {-# UNPACK #-} !Double
        , zP4D :: {-# UNPACK #-} !Double
        , mP4D :: {-# UNPACK #-} !Double
        } deriving (Show, Eq)

storeP4D :: Store.Dictionary P4DS
storeP4D = Store.run $
    liftA4 Point4DS
        (Store.element xP4D)
        (Store.element yP4D)
        (Store.element zP4D)
        (Store.element mP4D)

instance Storable P4DS where
   sizeOf = Store.sizeOf storeP4D
   alignment = Store.alignment storeP4D
   peek = Store.peek storeP4D
   poke = Store.poke storeP4D

instance PointND P4DS where
    dimProps = (True, True)
    components (Point4DS x y z m) = (x, y, Just z, Just m)
    fromComponents (x, y, Just z, Just m) = Point4DS x y z m
    fromComponents _ = throw $
        GeometryError "invalid transition from user data type to P4D"

-- Cast ========================================================================

type instance Cast P2DS = P2DS
type instance Cast P3DZS = P3DZS
type instance Cast P3DMS = P3DMS
type instance Cast P4DS = P4DS

instance Castable P2DS where
    toPointND = coerce
    fromPointND = coerce
instance Castable P3DZS where
    toPointND = coerce
    fromPointND = coerce
instance Castable P3DMS where
    toPointND = coerce
    fromPointND = coerce
instance Castable P4DS where
    toPointND = coerce
    fromPointND = coerce





