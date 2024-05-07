{-# LANGUAGE TypeFamilies #-}

module Database.Postgis.Trivial.Storable.PointND where

import GHC.Base
import GHC.Show ( Show )
import Foreign.Storable ( Storable (..) )
import Foreign.Storable.Record as Store
import Control.Exception ( throw )

import Database.Postgis.Trivial.Types
import Database.Postgis.Trivial.Cast


liftA4 :: Applicative f
        => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = liftA3 f a b c <*> d

-- P2D =========================================================================

data P2D = Point2D
        { xP2D :: {-# UNPACK #-} !Double
        , yP2D :: {-# UNPACK #-} !Double
        } deriving (Show, Eq)

storeP2D :: Store.Dictionary P2D
storeP2D = Store.run $
    liftA2 Point2D
        (Store.element xP2D)
        (Store.element yP2D)

instance Storable P2D where
   sizeOf = Store.sizeOf storeP2D
   alignment = Store.alignment storeP2D
   peek = Store.peek storeP2D
   poke = Store.poke storeP2D

instance PointND P2D where
    dimProps = (False, False)
    components (Point2D x y) = (x, y, Nothing, Nothing)
    fromComponents (x, y, Nothing, Nothing) = Point2D x y
    fromComponents _ = throw $
        GeometryError "invalid transition from user data type to P2D"

-- P3DZ ========================================================================

data P3DZ = Point3DZ
        { xP3DZ :: {-# UNPACK #-} !Double
        , yP3DZ :: {-# UNPACK #-} !Double
        , zP3DZ :: {-# UNPACK #-} !Double
        } deriving (Show, Eq)

storeP3DZ :: Store.Dictionary P3DZ
storeP3DZ = Store.run $
    liftA3 Point3DZ
        (Store.element xP3DZ)
        (Store.element yP3DZ)
        (Store.element zP3DZ)

instance Storable P3DZ where
   sizeOf = Store.sizeOf storeP3DZ
   alignment = Store.alignment storeP3DZ
   peek = Store.peek storeP3DZ
   poke = Store.poke storeP3DZ

instance PointND P3DZ where
    dimProps = (False, True)
    components (Point3DZ x y z) = (x, y, Just z, Nothing)
    fromComponents (x, y, Just z, Nothing) = Point3DZ x y z
    fromComponents _ = throw $
        GeometryError "invalid transition from user data type to P3DZ"

-- P3DM ========================================================================

data P3DM = Point3DM
        { xP3DM :: {-# UNPACK #-} !Double
        , yP3DM :: {-# UNPACK #-} !Double
        , mP3DM :: {-# UNPACK #-} !Double
        } deriving (Show, Eq)

storeP3DM :: Store.Dictionary P3DM
storeP3DM = Store.run $
    liftA3 Point3DM
        (Store.element xP3DM)
        (Store.element yP3DM)
        (Store.element mP3DM)

instance Storable P3DM where
   sizeOf = Store.sizeOf storeP3DM
   alignment = Store.alignment storeP3DM
   peek = Store.peek storeP3DM
   poke = Store.poke storeP3DM

instance PointND P3DM where
    dimProps = (True, False)
    components (Point3DM x y m) = (x, y, Just m, Nothing)
    fromComponents (x, y, Just m, Nothing) = Point3DM x y m
    fromComponents _ = throw $
        GeometryError "invalid transition from user data type to P3DM"

-- P4D =========================================================================

data P4D = Point4D
        { xP4D :: {-# UNPACK #-} !Double
        , yP4D :: {-# UNPACK #-} !Double
        , zP4D :: {-# UNPACK #-} !Double
        , mP4D :: {-# UNPACK #-} !Double
        } deriving (Show, Eq)

storeP4D :: Store.Dictionary P4D
storeP4D = Store.run $
    liftA4 Point4D
        (Store.element xP4D)
        (Store.element yP4D)
        (Store.element zP4D)
        (Store.element mP4D)

instance Storable P4D where
   sizeOf = Store.sizeOf storeP4D
   alignment = Store.alignment storeP4D
   peek = Store.peek storeP4D
   poke = Store.poke storeP4D

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





