{-# LANGUAGE TypeFamilies #-}

module Database.Postgis.Trivial.Traversable.PointND where

import GHC.Base
import GHC.Show ( Show )
import Control.Exception ( throw )

import Database.Postgis.Trivial.Types
import Database.Postgis.Trivial.Cast


-- | Default 2D point
data P2D = Point2D
        { xP2D :: {-# UNPACK #-} !Double
        , yP2D :: {-# UNPACK #-} !Double
        } deriving (Show, Eq)

instance PointND P2D where
    dimProps = (False, False)
    components (Point2D x y) = (x, y, Nothing, Nothing)
    fromComponents (x, y, Nothing, Nothing) = Point2D x y
    fromComponents _ = throw $
        GeometryError "invalid transition from user data type to Point2D"

-- | Default 3D point with Z component
data P3DZ = Point3DZ
        { xP3DZ :: {-# UNPACK #-} !Double
        , yP3DZ :: {-# UNPACK #-} !Double
        , zP3DZ :: {-# UNPACK #-} !Double
        } deriving (Show, Eq)

instance PointND P3DZ where
    dimProps = (False, True)
    components (Point3DZ x y z) = (x, y, Just z, Nothing)
    fromComponents (x, y, Just z, Nothing) = Point3DZ x y z
    fromComponents _ = throw $
        GeometryError "invalid transition from user data type to Point3DZ"

-- | Default 3D point with M component
data P3DM = Point3DM
        { xP3DM :: {-# UNPACK #-} !Double
        , yP3DM :: {-# UNPACK #-} !Double
        , mP3DM :: {-# UNPACK #-} !Double
        } deriving (Show, Eq)

instance PointND P3DM where
    dimProps = (True, False)
    components (Point3DM x y m) = (x, y, Just m, Nothing)
    fromComponents (x, y, Just m, Nothing) = Point3DM x y m
    fromComponents _ = throw $
        GeometryError "invalid transition from user data type to Point2DM"

-- | Default point with Z and M component
data P4D = Point4D
        { xP4D :: {-# UNPACK #-} !Double
        , yP4D :: {-# UNPACK #-} !Double
        , zP4D :: {-# UNPACK #-} !Double
        , mP4D :: {-# UNPACK #-} !Double
        } deriving (Show, Eq)

instance PointND P4D where
    dimProps = (True, True)
    components (Point4D x y z m) = (x, y, Just z, Just m)
    fromComponents (x, y, Just z, Just m) = Point4D x y z m
    fromComponents _ = throw $
        GeometryError "invalid transition from user data type to Point4D"

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
