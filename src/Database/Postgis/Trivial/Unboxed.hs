-- | Makes it possible to operate with various data enclosed in
-- Traversable data structures with the most inner Unboxed Vectors.
--
-- Example of suitable data type for LineString and Polygon
-- > {-# LANGUAGE TypeFamilies #-}
-- >
-- > import qualified Data.Vector.Unbox as VU
-- >
-- > type instance Cast (Double, Double, Double) = P3DZ
-- >
-- > instance Castable (Double, Double, Double) where
-- >     toPointND (x, y, z) = Point3DZ x y z
-- >     fromPointND (Point3DZ x y z) = (x, y, z)
-- >
-- > type LineStringData = VU.Vector (Double, Double, Double)
-- > type PolygonData = [VU.Vector (Double, Double, Double)]

module Database.Postgis.Trivial.Unboxed (
    module Database.Postgis.Trivial.Types
  , module Database.Postgis.Trivial.Cast
  , module Database.Postgis.Trivial.Unboxed.PointND
  , module Database.Postgis.Trivial.Unboxed.Geometry
) where

import Database.Postgis.Trivial.Types
import Database.Postgis.Trivial.Cast
import Database.Postgis.Trivial.Unboxed.PointND
import Database.Postgis.Trivial.Unboxed.Geometry
