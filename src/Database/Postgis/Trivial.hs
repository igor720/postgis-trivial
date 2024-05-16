-- | Allows PostGIS to work with Traversable geospatial data enclosed in
-- Traversable data structures.
--
-- Example of suitable data types for LineString and Polygon.
--
-- > {-# LANGUAGE TypeFamilies #-}
-- >
-- > data LatLon =
-- >         LatLon !Double !Double
-- >         deriving (Show, Eq)
-- >
-- > type instance Cast LatLon = P2D
-- >
-- > instance Castable LatLon where
-- >     toPointND (LatLon y x) = Point2D x y
-- >     fromPointND (Point2D x y) = LatLon y x
-- >
-- > type LineStringData = [LatLon]
-- > type PolygonData = [[LatLon], [LatLon]]
--

module Database.Postgis.Trivial (
    module Database.Postgis.Trivial.Types
  , module Database.Postgis.Trivial.Cast
  , module Database.Postgis.Trivial.Traversable.PointND
  , module Database.Postgis.Trivial.Traversable.Geometry
) where

import Database.Postgis.Trivial.Types
import Database.Postgis.Trivial.Cast
import Database.Postgis.Trivial.Traversable.PointND
import Database.Postgis.Trivial.Traversable.Geometry



