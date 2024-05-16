-- | Allows PostGIS to work with geospatial data enclosed in
-- Traversable data structures with Storable Vectors as the most
-- inner structures.
--
-- Example of suitable data types for LineString and Polygon.
--
-- > {-# LANGUAGE TypeFamilies #-}
-- >
-- > import qualified Data.Vector.Storable as VS
-- > import Foreign.Storable.Record as Store
-- > import Foreign.Storable ( Storable (..) )
-- >
-- > data GeoDuo = GeoDuo
-- >         { dx :: !Double
-- >         , dy :: !Double
-- >         } deriving (Show, Eq)
-- >
-- > storeGeoDuo :: Store.Dictionary GeoDuo
-- > storeGeoDuo = Store.run $
-- >     liftA2 GeoDuo
-- >         (Store.element dx)
-- >         (Store.element dy)
-- >
-- > instance Storable GeoDuo where
-- >    sizeOf = Store.sizeOf storeGeoDuo
-- >    alignment = Store.alignment storeGeoDuo
-- >    peek = Store.peek storeGeoDuo
-- >    poke = Store.poke storeGeoDuo
-- >
-- > type instance Cast GeoDuo = P2D
-- >
-- > instance Castable GeoDuo where
-- >     toPointND (GeoDuo y x) = Point2D x y
-- >     fromPointND (Point2D x y) = GeoDuo y x
-- >
-- > type LineStringData = VS.Vector GeoDuo
-- > type PolygonData = [VS.Vector GeoDuo]
--

module Database.Postgis.Trivial.Storable (
    module Database.Postgis.Trivial.Types
  , module Database.Postgis.Trivial.Cast
  , module Database.Postgis.Trivial.Storable.PointND
  , module Database.Postgis.Trivial.Storable.Geometry
) where

import Database.Postgis.Trivial.Types
import Database.Postgis.Trivial.Cast
import Database.Postgis.Trivial.Storable.PointND
import Database.Postgis.Trivial.Storable.Geometry
