{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Postgis.Trivial.Traversable.Geometry where

import GHC.Base hiding ( foldr )
import Control.Monad ( mapM_ )
import Control.Exception ( throw )
import Control.Applicative ( (<$>) )

import Database.Postgis.Trivial.PGISConst
import Database.Postgis.Trivial.Types
import Database.Postgis.Trivial.Internal
import Database.Postgis.Trivial.Cast


-- | Point geometry
data Point p = Point SRID p

instance Castable p => Geometry (Point p) where
    putGeometry (Point srid v) = do
        putHeader srid pgisPoint (dimProps @(Cast p))
        putPointND (toPointND v::Cast p)
    getGeometry = do
        h <- getHeaderPre
        (v::Cast p, srid) <- if lookupType h==pgisPoint
            then makeResult h (skipHeader >> getPointND)
            else throw $
                GeometryError "invalid data for point geometry"
        return (Point srid (fromPointND v::p))

-- | Linestring geometry
data LineString t p = LineString SRID (t p)

instance (GeoChain t, Trans t p) => Geometry (LineString t p) where
    putGeometry (LineString srid vs) = do
        putHeader srid pgisLinestring (dimProps @(Cast p))
        putChain (transTo vs::t (Cast p))
    getGeometry = do
        h <- getHeaderPre
        (vs::t (Cast p), srid) <- if lookupType h==pgisLinestring
            then makeResult h (skipHeader >> getChain)
            else throw $
                GeometryError "invalid data for linestring geometry"
        return (LineString srid (transFrom vs::t p))

-- | Polygon geometry
data Polygon t2 t1 p = Polygon SRID (t2 (t1 p))

instance (Repl t2 (t1 (Cast p)), GeoChain t2, GeoChain t1, Trans t1 p) =>
        Geometry (Polygon t2 t1 p) where
    putGeometry (Polygon srid vss) = do
        putHeader srid pgisPolygon (dimProps @(Cast p))
        putChainLen $ count vss
        mapM_ (\vs -> putChain (transTo vs :: t1 (Cast p))) vss
    getGeometry = do
        h <- getHeaderPre
        (vss::t2 (t1 (Cast p)), srid) <- if lookupType h==pgisPolygon
            then makeResult h (skipHeader >> getChainLen >>= (`repl` getChain))
            else throw $
                GeometryError "invalid data for polygon geometry"
        return (Polygon srid (transFrom <$> vss::t2 (t1 p)))

-- | MultiPoint geometry
data MultiPoint t p = MultiPoint SRID (t p)

instance (Repl t (Cast p), GeoChain t, Trans t p) =>
        Geometry (MultiPoint t p) where
    putGeometry (MultiPoint srid vs) = do
        putHeader srid pgisMultiPoint (dimProps @(Cast p))
        putChainLen $ count vs
        mapM_ (\v -> do
            putGeometry (Point srid v :: Point p)
            ) vs
    getGeometry = do
        h <- getHeaderPre
        (vs::t (Cast p), srid) <- if lookupType h==pgisMultiPoint
            then makeResult h (
                skipHeader >> getChainLen >>= (`repl` (skipHeader >> getPointND))
                )
            else throw $
                GeometryError "invalid data for multipoint geometry"
        return (MultiPoint srid (transFrom vs::t p))

-- | MultiLineString geometry
data MultiLineString t2 t1 p = MultiLineString SRID (t2 (t1 p))

instance (Repl t2 (t1 (Cast p)), GeoChain t2, GeoChain t1, Trans t1 p) =>
        Geometry (MultiLineString t2 t1 p) where
    putGeometry (MultiLineString srid vss) = do
        putHeader srid pgisMultiLinestring (dimProps @(Cast p))
        putChainLen $ count vss
        mapM_ (\vs -> do
            putGeometry (LineString srid vs :: LineString t1 p)
            ) vss
    getGeometry = do
        h <- getHeaderPre
        (vss::t2 (t1 (Cast p)), srid) <- if lookupType h==pgisMultiLinestring
            then makeResult h (
                skipHeader >> getChainLen >>= (`repl` (skipHeader >> getChain))
                )
            else throw $
                GeometryError "invalid data for multilinestring geometry"
        return (MultiLineString srid (transFrom <$> vss::t2 (t1 p)))

-- | MultiPolygon geometry
data MultiPolygon t3 t2 t1 p = MultiPolygon SRID (t3 (t2 (t1 p)))

instance (Repl t3 (t2 (t1 (Cast p))), Repl t2 (t1 (Cast p)), GeoChain t3, GeoChain t2,
        GeoChain t1, Trans t1 p) => Geometry (MultiPolygon t3 t2 t1 p) where
    putGeometry (MultiPolygon srid vsss) = do
        putHeader srid pgisMultiPolygon (dimProps @(Cast p))
        putChainLen $ count vsss
        mapM_ (\vss -> do
            putGeometry (Polygon srid vss :: Polygon t2 t1 p)
            ) vsss
    getGeometry = do
        h <- getHeaderPre
        (vsss::t3 (t2 (t1 (Cast p))), srid) <- if lookupType h==pgisMultiPolygon
            then makeResult h (do
                skipHeader >> getChainLen >>= (`repl` (skipHeader >> getChainLen
                    >>= (`repl` getChain)))
                )
            else throw $
                GeometryError "invalid data for multipolygon geometry"
        return (MultiPolygon srid ((transFrom <$>) <$> vsss::t3 (t2 (t1 p))))

-- Helpers

-- | Point
putPoint :: Castable p => SRID -> p -> Geo (Point p)
putPoint srid p = Geo (Point srid p)

getPoint :: Geo (Point p) -> (SRID, p)
getPoint (Geo (Point srid p)) = (srid, p)

-- | Linestring
putLS :: SRID -> t p -> Geo (LineString t p)
putLS srid ps = Geo (LineString srid ps)

getLS :: Geo (LineString t p) -> (SRID, t p)
getLS (Geo (LineString srid vs)) = (srid, vs)

-- | Polygon
putPoly :: SRID -> t2 (t1 p) -> Geo (Polygon t2 t1 p)
putPoly srid pss = Geo (Polygon srid pss)

getPoly :: Geo (Polygon t2 t1 p) -> (SRID, t2 (t1 p))
getPoly (Geo (Polygon srid vss)) = (srid, vss)

-- | MultiPoint
putMPoint :: SRID -> t p -> Geo (MultiPoint t p)
putMPoint srid ps = Geo (MultiPoint srid ps)

getMPoint :: Geo (MultiPoint t p) -> (SRID, t p)
getMPoint (Geo (MultiPoint srid vs)) = (srid, vs)

-- | MultiLineString
putMLS :: SRID -> t2 (t1 p) -> Geo (MultiLineString t2 t1 p)
putMLS srid pss = Geo (MultiLineString srid pss)

getMLS :: Geo (MultiLineString t2 t1 p) -> (SRID, t2 (t1 p))
getMLS (Geo (MultiLineString srid vs)) = (srid, vs)

-- | MultiPolygon
putMPoly :: SRID -> t3 (t2 (t1 p)) -> Geo (MultiPolygon t3 t2 t1 p)
putMPoly srid psss = Geo (MultiPolygon srid psss)

getMPoly :: Geo (MultiPolygon t3 t2 t1 p) -> (SRID, t3 (t2 (t1 p)))
getMPoly (Geo (MultiPolygon srid vs)) = (srid, vs)





