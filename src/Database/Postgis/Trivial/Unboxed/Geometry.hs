{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Database.Postgis.Trivial.Unboxed.Geometry where

import GHC.Base hiding ( foldr )
import Control.Monad ( mapM_ )
import Control.Exception ( throw )
import Control.Applicative ( (<$>) )
import qualified Data.Vector.Unboxed as VU

import Database.Postgis.Trivial.PGISConst
import Database.Postgis.Trivial.Types
import Database.Postgis.Trivial.Internal
import Database.Postgis.Trivial.Cast


-- | Translator of Unboxed vectors
transToU :: (Castable p, VU.Unbox p, VU.Unbox (Cast p)) =>
        VU.Vector p -> VU.Vector (Cast p)
transToU = VU.map toPointND
transFromU :: (Castable p, VU.Unbox p, VU.Unbox (Cast p)) =>
        VU.Vector (Cast p) -> VU.Vector p
transFromU = VU.map fromPointND

-- | Point chain is a base structural component of various geometries
putChainU :: (PointND a, VU.Unbox a) => Putter (VU.Vector a)
putChainU vs = do
        putChainLen $ VU.length vs
        VU.mapM_ putPointND vs
getChainU :: (PointND a, VU.Unbox a) => HeaderGetter (VU.Vector a)
getChainU = getChainLen >>= (`VU.replicateM` getPointND)

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

-- | LineString geometry
data LineString p = LineString SRID (VU.Vector p)

instance (Castable p, VU.Unbox p, VU.Unbox (Cast p)) =>
        Geometry (LineString p) where
    putGeometry (LineString srid vs) = do
        putHeader srid pgisLinestring (dimProps @(Cast p))
        putChainU (transToU (vs::VU.Vector p)::VU.Vector (Cast p))
    getGeometry = do
        h <- getHeaderPre
        (vs::VU.Vector (Cast p), srid) <- if lookupType h==pgisLinestring
            then makeResult h (skipHeader >> getChainU)
            else throw $
                GeometryError "invalid data for linestring geometry"
        return (LineString srid (transFromU vs::VU.Vector p))

-- | Polygon geometry
data Polygon t2 p = Polygon SRID (t2 (VU.Vector p))

instance (Castable p, VU.Unbox p, VU.Unbox (Cast p), GeoChain t2,
        Repl t2 (VU.Vector (Cast p))) => Geometry (Polygon t2 p) where
    putGeometry (Polygon srid vss) = do
        putHeader srid pgisPolygon (dimProps @(Cast p))
        putChainLen $ count vss
        mapM_ (\vs -> putChainU (transToU vs::VU.Vector (Cast p))) vss
    getGeometry = do
        h <- getHeaderPre
        (vss::t2 (VU.Vector (Cast p)), srid) <- if lookupType h==pgisPolygon
            then makeResult h (skipHeader >> getChainLen >>= (`repl` getChainU))
            else throw $
                GeometryError "invalid data for polygon geometry"
        return (Polygon srid (transFromU <$> vss::t2 (VU.Vector p)))

-- | MultiPoint geometry
data MultiPoint p = MultiPoint SRID (VU.Vector p)

instance (Castable p, VU.Unbox p, VU.Unbox (Cast p)) =>
        Geometry (MultiPoint p) where
    putGeometry (MultiPoint srid vs) = do
        putHeader srid pgisMultiPoint (dimProps @(Cast p))
        putChainLen $ VU.length vs
        VU.mapM_ (\v -> do
            putGeometry (Point srid v :: Point p)
            ) vs
    getGeometry = do
        h <- getHeaderPre
        (vs::t (Cast p), srid) <- if lookupType h==pgisMultiPoint
            then makeResult h (
                skipHeader >> getChainLen >>=
                    (`VU.replicateM` (skipHeader >> getPointND))
                )
            else throw $
                GeometryError "invalid data for multipoint geometry"
        return (MultiPoint srid (transFromU vs::VU.Vector p))

-- | MultiLineString geometry
data MultiLineString t2 p = MultiLineString SRID (t2 (VU.Vector p))

instance (Castable p, VU.Unbox p, VU.Unbox (Cast p), GeoChain t2,
        Repl t2 (VU.Vector (Cast p))) => Geometry (MultiLineString t2 p) where
    putGeometry (MultiLineString srid vss) = do
        putHeader srid pgisMultiLinestring (dimProps @(Cast p))
        putChainLen $ count vss
        mapM_ (\vs -> do
            putGeometry (LineString srid vs :: LineString p)
            ) vss
    getGeometry = do
        h <- getHeaderPre
        (vss::t2 (t1 (Cast p)), srid) <- if lookupType h==pgisMultiLinestring
            then makeResult h (
                skipHeader >> getChainLen >>= (`repl` (skipHeader >> getChainU))
                )
            else throw $
                GeometryError "invalid data for multilinestring geometry"
        return (MultiLineString srid (transFromU <$> vss::t2 (VU.Vector p)))

-- | MultiPolygon geometry
data MultiPolygon t3 t2 p = MultiPolygon SRID (t3 (t2 (VU.Vector p)))

instance (Castable p, VU.Unbox p, VU.Unbox (Cast p),
        Repl t3 (t2 (VU.Vector (Cast p))), Repl t2 (VU.Vector (Cast p)),
        GeoChain t2, GeoChain t3) => Geometry (MultiPolygon t3 t2 p) where
    putGeometry (MultiPolygon srid vsss) = do
        putHeader srid pgisMultiPolygon (dimProps @(Cast p))
        putChainLen $ count vsss
        mapM_ (\vss -> do
            putGeometry (Polygon srid vss :: Polygon t2 p)
            ) vsss
    getGeometry = do
        h <- getHeaderPre
        (vsss::t3 (t2 (VU.Vector (Cast p))), srid) <-
            if lookupType h==pgisMultiPolygon
            then makeResult h (do
                skipHeader >> getChainLen >>= (`repl` (skipHeader >> getChainLen
                    >>= (`repl` getChainU)))
                )
            else throw $
                GeometryError "invalid data for multipolygon geometry"
        return (MultiPolygon srid ((transFromU <$>) <$>
            vsss::t3 (t2 (VU.Vector p))))

-- Helpers

-- | Point
putPoint :: Castable p => SRID -> p -> Geo (Point p)
putPoint srid p = Geo (Point srid p)

getPoint :: Castable p => Geo (Point p) -> (SRID, p)
getPoint (Geo (Point srid v)) = (srid, v)

-- | Linestring
putLS :: SRID -> VU.Vector p -> Geo (LineString p)
putLS srid ps = Geo (LineString srid ps)

getLS :: Geo (LineString p) -> (SRID, VU.Vector p)
getLS (Geo (LineString srid vs)) = (srid, vs)

-- | Polygon
putPoly :: SRID -> t2 (VU.Vector p) -> Geo (Polygon t2 p)
putPoly srid pss = Geo (Polygon srid pss)

getPoly :: Geo (Polygon t2 p) -> (SRID, t2 (VU.Vector p))
getPoly (Geo (Polygon srid vss)) = (srid, vss)

-- | MultiPoint
putMPoint :: SRID -> VU.Vector p -> Geo (MultiPoint p)
putMPoint srid ps = Geo (MultiPoint srid ps)

getMPoint :: Geo (MultiPoint p) -> (SRID, VU.Vector p)
getMPoint (Geo (MultiPoint srid vs)) = (srid, vs)

-- | MultiLineString
putMLS :: SRID -> t2 (VU.Vector p) -> Geo (MultiLineString t2 p)
putMLS srid pss = Geo (MultiLineString srid pss)

getMLS :: Geo (MultiLineString t2 p) -> (SRID, t2 (VU.Vector p))
getMLS (Geo (MultiLineString srid vs)) = (srid, vs)

-- | MultiPolygon
putMPoly :: SRID -> t3 (t2 (VU.Vector p)) -> Geo (MultiPolygon t3 t2 p)
putMPoly srid psss = Geo (MultiPolygon srid psss)

getMPoly :: Geo (MultiPolygon t3 t2 p) -> (SRID, t3 (t2 (VU.Vector p)))
getMPoly (Geo (MultiPolygon srid vs)) = (srid, vs)

