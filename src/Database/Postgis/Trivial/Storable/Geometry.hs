{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Postgis.Trivial.Storable.Geometry where

import GHC.Base hiding ( foldr )
import Control.Monad ( mapM_ )
import Control.Exception ( throw )
import Control.Applicative ( (<$>) )
import qualified Data.Vector.Storable as VS

import Database.Postgis.Trivial.PGISConst
import Database.Postgis.Trivial.Types
import Database.Postgis.Trivial.Internal
import Database.Postgis.Trivial.Cast


-- | Translator of Unboxed vectors
transToS :: (Castable p, VS.Storable p, VS.Storable (Cast p)) =>
        VS.Vector p -> VS.Vector (Cast p)
transToS = VS.map toPointND
transFromS :: (Castable p, VS.Storable p, VS.Storable (Cast p)) =>
        VS.Vector (Cast p) -> VS.Vector p
transFromS = VS.map fromPointND

-- | Point chain is a base structural component of various geometries
putChainS :: (PointND a, VS.Storable a) => Putter (VS.Vector a)
putChainS vs = do
        putChainLen $ VS.length vs
        VS.mapM_ putPointND vs
getChainS :: (PointND a, VS.Storable a) => HeaderGetter (VS.Vector a)
getChainS = getChainLen >>= (`VS.replicateM` getPointND)

-- | Point geometry
data Point p = Point SRID p

instance Castable p => Geometry (Point p) where
    putGeometry (Point srid v) = do
        putHeader srid pgisPoint (dimProps @(Cast p))
        putPointND (toPointND v::Cast p)
    getGeometry = do
        h <- getHeaderPre
        (v::(Cast p), srid) <- if lookupType h==pgisPoint
            then makeResult h (skipHeader >> getPointND)
            else throw $
                GeometryError "invalid data for point geometry"
        return (Point srid (fromPointND v::p))

-- | LineString geometry
data LineString p = LineString SRID (VS.Vector p)

instance (Castable p, VS.Storable p, VS.Storable (Cast p)) =>
        Geometry (LineString p) where
    putGeometry (LineString srid vs) = do
        putHeader srid pgisLinestring (dimProps @(Cast p))
        putChainS (transToS (vs::VS.Vector p)::VS.Vector (Cast p))
    getGeometry = do
        h <- getHeaderPre
        (vs::VS.Vector (Cast p), srid) <-
            if lookupType h==pgisLinestring
            then makeResult h (skipHeader >> getChainS)
            else throw $
                GeometryError "invalid data for linestring geometry"
        return (LineString srid (transFromS vs::VS.Vector p))

-- | Polygon geometry
data Polygon t2 p = Polygon SRID (t2 (VS.Vector p))

instance (Castable p, VS.Storable p, VS.Storable (Cast p), GeoChain t2,
        Repl t2 (VS.Vector (Cast p))) => Geometry (Polygon t2 p) where
    putGeometry (Polygon srid vss) = do
        putHeader srid pgisPolygon (dimProps @(Cast p))
        putChainLen $ count vss
        mapM_ (\vs -> putChainS (transToS vs::VS.Vector (Cast p))) vss
    getGeometry = do
        h <- getHeaderPre
        (vss::t2 (VS.Vector (Cast p)), srid) <- if lookupType h==pgisPolygon
            then makeResult h (skipHeader >> getChainLen >>= (`repl` getChainS))
            else throw $
                GeometryError "invalid data for polygon geometry"
        return (Polygon srid (transFromS <$> vss::t2 (VS.Vector p)))

-- | MultiPoint geometry
data MultiPoint p = MultiPoint SRID (VS.Vector p)

instance (Castable p, VS.Storable p, VS.Storable (Cast p)) =>
        Geometry (MultiPoint p) where
    putGeometry (MultiPoint srid vs) = do
        putHeader srid pgisMultiPoint (dimProps @(Cast p))
        putChainLen $ VS.length vs
        VS.mapM_ (\v -> do
            putGeometry (Point srid v :: Point p)
            ) vs
    getGeometry = do
        h <- getHeaderPre
        (vs::t (Cast p), srid) <- if lookupType h==pgisMultiPoint
            then makeResult h (
                skipHeader >> getChainLen >>=
                    (`VS.replicateM` (skipHeader >> getPointND))
                )
            else throw $
                GeometryError "invalid data for multipoint geometry"
        return (MultiPoint srid (transFromS vs::VS.Vector p))

-- | MultiLineString geometry
data MultiLineString t2 p = MultiLineString SRID (t2 (VS.Vector p))

instance (Castable p, VS.Storable p, VS.Storable (Cast p), GeoChain t2,
        Repl t2 (VS.Vector (Cast p))) => Geometry (MultiLineString t2 p) where
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
                skipHeader >> getChainLen >>= (`repl` (skipHeader >> getChainS))
                )
            else throw $
                GeometryError "invalid data for multilinestring geometry"
        return (MultiLineString srid (transFromS <$> vss::t2 (VS.Vector p)))

-- | MultiPolygon geometry
data MultiPolygon t3 t2 p = MultiPolygon SRID (t3 (t2 (VS.Vector p)))

instance (Castable p, VS.Storable p, VS.Storable (Cast p),
        Repl t3 (t2 (VS.Vector (Cast p))), Repl t2 (VS.Vector (Cast p)),
        GeoChain t2, GeoChain t3) => Geometry (MultiPolygon t3 t2 p) where
    putGeometry (MultiPolygon srid vsss) = do
        putHeader srid pgisMultiPolygon (dimProps @(Cast p))
        putChainLen $ count vsss
        mapM_ (\vss -> do
            putGeometry (Polygon srid vss :: Polygon t2 p)
            ) vsss
    getGeometry = do
        h <- getHeaderPre
        (vsss::t3 (t2 (VS.Vector (Cast p))), srid) <-
            if lookupType h==pgisMultiPolygon
            then makeResult h (do
                skipHeader >> getChainLen >>= (`repl` (skipHeader >> getChainLen
                    >>= (`repl` getChainS)))
                )
            else throw $
                GeometryError "invalid data for multipolygon geometry"
        return (MultiPolygon srid ((transFromS <$>) <$>
            vsss::t3 (t2 (VS.Vector p))))

-- | Helpers

-- | Point
putPoint :: Castable p => SRID -> p -> Point p
putPoint = Point

getPoint :: Castable p => Point p -> (SRID, p)
getPoint (Point srid v) = (srid, v)

-- | Linestring
putLS :: SRID -> VS.Vector p -> LineString p
putLS = LineString

getLS :: LineString p -> (SRID, VS.Vector p)
getLS (LineString srid vs) = (srid, vs)

-- | Polygon
putPoly :: SRID -> t2 (VS.Vector p) -> Polygon t2 p
putPoly = Polygon

getPoly :: Polygon t2 p -> (SRID, t2 (VS.Vector p))
getPoly (Polygon srid vss) = (srid, vss)

-- | MultiPoint
putMPoint :: SRID -> VS.Vector p -> MultiPoint p
putMPoint = MultiPoint

getMPoint :: MultiPoint p -> (SRID, VS.Vector p)
getMPoint (MultiPoint srid vs) = (srid, vs)

-- | MultiLineString
putMLS :: SRID -> t2 (VS.Vector p) -> MultiLineString t2 p
putMLS = MultiLineString

getMLS :: MultiLineString t2 p -> (SRID, t2 (VS.Vector p))
getMLS (MultiLineString srid vs) = (srid, vs)

-- | MultiPolygon
putMPoly :: SRID -> t3 (t2 (VS.Vector p)) -> MultiPolygon t3 t2 p
putMPoly = MultiPolygon

getMPoly :: MultiPolygon t3 t2 p -> (SRID, t3 (t2 (VS.Vector p)))
getMPoly (MultiPolygon srid vs) = (srid, vs)

