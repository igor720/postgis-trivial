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


-- | Translator of Unboxed vectors (direct)
transToS :: (Castable p, VS.Storable p, VS.Storable (Cast p)) =>
        VS.Vector p -> VS.Vector (Cast p)
transToS = VS.map toPointND

-- | Translator of Unboxed vectors (reverse)
transFromS :: (Castable p, VS.Storable p, VS.Storable (Cast p)) =>
        VS.Vector (Cast p) -> VS.Vector p
transFromS = VS.map fromPointND

-- | Chain putter
putChainS :: (PointND a, VS.Storable a) => Putter (VS.Vector a)
putChainS vs = do
        putChainLen $ VS.length vs
        VS.mapM_ putPointND vs

-- | Chain getter
getChainS :: (PointND a, VS.Storable a) => HeaderGetter (VS.Vector a)
getChainS = getChainLen >>= (`VS.replicateM` getPointND)

-- | Point geometry
data Point p = Point SRID p

instance (Castable p, VS.Storable p) => Geometry (Point p) where
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

-- | Point putter
putPoint :: Castable p => SRID -> p -> Geo (Point p)
putPoint srid p = Geo (Point srid p)

-- | Point getter
getPoint :: Geo (Point p) -> (SRID, p)
getPoint (Geo (Point srid p)) = (srid, p)

-- | Linestring putter
putLS :: SRID -> VS.Vector p -> Geo (LineString p)
putLS srid ps = Geo (LineString srid ps)

-- | LineString getter
getLS :: Geo (LineString p) -> (SRID, VS.Vector p)
getLS (Geo (LineString srid vs)) = (srid, vs)

-- | Polygon putter
putPoly :: SRID -> t2 (VS.Vector p) -> Geo (Polygon t2 p)
putPoly srid pss = Geo (Polygon srid pss)

-- | Polygon getter
getPoly :: Geo (Polygon t2 p) -> (SRID, t2 (VS.Vector p))
getPoly (Geo (Polygon srid vss)) = (srid, vss)

-- | MultiPoint putter
putMPoint :: SRID -> VS.Vector p -> Geo (MultiPoint p)
putMPoint srid ps = Geo (MultiPoint srid ps)

-- | MultiPoint getter
getMPoint :: Geo (MultiPoint p) -> (SRID, VS.Vector p)
getMPoint (Geo (MultiPoint srid vs)) = (srid, vs)

-- | MultiLineString putter
putMLS :: SRID -> t2 (VS.Vector p) -> Geo (MultiLineString t2 p)
putMLS srid pss = Geo (MultiLineString srid pss)

-- | MultiLineString getter
getMLS :: Geo (MultiLineString t2 p) -> (SRID, t2 (VS.Vector p))
getMLS (Geo (MultiLineString srid vs)) = (srid, vs)

-- | MultiPolygon putter
putMPoly :: SRID -> t3 (t2 (VS.Vector p)) -> Geo (MultiPolygon t3 t2 p)
putMPoly srid psss = Geo (MultiPolygon srid psss)

-- | MultiPolygon getter
getMPoly :: Geo (MultiPolygon t3 t2 p) -> (SRID, t3 (t2 (VS.Vector p)))
getMPoly (Geo (MultiPolygon srid vs)) = (srid, vs)

