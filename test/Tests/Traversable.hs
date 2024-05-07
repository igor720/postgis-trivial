{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Traversable where

import GHC.Base
import GHC.Show ( Show(..) )
import Control.Applicative ( (<$>) )
import Control.Exception ( bracket )
import Data.List
import qualified Data.Vector as V
import Test.HUnit
import Database.PostgreSQL.Simple

import Database.Postgis.Trivial


data LatLon =
        LatLon {-# UNPACK #-} !Double {-# UNPACK #-} !Double
        deriving (Show, Eq)

type instance Cast LatLon = P2D

instance Castable LatLon where
    toPointND (LatLon y x) = Point2D x y
    fromPointND (Point2D x y) = LatLon y x

data LatLonZ =
        LatLonZ {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double
        deriving (Show, Eq)

type instance Cast LatLonZ = P3DZ

instance Castable LatLonZ where
    toPointND (LatLonZ y x z) = Point3DZ x y z
    fromPointND (Point3DZ x y z) = LatLonZ y x z

type instance Cast (Double, Double) = P2D

instance Castable (Double, Double) where
    toPointND (x, y) = Point2D x y
    fromPointND (Point2D x y) = (x, y)

tInsSel :: Test
tInsSel = TestList
    [ "geometry point (PointND)" ~: bracket
        (connectPostgreSQL dbconn) close
        (\conn -> do
            let srid = Just 3785 :: SRID
                point0 = Point2D 1 2
            _ <- execute_ conn "TRUNCATE points"
            _ <- execute conn "INSERT INTO points (geom) VALUES (?)"
                (Only (putPoint srid point0))
            [Only res] <- query_ conn "SELECT * FROM points"
            let (srid', point0') = getPoint res
            (point0', srid') @?= (point0, srid)
        )
    , "geometry point (Pair)" ~: bracket
        (connectPostgreSQL dbconn) close
        (\conn -> do
            let srid = Just 3785 :: SRID
                point0 = (1, 2) :: (Double, Double)
            _ <- execute_ conn "TRUNCATE points"
            _ <- execute conn "INSERT INTO points (geom) VALUES (?)"
                (Only (putPoint srid point0))
            [Only res] <- query_ conn "SELECT * FROM points"
            let (srid', point0') = getPoint res
            (point0', srid') @?= (point0, srid)
        )
    , "geometry points ([LatLon, LatLon])" ~: bracket
        (connectPostgreSQL dbconn) close
        (\conn -> do
            let srid = Just 3785 :: SRID
                point0 = LatLon 1 2 :: LatLon
                point1 = LatLon 11 2 :: LatLon
            _ <- execute_ conn "TRUNCATE points"
            _ <- executeMany conn "INSERT INTO points (geom) VALUES (?)"
                (Only <$> [putPoint srid point0, putPoint srid point1])
            res <- map (getPoint . fromOnly) <$> query_ conn "SELECT * FROM points"
            _ <- execute_ conn "TRUNCATE points"
            res @?= [(srid, point0), (srid, point1)]
        )
    , "linestring (3D, Vector)" ~: bracket
        (connectPostgreSQL dbconn) close
        (\conn -> do
            let srid = Just 3785 :: SRID
                ls0 = V.fromList [Point3DZ 1 2 0, Point3DZ 1.5 2.5 10, Point3DZ 2.5 3 20, Point3DZ 1 2 30]
            _ <- execute_ conn "TRUNCATE linestringZs"
            _ <- execute conn "INSERT INTO linestringZs (geom) VALUES (?)"
                (Only (putLS srid ls0))
            [Only res] <- query_ conn "SELECT * FROM linestringZs"
            let (srid', ls0') = getLS res
            _ <- execute_ conn "TRUNCATE linestringZs"
            length ls0'==length ls0 && srid'==srid && all (==True) (V.zipWith (==) ls0' ls0) @?= True
        )
    , "linestring (3D, Vector), without helpers" ~: bracket
        (connectPostgreSQL dbconn) close
        (\conn -> do
            let srid = Just 3785 :: SRID
                ls0 = V.fromList [Point3DZ 1 2 0, Point3DZ 1.5 2.5 10, Point3DZ 2.5 3 20, Point3DZ 1 2 30]
            _ <- execute_ conn "TRUNCATE linestringZs"
            _ <- execute conn "INSERT INTO linestringZs (geom) VALUES (?)"
                (Only (LineString srid ls0))
            [Only res] <- query_ conn "SELECT * FROM linestringZs"
            let LineString srid' ls0' = res
            _ <- execute_ conn "TRUNCATE linestringZs"
            length ls0'==length ls0 && srid'==srid && all (==True) (V.zipWith (==) ls0' ls0) @?= True
        )
    , "linestring (empty)" ~: bracket
        (connectPostgreSQL dbconn) close
        (\conn -> do
            let srid = Just 3785 :: SRID
                ls0 = [] :: [LatLonZ]
            _ <- execute_ conn "TRUNCATE linestringZs"
            _ <- execute conn "INSERT INTO linestringZs (geom) VALUES (?)"
                (Only (putLS srid ls0))
            [Only res] <- query_ conn "SELECT * FROM linestringZs LIMIT 1"
            let (srid', ls0') = getLS res
            _ <- execute_ conn "TRUNCATE linestringZs"
            length ls0'==length ls0 && srid'==srid && all (==True) (zipWith (==) ls0' ls0) @?= True
        )
    , "polygon" ~: bracket
        (connectPostgreSQL dbconn) close
        (\conn -> do
            let srid = Just 3785 :: SRID
                polygon0 = [[Point2D 1 2, Point2D 1.5 2.5, Point2D 2.5 3, Point2D 1 2]]
            _ <- execute_ conn "TRUNCATE polygons"
            _ <- execute conn "INSERT INTO polygons (geom) VALUES (?)"
                (Only (putPoly srid polygon0))
            [Only res] <- query_ conn "SELECT * FROM polygons"
            let (srid', polygon0') = getPoly res
            _ <- execute_ conn "TRUNCATE polygons"
            not (null polygon0')
                && length (head polygon0')==length (head polygon0)
                && srid'==srid
                && all (==True) (zipWith (==) (head polygon0') (head polygon0)) @?= True
        )
    , "multipoint" ~: bracket
        (connectPostgreSQL dbconn) close
        (\conn -> do
            let srid = Just 3785 :: SRID
                mp0 = [Point2D 1 2, Point2D 1.5 2.5, Point2D 2.5 3, Point2D 1 2]
            _ <- execute_ conn "TRUNCATE multipoints"
            _ <- execute conn "INSERT INTO multipoints (geom) VALUES (?)"
                (Only (putMPoint srid mp0))
            [Only res] <- query_ conn "SELECT * FROM multipoints"
            let (srid', mp0') = getMPoint res
            _ <- execute_ conn "TRUNCATE multipoints"
            length mp0'==length mp0 && srid'==srid && all (==True) (zipWith (==) mp0' mp0) @?= True
        )
    , "multilinestring" ~: bracket
        (connectPostgreSQL dbconn) close
        (\conn -> do
            let srid = Just 3785 :: SRID
                mls0 = [[Point2D 1 2, Point2D 1.5 2.5, Point2D 2.5 3, Point2D 1 2], [], [Point2D 1 2]]
            _ <- execute_ conn "TRUNCATE multilinestrings"
            _ <- execute conn "INSERT INTO multilinestrings (geom) VALUES (?)"
                (Only (putMLS srid mls0))
            [Only res] <- query_ conn "SELECT * FROM multilinestrings"
            let (srid', mls0') = getMLS res
            _ <- execute_ conn "TRUNCATE multilinestrings"
            length mls0'==length mls0 && srid'==srid && all (==True) (zipWith (==) mls0' mls0) @?= True
        )
    , "multipolygon" ~: bracket
        (connectPostgreSQL dbconn) close
        (\conn -> do
            let srid = Just 3785 :: SRID
                mps0 = [[[Point2D 1 2, Point2D 1.5 2.5, Point2D 2.5 3, Point2D 1 2], []], [[Point2D 1 2]]]
            _ <- execute_ conn "TRUNCATE multipolygons"
            _ <- execute conn "INSERT INTO multipolygons (geom) VALUES (?)"
                (Only (putMPoly srid mps0))
            [Only res] <- query_ conn "SELECT * FROM multipolygons"
            let (srid', mps0') = getMPoly res
            _ <- execute_ conn "TRUNCATE multipolygons"
            length mps0'==length mps0 && srid'==srid && all (==True) (zipWith (==) mps0' mps0) @?= True
        )
    ] where
        dbconn = "dbname=test"



