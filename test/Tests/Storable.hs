{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Storable where

import GHC.Base
import GHC.Show ( Show(..) )
import Foreign.Storable.Record as Store
import Foreign.Storable ( Storable (..) )
import Control.Exception ( bracket )
import Data.List ( length )
import qualified Data.Vector.Storable as VS
import Test.HUnit
import Database.PostgreSQL.Simple

import Database.Postgis.Trivial.Storable
import Data.ByteString (ByteString)


-- type InnerData = VS.Vector P2D

data GeoDuo = GeoDuo
        { dx :: !Double
        , dy :: !Double
        } deriving (Show, Eq)

storeGeoDuo :: Store.Dictionary GeoDuo
storeGeoDuo = Store.run $
    liftA2 GeoDuo
        (Store.element dx)
        (Store.element dy)

instance Storable GeoDuo where
   sizeOf = Store.sizeOf storeGeoDuo
   alignment = Store.alignment storeGeoDuo
   peek = Store.peek storeGeoDuo
   poke = Store.poke storeGeoDuo

type instance Cast GeoDuo = P2DS

instance Castable GeoDuo where
    toPointND (GeoDuo y x) = Point2DS x y
    fromPointND (Point2DS x y) = GeoDuo y x

type SomeData = VS.Vector GeoDuo


data GeoTrio = GeoTrio
        { tx :: !Double
        , ty :: !Double
        , tz :: !Double
        } deriving (Show, Eq)

storeGeoTrio :: Store.Dictionary GeoTrio
storeGeoTrio = Store.run $
    liftA3 GeoTrio
        (Store.element tx)
        (Store.element ty)
        (Store.element tz)

instance Storable GeoTrio where
   sizeOf = Store.sizeOf storeGeoTrio
   alignment = Store.alignment storeGeoTrio
   peek = Store.peek storeGeoTrio
   poke = Store.poke storeGeoTrio

type instance Cast GeoTrio = P3DZS

instance Castable GeoTrio where
    toPointND (GeoTrio x y z) = Point3DZS x y z
    fromPointND (Point3DZS x y z) = GeoTrio x y z

type SomeDataZ = VS.Vector GeoTrio

tMutability ::Test
tMutability = TestList
    [ "safe update" ~:
        VS.fromList [Point2DS 1 2, Point2DS 1.5 2.5, Point2DS 2.5 3, Point2DS 1 2] VS.//
            [(2, Point2DS 2 13), (0, Point2DS 10 2)]
        ~?= VS.fromList [Point2DS 10 2, Point2DS 1.5 2.5, Point2DS 2 13, Point2DS 1 2]
    , "unsafe update" ~:
        VS.unsafeUpd
            (VS.fromList [Point2DS 1 2, Point2DS 1.5 2.5, Point2DS 2.5 3, Point2DS 1 2])
            [(2, Point2DS 2 13), (0, Point2DS 10 2)]
        ~?= VS.fromList [Point2DS 10 2, Point2DS 1.5 2.5, Point2DS 2 13, Point2DS 1 2]
    ]

tInsSel :: ByteString -> Test
tInsSel dbconn = TestList
    [ "linestring (2D, Unboxed Vector)" ~: bracket
        (connectPostgreSQL dbconn) close
        (\conn -> do
            let srid = Just 3785 :: SRID
                ls0 = VS.fromList [Point2DS 1 2, Point2DS 1.5 2.5, Point2DS 2.5 3, Point2DS 1 2]
            _ <- execute_ conn "TRUNCATE linestrings"
            _ <- execute conn "INSERT INTO linestrings (geom) VALUES (?)"
                (Only (putLS srid ls0))
            [Only res] <- query_ conn "SELECT * FROM linestrings"
            let (srid', ls0') = getLS res
            _ <- execute_ conn "TRUNCATE linestrings"
            VS.length ls0'==VS.length ls0 && srid'==srid && ls0'==ls0 @?= True
        )
    , "linestring (2D, Unboxed Vector)" ~: bracket
        (connectPostgreSQL dbconn) close
        (\conn -> do
            let srid = Just 3785 :: SRID
                ls0 = VS.fromList [GeoDuo 1 2, GeoDuo 1.5 2.5, GeoDuo 2.5 3, GeoDuo 1 2]
            _ <- execute_ conn "TRUNCATE linestrings"
            _ <- execute conn "INSERT INTO linestrings (geom) VALUES (?)"
                (Only (putLS srid ls0))
            [Only res] <- query_ conn "SELECT * FROM linestrings"
            let (srid', ls0') = getLS res
            _ <- execute_ conn "TRUNCATE linestrings"
            VS.length ls0'==VS.length ls0 && srid'==srid && ls0'==ls0 @?= True
        )
    , "linestring (3D, Unboxed Vector)" ~: bracket
        (connectPostgreSQL dbconn) close
        (\conn -> do
            let srid = Just 3785 :: SRID
                ls0 = VS.fromList [GeoTrio 1 2 0, GeoTrio 1.5 2.5 10, GeoTrio 2.5 3 20, GeoTrio 1 2 30]
            _ <- execute_ conn "TRUNCATE linestringZs"
            _ <- execute conn "INSERT INTO linestringZs (geom) VALUES (?)"
                (Only (putLS srid ls0))
            [Only res] <- query_ conn "SELECT * FROM linestringZs"
            let (srid', ls0') = getLS res
            _ <- execute_ conn "TRUNCATE linestringZs"
            VS.length ls0'==VS.length ls0 && srid'==srid && ls0'==ls0 @?= True
        )
    , "multilinestring" ~: bracket
        (connectPostgreSQL dbconn) close
        (\conn -> do
            let srid = Just 3785 :: SRID
                mls0 =
                    [ VS.fromList [GeoDuo 1 2, GeoDuo 1.5 2.5, GeoDuo 2.5 3, GeoDuo 1 2]
                    , VS.fromList []
                    , VS.fromList [GeoDuo 1 2] ]
            _ <- execute_ conn "TRUNCATE multilinestrings"
            _ <- execute conn "INSERT INTO multilinestrings (geom) VALUES (?)"
                (Only (putMLS srid mls0))
            [Only res] <- query_ conn "SELECT * FROM multilinestrings"
            let (srid', mls0'::[SomeData]) = getMLS res
            _ <- execute_ conn "TRUNCATE multilinestrings"
            length mls0'==length mls0 && srid'==srid && mls0'==mls0 @?= True
        )
    ]



