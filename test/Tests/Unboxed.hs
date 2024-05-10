{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Unboxed where

import GHC.Base
import Control.Exception ( bracket )
import Data.List
import qualified Data.Vector.Unboxed as VU
import Test.HUnit
import Database.PostgreSQL.Simple

import Database.Postgis.Trivial.Unboxed
import Data.ByteString (ByteString)

-- type InnerData = VU.Vector P2D

type instance Cast (Double, Double, Double) = P3DZU
instance Castable (Double, Double, Double) where
    toPointND (x, y, z) = Point3DZU x y z
    fromPointND (Point3DZU x y z) = (x, y, z)
type SomeDataZ = VU.Vector (Double, Double, Double)

tMutability :: Test
tMutability = TestList
    [ "safe update" ~:
        VU.fromList [Point2DU 1 2, Point2DU 1.5 2.5, Point2DU 2.5 3, Point2DU 1 2] VU.//
            [(2, Point2DU 2 13), (0, Point2DU 10 2)]
        ~?= VU.fromList [Point2DU 10 2, Point2DU 1.5 2.5, Point2DU 2 13, Point2DU 1 2]
    , "unsafe update" ~:
        VU.unsafeUpd
            (VU.fromList [Point2DU 1 2, Point2DU 1.5 2.5, Point2DU 2.5 3, Point2DU 1 2])
            [(2, Point2DU 2 13), (0, Point2DU 10 2)]
        ~?= VU.fromList [Point2DU 10 2, Point2DU 1.5 2.5, Point2DU 2 13, Point2DU 1 2]
    ]

tInsSel :: ByteString -> Test
tInsSel dbconn = TestList
    [ "linestring (2D, Unboxed Vector)" ~: bracket
        (connectPostgreSQL dbconn) close
        (\conn -> do
            let srid = Just 3785 :: SRID
                ls0 = VU.fromList [Point2DU 1 2, Point2DU 1.5 2.5, Point2DU 2.5 3, Point2DU 1 2]
            _ <- execute_ conn "TRUNCATE linestrings"
            _ <- execute conn "INSERT INTO linestrings (geom) VALUES (?)"
                (Only (putLS srid ls0))
            -- return True
            [Only res] <- query_ conn "SELECT * FROM linestrings"
            let (srid', ls0') = getLS res
            _ <- execute_ conn "TRUNCATE linestrings"
            VU.length ls0'==VU.length ls0 && srid'==srid && ls0'==ls0 @?= True
        )
    , "linestring (2D, Unboxed Vector)" ~: bracket
        (connectPostgreSQL dbconn) close
        (\conn -> do
            let srid = Just 3785 :: SRID
                ls0 = VU.fromList [Point2DU 1 2, Point2DU 1.5 2.5, Point2DU 2.5 3, Point2DU 1 2]
            _ <- execute_ conn "TRUNCATE linestrings"
            _ <- execute conn "INSERT INTO linestrings (geom) VALUES (?)"
                (Only (putLS srid ls0))
            -- return True
            [Only res] <- query_ conn "SELECT * FROM linestrings"
            let (srid', ls0') = getLS res
            _ <- execute_ conn "TRUNCATE linestrings"
            VU.length ls0'==VU.length ls0 && srid'==srid && ls0'==ls0 @?= True
        )
    , "linestring (3D, Unboxed Vector)" ~: bracket
        (connectPostgreSQL dbconn) close
        (\conn -> do
            let srid = Just 3785 :: SRID
                ls0 = VU.fromList [(1, 2, 0), (1.5, 2.5, 10), (2.5, 3, 20), (1, 2, 30)] :: SomeDataZ
            _ <- execute_ conn "TRUNCATE linestringZs"
            _ <- execute conn "INSERT INTO linestringZs (geom) VALUES (?)"
                (Only (putLS srid ls0))
            [Only res] <- query_ conn "SELECT * FROM linestringZs"
            let (srid', ls0') = getLS res
            _ <- execute_ conn "TRUNCATE linestringZs"
            VU.length ls0'==VU.length ls0 && srid'==srid && ls0'==ls0 @?= True
        )
    , "multilinestring" ~: bracket
        (connectPostgreSQL dbconn) close
        (\conn -> do
            let srid = Just 3785 :: SRID
                mls0 =
                    [ VU.fromList [Point2DU 1 2, Point2DU 1.5 2.5, Point2DU 2.5 3, Point2DU 1 2]
                    , VU.fromList []
                    , VU.fromList [Point2DU 1 2] ]
            _ <- execute_ conn "TRUNCATE multilinestrings"
            _ <- execute conn "INSERT INTO multilinestrings (geom) VALUES (?)"
                (Only (putMLS srid mls0))
            [Only res] <- query_ conn "SELECT * FROM multilinestrings"
            let (srid', mls0') = getMLS res
            _ <- execute_ conn "TRUNCATE multilinestrings"
            length mls0'==length mls0 && srid'==srid && mls0'==mls0 @?= True
        )
    ]



