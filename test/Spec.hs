-----------------------------------------------------------------------------
--
-- Testing database can be created as following:
--      createdb --owner=... --template=template0 test "a postgis testing db"
-- After login as database owner execute next SQLs:
--      CREATE EXTENSION IF NOT EXISTS postgis;
--      CREATE TABLE IF NOT EXISTS points           (geom     geometry(POINT, 3785));
--      CREATE TABLE IF NOT EXISTS linestrings      (geom     geometry(LINESTRING, 3785));
--      CREATE TABLE IF NOT EXISTS linestringZs     (geom     geometry(LINESTRINGZ, 3785));
--      CREATE TABLE IF NOT EXISTS polygons         (geom     geometry(POLYGON, 3785));
--      CREATE TABLE IF NOT EXISTS multipoints      (geom     geometry(MULTIPOINT, 3785));
--      CREATE TABLE IF NOT EXISTS multilinestrings (geom     geometry(MULTILINESTRING, 3785));
--      CREATE TABLE IF NOT EXISTS multipoligons    (geom     geometry(MULTIPOLIGON, 3785));
-- In some cases, you may also need to change `dbconn` function below and/or
-- to set necessary privileges for user and tables
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import GHC.Base
import GHC.Show ( Show(show) )
import System.IO ( putStrLn )
import System.IO.Error ( userError )
import System.Exit ( exitFailure, exitSuccess )
import Control.Exception
import Data.ByteString ( ByteString )
import Database.PostgreSQL.Simple
import Test.HUnit
import qualified Tests.Traversable as T
import qualified Tests.Unboxed as U
import qualified Tests.Storable as S


dbconn :: ByteString
dbconn = "dbname=test"

checkdb :: IO ()
checkdb = bracket
    (connectPostgreSQL dbconn) close
    (\conn -> do
        [Only res] <- catch (query_ conn "SELECT PostGIS_version()")
            (\(e::SomeException) -> ioError (userError (show e)))
        if res/=(""::ByteString)
            then return ()
            else ioError (userError "Unsuccessfull probe SQL result")
    )

tests :: Test
tests = TestList
    [ TestLabel "Traversable-InsSel" $ T.tInsSel dbconn
    , TestLabel "Unboxed-Mutability" U.tMutability
    , TestLabel "Unboxed-InsSel" $ U.tInsSel dbconn
    , TestLabel "Storable-Mutability" S.tMutability
    , TestLabel "Storable-InsSel" $ S.tInsSel dbconn
    ]

main :: IO ()
main = do
    catch checkdb
        (\(_::IOException) -> do
            putStrLn
                "\nSkipping tests (see Spec.hs header for test instructions)"
            exitSuccess
        )
    cs <- runTestTT tests
    when (errors cs>0 || failures cs>0)
        exitFailure


