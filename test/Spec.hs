-----------------------------------------------------------------------------
-- |
-- File        :  Spec.hs
-- Copyright   :  (c) Igor Chudaev
-- License     :  BSD3
--
-- Maintainer  :  igor720@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- HUnit testing routines
--
-- Testing database can be created as following:
--      createdb --owner=... --template=template0 test "a postgis testing db"
-- After login as database owner execute next SQLs:
--      CREATE EXTENSION IF NOT EXISTS postgis;
--      CREATE TABLE IF NOT EXISTS points           (geom     geometry(POINT, 3785));
--      CREATE TABLE IF NOT EXISTS linestringZs     (geom     geometry(LINESTRINGZ, 3785));
--      CREATE TABLE IF NOT EXISTS polygons         (geom     geometry(POLYGON, 3785));
--      CREATE TABLE IF NOT EXISTS multipoints      (geom     geometry(MULTIPOINT, 3785));
--      CREATE TABLE IF NOT EXISTS multilinestrings (geom     geometry(MULTILINESTRING, 3785));
--      CREATE TABLE IF NOT EXISTS multipoligons    (geom     geometry(MULTIPOLIGON, 3785));
-- In some cases, you may also need to set necessary privileges for user and tables
--
-----------------------------------------------------------------------------

import GHC.Base
import System.Exit ( exitFailure )
import Test.HUnit
import qualified Tests.Traversable as T
import qualified Tests.Unboxed as U
import qualified Tests.Storable as S

tests :: Test
tests = TestList
    [ TestLabel "Traversable-InsSel" T.tInsSel
    , TestLabel "Unboxed-Mutability" U.tMutability
    , TestLabel "Unboxed-InsSel" U.tInsSel
    , TestLabel "Storable-Mutability" S.tMutability
    , TestLabel "Storable-InsSel" S.tInsSel
    ]

main :: IO ()
main = do
    cs <- runTestTT tests
    when (errors cs>0 || failures cs>0)
        exitFailure


