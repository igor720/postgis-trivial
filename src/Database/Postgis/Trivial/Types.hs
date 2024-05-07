{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification                            #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Database.Postgis.Trivial.Types
    ( SRID
    , PointND (..)
    , Putter
    , Getter
    , Geometry (..)
    , GeometryError(..)
    ) where

import GHC.Base
import GHC.Show ( Show(..), ShowS )
import Control.Monad.Reader ( ReaderT )
import Control.Exception ( SomeException, Exception(..) )
import Data.Typeable ( Typeable, cast )
import Data.Binary ( Get, Put )
import Data.Binary.Get ( runGet )
import Data.Binary.Put ( runPut )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField


-- | Spatial reference identifier
type SRID = Maybe Int

-- | Inner point type class
class Typeable a => PointND a where
    dimProps :: (Bool, Bool)
    components :: a -> (Double, Double, Maybe Double, Maybe Double)
    fromComponents :: (Double, Double, Maybe Double, Maybe Double) -> a

-- | Binary Putter
type Putter a = a -> Put

writeEWKB :: Putter a -> a -> BS.ByteString
writeEWKB putter = BL.toStrict . runPut . putter

-- | Binary Getter
type Getter h = ReaderT h Get

readEWKB :: Get a -> BS.ByteString -> a
readEWKB getter bs = runGet getter (BL.fromStrict bs)

-- | Type class which defines geometry 'to/from' binary translations
class Typeable a => Geometry a where
    putGeometry :: Putter a
    getGeometry :: Get a

instance Geometry g => ToField g where
    toField = Escape . writeEWKB putGeometry

instance Geometry g => FromField g where
    fromField f m = do
        typ <- typename f
        if typ /= "geometry"
            then returnError Incompatible f (show typ)
            else case m of
                Nothing  -> returnError UnexpectedNull f ""
                Just bs -> return $ readEWKB getGeometry bs

-- | Exeptions

-- | Geometry exceptions
data SomeGeometryException = forall e. Exception e => SomeGeometryException e
    deriving Typeable

geometryExceptionToException :: Exception e => e -> SomeException
geometryExceptionToException = toException . SomeGeometryException

geometryExceptionFromException :: Exception e => SomeException -> Maybe e
geometryExceptionFromException x = do
    SomeGeometryException a <- fromException x
    cast a

instance Show SomeGeometryException where
    showsPrec :: Int -> SomeGeometryException -> ShowS
    showsPrec p (SomeGeometryException e) = showsPrec p e

instance Exception SomeGeometryException where
    displayException (SomeGeometryException e) = displayException e

newtype GeometryError = GeometryError {
    geoMessage :: String
    } deriving (Eq, Show, Typeable)

instance Exception GeometryError where
  toException = geometryExceptionToException
  fromException = geometryExceptionFromException

