

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification                            #-}
{-# LANGUAGE InstanceSigs #-}

module Database.Postgis.Trivial.Types
    ( SRID
    , PointND (..)
    , Putter
    , Getter
    , Geo (..)
    , Geometry (..)
    , GeometryError (..)
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

-- | Binary putter
type Putter a = a -> Put

writeEWKB :: Putter a -> a -> BS.ByteString
writeEWKB putter = BL.toStrict . runPut . putter

-- | Binary getter
type Getter h = ReaderT h Get

readEWKB :: Get a -> BS.ByteString -> a
readEWKB getter bs = runGet getter (BL.fromStrict bs)

-- | Type class which defines geometry to/from binary form convertions
class Typeable a => Geometry a where
    putGeometry :: Putter a
    getGeometry :: Get a

-- | Wrapper for geomety types (prevents collisions with default instances)
newtype Geo g = Geo g

instance Geometry g => ToField (Geo g) where
    toField (Geo g) = Escape . writeEWKB putGeometry $ g

instance Geometry g => FromField (Geo g) where
    fromField f m = do
        typ <- typename f
        if typ /= "geometry"
            then returnError Incompatible f (show typ)
            else case m of
                Nothing  -> returnError UnexpectedNull f ""
                Just bs -> return $ Geo (readEWKB getGeometry bs)

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

-- | Exception for operations with geometries
newtype GeometryError = GeometryError {
    geoMessage :: String
    } deriving (Eq, Show, Typeable)

instance Exception GeometryError where
  toException = geometryExceptionToException
  fromException = geometryExceptionFromException


