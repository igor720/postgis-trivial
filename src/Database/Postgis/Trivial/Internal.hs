{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Database.Postgis.Trivial.Internal
--
-- Low level operations on the Postgis extention of the PostgreSQL Database
--
-----------------------------------------------------------------------------

{- Some techniques in this module were taken from
haskell-postgis <https://hackage.haskell.org/package/haskell-postgis>

Since that package comes with MIT license, we provide it here.

> Copyright (c) 2014 Peter
>
> Permission is hereby granted, free of charge, to any person obtaining
> a copy of this software and associated documentation files (the
> "Software"), to deal in the Software without restriction, including
> without limitation the rights to use, copy, modify, merge, publish,
> distribute, sublicense, and/or sell copies of the Software, and to
> permit persons to whom the Software is furnished to do so, subject to
> the following conditions:
>
> The above copyright notice and this permission notice shall be included
> in all copies or substantial portions of the Software.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
> EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
> MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
> IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
> CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
> TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
> SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}


module Database.Postgis.Trivial.Internal where

import GHC.Base hiding ( foldr )
import GHC.Num ( Num(..) )
import GHC.Show ( Show(show) )
import GHC.Real ( fromIntegral, Integral )
import System.Endian ( getSystemEndianness, Endianness(..) )
import Foreign ( Int64, Bits((.&.), (.|.)) )
import Control.Applicative ( (<$>) )
import Control.Monad (void )
import Control.Monad.Reader ( ReaderT(runReaderT), asks, MonadTrans(lift) )
import Control.Exception ( throw )
import Data.Foldable ( Foldable(..) )
import Data.ByteString.Lex.Integral ( packHexadecimal, readHexadecimal )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Binary ( Word8, Word32, Word64, Get, Put, byteSwap32, byteSwap64,
    Binary(get, put) )
import Data.Binary.Get ( getLazyByteString, lookAhead )
import Data.Binary.Put ( putLazyByteString )
import Data.Maybe ( isJust )
import Data.Binary.IEEE754 ( wordToDouble, doubleToWord )

import Database.Postgis.Trivial.PGISConst
import Database.Postgis.Trivial.Types


type HeaderGetter = Getter Header

newtype ByteOrder = ByteOrder Endianness deriving Show

-- | Header record
data Header = Header {
    _byteOrder  :: ByteOrder
  , _geoType    :: Word32
  , _srid       :: SRID
} deriving Show

instance Binary ByteOrder where
    get = fromBin <$> getLazyByteString 2
    put = putLazyByteString . toBin

instance Binary Header where
    get = getHeader
    put (Header bo gt s) =
        put bo >> (putInt . fromIntegral) gt  >> putMaybe s putInt

-- | Serializable class
class Serializable a where
    toBin :: a -> BL.ByteString
    fromBin :: BL.ByteString -> a

instance Serializable ByteOrder where
    toBin (ByteOrder BigEndian) = "00" :: BL.ByteString
    toBin (ByteOrder LittleEndian) = "01" :: BL.ByteString
    fromBin bo = case _fromBin bo :: Word8 of
        0 -> ByteOrder BigEndian
        1 -> ByteOrder LittleEndian
        _ -> throw $ GeometryError $ "Incorrect ByteOrder " ++ show bo

instance Serializable Word32 where
    toBin = _toBin 8
    fromBin = _fromBin

instance Serializable Word64 where
    toBin = _toBin 16
    fromBin = _fromBin

_toBin :: Integral a => Int -> a -> BL.ByteString
_toBin l w = case bsRes of
    Just s -> BL.fromChunks [pad l s]
    Nothing -> throw $ GeometryError "toBin: cannot convert word"
    where
        bsRes = packHexadecimal w
        pad l' bs' = BS.append (BC.replicate (l' - BS.length bs') '0') bs'

_fromBin :: Integral a => BL.ByteString -> a
_fromBin bs = case hexRes of
    Just (v, _) -> v
    Nothing -> throw $ GeometryError "fromBin: cannot parse hexadecimal"
    where
        hexRes = readHexadecimal $ BS.concat . BL.toChunks $ bs

{-# INLINE byteSwapFn #-}
byteSwapFn :: ByteOrder -> (a -> a) -> a -> a
byteSwapFn bo f = case bo of
    ByteOrder BigEndian     -> id
    ByteOrder LittleEndian  -> f

makeHeader :: SRID -> Word32 -> (Bool, Bool) -> Header
makeHeader srid geoType (hasM, hasZ) =
    Header (ByteOrder getSystemEndianness) gt srid where
        wOr acc (p, h) = if p then h .|. acc else acc
        gt = foldl wOr geoType
            [(hasM, wkbM), (hasZ, wkbZ), (isJust srid, wkbSRID)]

{-# INLINE lookupType #-}
lookupType :: Header -> Word32
lookupType h = _geoType h .&. ewkbTypeOffset

-- | Putters
putDouble :: Putter Double
putDouble = putLazyByteString . toBin .
    byteSwapFn (ByteOrder getSystemEndianness) byteSwap64 . doubleToWord

putInt :: Putter Int
putInt = putLazyByteString . toBin .
    byteSwapFn (ByteOrder getSystemEndianness) byteSwap32 . fromIntegral

{-# INLINE putMaybe #-}
putMaybe :: Maybe a -> Putter a -> Put
putMaybe mi = case mi of
    Just i -> ($ i)
    Nothing -> \_ -> return ()

{-# INLINE putChainLen #-}
putChainLen :: Putter Int
putChainLen = putInt

putHeader :: SRID -> Word32 -> (Bool, Bool) -> Put
putHeader s geoType (hasMBool, hasZBool) =
    put $ makeHeader s geoType (hasMBool, hasZBool)

putPoint :: Putter (Double, Double, Maybe Double, Maybe Double)
putPoint (x, y, Nothing, Nothing)   = putDouble x >> putDouble y
putPoint (x, y, Just z, Nothing)    = putDouble x >> putDouble y >> putDouble z
putPoint (x, y, Nothing, Just m)    = putDouble x >> putDouble y >> putDouble m
putPoint (x, y, Just z, Just m)     = putDouble x >> putDouble y >> putDouble z
                                        >> putDouble m

{-# INLINE putPointND #-}
putPointND :: PointND a => Putter a
putPointND a = putPoint $ components a

-- | BinGetters
getHeader :: Get Header
getHeader = do
    bo <- get
    t <- fromIntegral <$>  _getInt bo
    s <- if t .&. wkbSRID > 0
        then Just . fromIntegral <$> _getInt bo
        else return Nothing
    return $ Header bo t s

{-# INLINE skipHeader #-}
skipHeader :: HeaderGetter ()
skipHeader = void (lift getHeader)

{-# INLINE getHeaderPre #-}
getHeaderPre :: Get Header
getHeaderPre = lookAhead get

getPoint :: HeaderGetter (Double, Double, Maybe Double, Maybe Double)
getPoint = do
    gt <- asks _geoType
    let hasM = (gt .&. wkbM) > 0
        hasZ = (gt .&. wkbZ) > 0
    x <- getDouble
    y <- getDouble
    zMb <- if hasZ then Just <$> getDouble else return Nothing
    mMb <- if hasM then Just <$> getDouble else return Nothing
    return (x, y, zMb, mMb)

{-# INLINE getPointND #-}
getPointND :: PointND a => HeaderGetter a
getPointND = do
    (x, y, zMb, mMb) <- getPoint
    return $ fromComponents (x, y, zMb, mMb)

{-# INLINE getNumber #-}
getNumber :: (Serializable a) => (a -> a) -> Int64 -> ByteOrder -> Get a
getNumber f l bo  = do
    bs <- getLazyByteString l
    case bo of
        ByteOrder BigEndian     -> return $ fromBin bs
        ByteOrder LittleEndian  -> return . f . fromBin $ bs

_getInt :: ByteOrder -> Get Int
_getInt = fmap fromIntegral . getNumber byteSwap32 8

getInt :: HeaderGetter Int
getInt = asks _byteOrder >>= lift . _getInt

_getDouble :: ByteOrder -> Get Double
_getDouble = fmap wordToDouble . getNumber byteSwap64 16

getDouble :: HeaderGetter Double
getDouble = asks _byteOrder >>= lift . _getDouble

{-# INLINE getChainLen #-}
getChainLen :: HeaderGetter Int
getChainLen = getInt

-- | Geometry returning function
makeResult :: Header -> HeaderGetter a -> Get (a, SRID)
makeResult h getter = (,_srid h) <$> runReaderT getter h

