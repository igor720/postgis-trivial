{-# LANGUAGE CPP #-}

module Database.Postgis.Trivial.PGISConst where

import Foreign

#include <pgisConst.h>

wkbZ :: Word32
wkbZ    = #const WKBZOFFSET
wkbM :: Word32
wkbM    = #const WKBMOFFSET
wkbSRID :: Word32
wkbSRID = #const WKBSRIDFLAG
ewkbTypeOffset :: Word32
ewkbTypeOffset = 0x1fffffff

pgisPoint :: Word32
pgisPoint = #const POINTTYPE
pgisLinestring :: Word32
pgisLinestring = #const LINETYPE
pgisPolygon :: Word32
pgisPolygon = #const POLYGONTYPE
pgisMultiPoint :: Word32
pgisMultiPoint = #const MULTIPOINTTYPE
pgisMultiLinestring :: Word32
pgisMultiLinestring = #const MULTILINETYPE
pgisMultiPolygon :: Word32
pgisMultiPolygon = #const MULTIPOLYGONTYPE
