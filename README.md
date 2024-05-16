# postgis-trivial

Haskell Postgresql/PostGIS DB Driver

This library provides methods which allow direct use of user-defined Haskell data with Postgresql/PostGIS databases. It is based on [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple) and can be used with other postgresql-simple capabilities.

The main interface module `Database.PostGIS.Trivial` allows PostGIS to work with various data enclosed in `Traversable` data structures. If the most inner data structures are `Unboxed` vectors, then use the functions and types defined in `Database.PostGIS.Trivial.Unboxed`. And, if the most inner data structures are `Storable` vectors then use `Database.PostGIS.Trivial.Storable` module.

## Synopsis

### Default types

- P2D for 2D points

- P3DZ, P3DM for 3D points

- P4D for points with z and m coordinates

You can freely use them for your data. Unboxed and Storable types are in the corresponding modules (thay have appending 'U' and 'S' letters in names).

### User-defined types

Ensure that user geometry data points is correctly translated into the internal default points data as in the example.

```haskell
{-# LANGUAGE TypeFamilies #-}

data LatLon =       -- example of 2D geospatial point data
        LatLon !Double !Double
        deriving (Show, Eq)

type instance Cast LatLon = P2D     -- translation to inner 2D point

instance Castable LatLon where      -- specify translation
    toPointND (LatLon y x) = Point2D x y
    fromPointND (Point2D x y) = LatLon y x
```

Then a structure of type `Traversable t => t LatLon` can be interpreted as `LineString` or `MultiPoint`. Any structure of type
`(Traversable t1, Traversable t2) => t2 (t1 LatLon)` can be interpreted as `Polygon` or `MultiLineString`. And any structure of type
`(Traversable t1, Traversable t2, Traversable t3) => t3 (t2 (t1 LatLon))` can be interpreted as `MultiPolygon`. Currently, only following `Traversable`s are supported: `List`, `Data.Vector.Vector`. `Data.IntMap.IntMap` and `Data.Map.Map` have partial support.

By this way you can use these structures in postgresql-simple functions as such:

```haskell
    ls = [LatLon 1 2, LatLon 1.5 2.5, LatLon 2.5 3, LatLon 1 2]
    _ <- execute conn "INSERT INTO linestrings (geom) VALUES (?)"
        (Only (Geo (LineString srid ls)))
    [Only res] <- query_ conn "SELECT * FROM linestrings LIMIT 1"
    let Geo (LineString srid' ls') = res
```

Or the same with suitable helper functions:

```haskell
    _ <- execute conn "INSERT INTO linestrings (geom) VALUES (?)"
        (Only (putLS srid ls))
    [Only res] <- query_ conn "SELECT * FROM linestrings LIMIT 1"
    let (srid', ls') = getLS res
```



