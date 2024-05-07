{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Postgis.Trivial.Cast where

import GHC.Base hiding ( foldr )
import GHC.Num ( Num((+)) )
import Control.Monad ( replicateM, mapM_ )
import Control.Exception ( throw )
import Data.Functor ( (<$>), (<&>) )
import Data.Typeable    ( Typeable )
import Data.Foldable    ( Foldable(..) )
import Data.Traversable ( Traversable(..) )
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.List (zip)

import Database.Postgis.Trivial.Types
import Database.Postgis.Trivial.Internal


type family Cast p

class (Typeable p, PointND (Cast p)) => Castable p where
    toPointND :: p -> Cast p
    toPointND _ = throw $
        GeometryError "no convertion from user data type to PointND"
    fromPointND :: Cast p -> p
    fromPointND _ = throw $
        GeometryError "no converttion from PointND to user data type"

-- | Translator of Traversables
class (Castable p, Traversable t, Typeable t) => Trans t p where
    transTo :: t p -> t (Cast p)
    transTo vs = toPointND <$> vs
    transFrom :: t (Cast p) -> t p
    transFrom vs = fromPointND <$> vs

instance Castable p => Trans [] p where
instance Castable p => Trans V.Vector p where
instance (Castable p, Typeable k) => Trans (M.Map k) p where
instance Castable p => Trans IM.IntMap p where

-- | Replication procedure for various Traversables
class (Traversable t, Typeable t) => Repl t b where
    repl :: Int -> HeaderGetter b -> HeaderGetter (t b)

instance Repl [] b where
    repl = replicateM
instance Repl V.Vector b where
    repl = V.replicateM

-- | Point chain is a base structural component of various geometries
class Traversable t => GeoChain t where
    count :: t p -> Int
    putChain :: PointND a => Putter (t a)
    putChain vs = do
        putChainLen $ count vs
        mapM_ putPointND vs
    getChain :: (Traversable t, PointND a) => HeaderGetter (t a)

instance GeoChain V.Vector where
    count = V.length
    getChain = getChainLen >>= (`V.replicateM` getPointND)
instance GeoChain [] where
    count = foldr (\_ r->r+1) (0::Int)
    getChain = getChainLen >>= (`replicateM` getPointND)
instance GeoChain (M.Map Int) where
    count = M.size
    getChain = (getChainLen >>= (`replicateM` getPointND)) <&>
        (M.fromList . zip ([0..]::[Int]))
instance GeoChain IM.IntMap where
    count = IM.size
    getChain = (getChainLen >>= (`replicateM` getPointND)) <&>
        (IM.fromList . zip ([0..]::[Int]))

