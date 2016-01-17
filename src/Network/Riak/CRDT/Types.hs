-- | module: Network.Riak.CRDT.Types
-- 
-- Haskell-side view of CRDT
-- 


module Network.Riak.CRDT.Types where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.Foldable
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)
import Data.Monoid

type MapKey = ByteString

-- | CRDT Map is a Data.Map holding 'MapEntry'
newtype Map = Map (M.Map MapKey MapEntry) deriving (Eq,Show)

-- | Selector (“xpath”) inside 'Map'
newtype MapPath = MapPath [ByteString]

data MapEntry = MSet Set
              | MCounter Int
              | MMap Map
                deriving (Eq,Show)
data MapOp = MapOp

-- | CRDT ADT. 'Network.Riak.CRDT.Riak.get' operations return value of this type
data DataType = DTCounter Counter
              | DTSet Set
              | DTMap Map
              | DTFlag Bool
                deriving (Eq,Show)

-- | CRDT Set is a Data.Set
newtype Set = Set (S.Set ByteString) deriving (Eq,Show)
data SetOp = SetAdd ByteString | SetRemove ByteString deriving Show

setFromSeq :: Seq.Seq ByteString -> Set
setFromSeq = Set . S.fromList . toList

-- | CRDT Counter hold a integer 'Count'
newtype Counter = Counter Count deriving (Eq,Show)
type Count = Int64
data CounterOp = Inc Count deriving (Show)

instance Monoid CounterOp where
    mempty = Inc 0
    Inc x `mappend` Inc y = Inc (x+y)

