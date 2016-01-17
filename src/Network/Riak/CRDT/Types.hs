-- | module: Network.Riak.CRDT.Types
-- 
-- Haskell-side view of CRDT
-- 
{-# LANGUAGE TypeFamilies #-}

module Network.Riak.CRDT.Types where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)
import Data.List.NonEmpty
import Data.Monoid

-- data Operation = MapOperation MapOp
--                | SetOperation SetOp
--                | CounterOperation CounterOp
--                  deriving (Show)


-- | CRDT Map is indexed by MapField, which is a name tagged by a type
-- (there may be different entries with the same name, but different
-- types)
data MapField = MapField MapEntryTag ByteString deriving (Eq,Ord,Show)

-- | CRDT Map is a Data.Map indexed by 'MapField' and holding
-- 'MapEntry'
newtype Map = Map (M.Map MapField MapEntry) deriving Show

type MapContent = M.Map MapField MapEntry

data MapEntryTag = MapCounterTag
                 | MapSetTag
                 | MapRegisterTag
                 | MapFlagTag
                 | MapMapTag
                   deriving (Eq,Ord,Show)

-- | CRDT Map holds values of type 'MapEntry'
data MapEntry = MapCounter Counter
              | MapSet Set
              | MapRegister Register
              | MapFlag Flag
              | MapMap Map
                deriving Show

-- data ME = MESet Set | MECounter Counter

-- type family S (t :: MapTag) :: * where
--     S 'MapCounter_ = Set

-- data family X (t::ME) :: *
-- data instance X (MESet _) = Q

tagOf :: MapEntry -> MapEntryTag
tagOf MapCounter{}  = MapCounterTag
tagOf MapSet{}      = MapSetTag
tagOf MapRegister{} = MapRegisterTag
tagOf MapFlag{}     = MapFlagTag
tagOf MapMap{}      = MapMapTag

tagOf' :: MapValueOp -> MapEntryTag
tagOf' MapCounterOp{}  = MapCounterTag
tagOf' MapSetOp{}      = MapSetTag
tagOf' MapRegisterOp{} = MapRegisterTag
tagOf' MapFlagOp{}     = MapFlagTag
tagOf' MapMapOp{}      = MapMapTag


-- data T :: MapTag -> * where
--     MapCounterT :: T 'MapCounter_

-- deriving instance Show (T a)

-- data MapEntry :: * where
--     MapEntry :: (Show (S a), Show (T a)) => T a -> S a -> MapEntry

-- deriving instance Show MapEntry

-- | Selector (“xpath”) inside 'Map'
newtype MapPath = MapPath (NonEmpty MapField) deriving Show


-- instance CRDT Counter where
--     type Operation_ Counter = CounterOp
--     modify = _

-- class IsMapEntry a
-- instance IsMapEntry Set
-- instance IsMapEntry (Map t)

-- | map operations
data MapOp = --MapRemove MapField           -- ^ remove value in map
             MapUpdate MapPath MapValueOp -- ^ update value on path by operation
             deriving Show

-- | registers can be set
data RegisterOp = RegisterSet ByteString deriving Show

-- | flags can be enabled/disabled
data FlagOp = FlagSet Bool deriving Show

newtype Flag = Flag Bool deriving Show
newtype Register = Register ByteString deriving Show

-- | operations on map values
data MapValueOp = MapCounterOp CounterOp
                | MapSetOp SetOp
                | MapRegisterOp RegisterOp
                | MapFlagOp FlagOp
                | MapMapOp MapOp
                  deriving Show


-- | CRDT ADT. 'Network.Riak.CRDT.Riak.get' operations return value of this type
data DataType = DTCounter Counter
              | DTSet Set
              | DTMap Map
                deriving (Show)

-- | CRDT Set is a Data.Set
newtype Set = Set (S.Set ByteString) deriving (Eq,Show)
data SetOp = SetAdd ByteString | SetRemove ByteString deriving Show

setFromSeq :: Seq.Seq ByteString -> Set
setFromSeq = Set . S.fromList . F.toList

-- | CRDT Counter hold a integer 'Count'
newtype Counter = Counter Count deriving (Eq,Show)
type Count = Int64
data CounterOp = Inc Count deriving (Show)

instance Monoid CounterOp where
    mempty = Inc 0
    Inc x `mappend` Inc y = Inc (x+y)

