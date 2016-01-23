-- | module: Network.Riak.CRDT.Types
-- 
-- Haskell-side view of CRDT
-- 
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, DeriveGeneric #-}


module Network.Riak.CRDT.Types (
        -- * Types
        DataType(..),
        -- ** Maps
        Map(..),
        MapField(..),
        MapEntry(..), MapEntryTag(..), tagOf',
        MapPath(..), MapContent,
        -- *** Modification
        MapOp(..), MapValueOp(..),
        -- ** Counters
        Counter(..), Count,
        -- *** Modification
        CounterOp(..),
        -- ** Sets
        Set(..), setFromSeq,
        -- *** Modification
        SetOp(..),
        -- ** Registers
        Register(..),
        -- *** Modification
        RegisterOp(..),
        -- ** Flags
        Flag(..),
        -- *** Modification
        FlagOp(..),
        -- * Re-exports
        NonEmpty(..))
    where


import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)
import Data.List.NonEmpty
import Data.Semigroup
import Data.Default.Class
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- data Operation = MapOperation MapOp
--                | SetOperation SetOp
--                | CounterOperation CounterOp
--                  deriving (Show)


-- | CRDT Map is indexed by MapField, which is a name tagged by a type
-- (there may be different entries with the same name, but different
-- types)
data MapField = MapField MapEntryTag ByteString deriving (Eq,Ord,Show,Generic)

instance NFData MapField

-- | CRDT Map is a Data.Map indexed by 'MapField' and holding
-- 'MapEntry'. Maps are specials in a way that they can additionally
-- hold 'Flag's, 'Register's, and most importantly, other 'Map's.
newtype Map = Map MapContent deriving (Eq,Show,Generic)

instance NFData Map

type MapContent = M.Map MapField MapEntry

instance Default Map where
    def = Map M.empty

data MapEntryTag = MapCounterTag
                 | MapSetTag
                 | MapRegisterTag
                 | MapFlagTag
                 | MapMapTag
                   deriving (Eq,Ord,Show,Generic)

instance NFData MapEntryTag

-- | CRDT Map holds values of type 'MapEntry'
data MapEntry = MapCounter !Counter
              | MapSet !Set
              | MapRegister !Register
              | MapFlag !Flag
              | MapMap !Map
                deriving (Eq,Show,Generic)

instance NFData MapEntry


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
newtype MapPath = MapPath (NonEmpty ByteString) deriving Show

-- instance CRDT Counter where
--     type Operation_ Counter = CounterOp
--     modify = _

-- class IsMapEntry a
-- instance IsMapEntry Set
-- instance IsMapEntry (Map t)

-- | map operations
data MapOp = --MapRemove MapField           -- ^ remove value in map
             MapUpdate MapPath MapValueOp   -- ^ update value on path by operation
             deriving Show

-- | registers can be set
data RegisterOp = RegisterSet !ByteString deriving Show

-- | flags can be enabled/disabled
data FlagOp = FlagSet !Bool deriving Show

-- | Flag holds a 'Bool'
newtype Flag = Flag Bool deriving (Eq,Ord,Show,Generic)

instance NFData Flag

-- | Last-wins monoid for 'Flag'
instance Monoid Flag where
    mempty = Flag False
    mappend = (<>)

-- | Last-wins semigroup for 'Flag'
instance Semigroup Flag where
    a <> b = getLast (Last a <> Last b)

instance Default Flag where
    def = mempty

-- | Register holds a 'ByteString'
newtype Register = Register ByteString deriving (Eq,Show,Generic)

instance NFData Register

-- | Last-wins monoid for 'Register'
instance Monoid Register where
    mempty = Register ""
    mappend = (<>)

instance Semigroup Register where
    a <> b = getLast (Last a <> Last b)

instance Default Register where
    def = mempty


-- data MapValueOp a where
--     MapCounterOp :: CounterOp -> MapValueOp Counter
--     MapSetOp :: SetOp -> MapValueOp Set
--     MapRegisterOp :: RegisterOp -> MapValueOp Register
--     MapFlagOp :: FlagOp -> MapValueOp Flag
--     MapMapOp :: MapOp -> MapValueOp Map

-- | operations on map values
data MapValueOp = MapCounterOp !CounterOp
                | MapSetOp !SetOp
                | MapRegisterOp !RegisterOp
                | MapFlagOp !FlagOp
                | MapMapOp !MapOp
                  deriving Show


-- | CRDT ADT.
-- 
-- 'Network.Riak.CRDT.Riak.get' operations return value of this type
data DataType = DTCounter Counter
              | DTSet Set
              | DTMap Map
                deriving (Eq,Show,Generic)

instance NFData DataType

-- | CRDT Set is a Data.Set
newtype Set = Set (S.Set ByteString) deriving (Eq,Ord,Show,Generic,Monoid)

instance NFData Set

instance Semigroup Set where
    Set a <> Set b = Set (a <> b)

instance Default Set where
    def = Set mempty

-- | CRDT Set operations
data SetOp = SetAdd ByteString    -- ^ add element to the set
           | SetRemove ByteString -- ^ remove element from the set
             deriving Show

setFromSeq :: Seq.Seq ByteString -> Set
setFromSeq = Set . S.fromList . F.toList

-- | CRDT Counter hold a integer 'Count'
newtype Counter = Counter Count deriving (Eq,Ord,Num,Show,Generic)
type Count = Int64

instance NFData Counter

instance Semigroup Counter where
    Counter a <> Counter b = Counter . getSum $ Sum a <> Sum b

instance Monoid Counter where
    mempty = Counter 0
    mappend = (<>)

instance Default Counter where
    def = mempty

data CounterOp = CounterInc !Count deriving (Show)

instance Monoid CounterOp where
    mempty = CounterInc 0
    CounterInc x `mappend` CounterInc y = CounterInc . getSum $ Sum x <> Sum y

