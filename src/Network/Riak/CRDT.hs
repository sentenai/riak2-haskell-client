-- | Module: Network.Riak.CRDT
-- 
-- CRDT operations
-- 
-- * Haskell values: 'Counter', 'Set' etc
-- 
-- * ADT for operations: 'CounterOp', 'SetOp' etc
-- 
-- * Pure modify* functions
-- 
-- * Functions asking current value and functions asking Riak to apply
-- operations
{-# LANGUAGE TypeFamilies, Rank2Types #-}

module Network.Riak.CRDT (module Network.Riak.CRDT.Types,
                          module Network.Riak.CRDT.Riak,
                          CRDT(..))
    where


import Data.Monoid
import qualified Data.Set as S
import qualified Data.Map as M
import Data.ByteString.Lazy (ByteString)
import Data.List.NonEmpty (NonEmpty(..))
import Network.Riak.CRDT.Riak
import Network.Riak.Types
import Network.Riak.CRDT.Types
import Network.Riak.CRDT.Ops


-- | Modify a counter by applying operations ops
modifyCounter :: CounterOp -> Counter -> Counter
modifyCounter ops (Counter c) = Counter (c+i)
    where CounterInc i = ops


-- | Modify a set by applying operations ops
modifySet :: SetOp -> Set -> Set
modifySet ops (Set c) = Set (c `S.union` adds S.\\ rems)
    where SetOpsComb adds rems = toOpsComb ops


modifyMap :: MapOp -> Map -> Map
--modifyMap (MapRemove path) m    = m
modifyMap (MapUpdate path op) m = modifyMap1 path op m

-- l1 = Map (M.fromList [])
-- l2 = Map (M.fromList [(MapField MapSetTag "X", MapSet (Set S.empty))])


modifyMap1 :: MapPath -> MapValueOp -> Map -> Map
modifyMap1 (MapPath (e :| [])) op m = modMap mf op m
    where mf = MapField (tagOf' op) e
modifyMap1 (MapPath (e :| (r:rs))) op (Map m')
    = Map $ M.update (Just . f) (MapField MapMapTag e) m'
      where f :: MapEntry -> MapEntry
            f (MapMap m) = MapMap . modifyMap1 (MapPath (r :| rs)) op $ m
            f z = z

modMap :: MapField -> MapValueOp -> Map -> Map
--modMap ix Nothing (Map m) = Map $ M.delete ix m
modMap ix op (Map m) = Map $ M.update (Just . modifyMapValue op) ix m

modifyMapValue :: MapValueOp -> MapEntry -> MapEntry
modifyMapValue (MapSetOp op) (MapSet s)           = MapSet      . modifySet op $ s
modifyMapValue (MapCounterOp op) (MapCounter c)   = MapCounter  . modifyCounter op $ c
modifyMapValue (MapMapOp op) (MapMap m)           = MapMap      . modifyMap op $ m
modifyMapValue (MapFlagOp op) (MapFlag f)         = MapFlag     . modifyFlag op $ f
modifyMapValue (MapRegisterOp op) (MapRegister m) = MapRegister . modifyRegister op $ m
modifyMapValue _ e = e

modifyFlag :: FlagOp -> Flag -> Flag
modifyFlag (FlagSet x) = const (Flag x)

modifyRegister :: RegisterOp -> Register -> Register
modifyRegister (RegisterSet x) = const (Register x)


-- class MapCRDT a where
--     type MapOperation a :: *
--     mapModify :: MapOperation a -> a -> a

-- instance MapCRDT Flag where
--     type MapOperation Flag = FlagOp
--     mapModify (FlagSet x) _ = Flag x

-- instance MapCRDT Set where
--     type MapOperation Set = SetOp
--     mapModify = modify

-- instance MapCRDT Counter where
--     type MapOperation Counter = CounterOp
--     mapModify = modify



class CRDT a where
    type Operation_ a :: *
    modify :: Operation_ a -> a -> a
    sendModify :: Connection
               -> BucketType -> Bucket -> Key
               -> a -> [Operation_ a] -> IO ()

instance CRDT Counter where
    type Operation_ Counter = CounterOp
    modify = modifyCounter
    sendModify c t b k _ o = counterUpdate c t b k o

instance CRDT Set where
    type Operation_ Set = SetOp
    modify = modifySet
    sendModify c t b k _ o = setUpdate c t b k o

instance CRDT Map where
    type Operation_ Map = MapOp
    modify = modifyMap
    sendModify c t b k _ o = mapUpdate c t b k o


