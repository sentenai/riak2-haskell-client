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
{-# LANGUAGE TypeFamilies, OverloadedStrings, ScopedTypeVariables, PatternGuards #-}

module Network.Riak.CRDT (module Network.Riak.CRDT.Types,
                          module Network.Riak.CRDT.Riak,
                          CRDT(..))
    where


import qualified Data.Set as S
import qualified Data.Map as M
import Data.ByteString.Lazy (ByteString)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup
import Data.Proxy
import Data.Default.Class
import Network.Riak.CRDT.Riak
import Network.Riak.Types
import Network.Riak.CRDT.Types
import Network.Riak.CRDT.Ops


-- | Modify a counter by applying operations ops
modifyCounter :: CounterOp -> Counter -> Counter
modifyCounter op c = c <> Counter i
    where CounterInc i = op



-- | Modify a set by applying operations ops
modifySet :: SetOp -> Set -> Set
modifySet op (Set c) = Set (c `S.union` adds S.\\ rems)
    where SetOpsComb adds rems = toOpsComb op


modifyMap :: MapOp -> Map -> Map
--modifyMap (MapRemove path) m    = m
modifyMap (MapUpdate path op) m = modifyMap1 path op m



modifyMap1 :: MapPath -> MapValueOp -> Map -> Map
modifyMap1 (MapPath (e :| [])) op m = modMap mf op m
    where mf = MapField (tagOf' op) e
modifyMap1 (MapPath (e :| (r:rs))) op (Map m')
    = Map $ M.alter (Just . f) (MapField MapMapTag e) m'
      where f :: Maybe MapEntry -> MapEntry
            f Nothing = f (Just $ MapMap def)
            f (Just (MapMap m)) = MapMap . modifyMap1 (MapPath (r :| rs)) op $ m
            f (Just z) = z

modMap :: MapField -> MapValueOp -> Map -> Map
--modMap ix Nothing (Map m) = Map $ M.delete ix m
modMap ix op (Map m) = Map $ M.alter alterBy ix m
    where
      alterBy v = Just . modifyMapValue op $ v


modifyMapValue :: MapValueOp -> Maybe MapEntry -> MapEntry
modifyMapValue (MapSetOp op)      = modifyEntry (Proxy :: Proxy Set) op
modifyMapValue (MapCounterOp op)  = modifyEntry (Proxy :: Proxy Counter) op
modifyMapValue (MapMapOp op)      = modifyEntry (Proxy :: Proxy Map) op
modifyMapValue (MapFlagOp op)     = modifyEntry (Proxy :: Proxy Flag) op
modifyMapValue (MapRegisterOp op) = modifyEntry (Proxy :: Proxy Register) op


modifyFlag :: FlagOp -> Flag -> Flag
modifyFlag (FlagSet x) = const (Flag x)

modifyRegister :: RegisterOp -> Register -> Register
modifyRegister (RegisterSet x) = const (Register x)


-- | Types that can be held inside 'Map'
class Default a => MapCRDT a where
    type MapOperation_ a :: *
    mapModify :: MapOperation_ a -> a -> a

    -- | modify a maybe-absent 'MapEntry'
    modifyEntry :: Proxy a -> MapOperation_ a -> Maybe MapEntry -> MapEntry
    modifyEntry _ op Nothing = toEntry . mapModify op $ (def :: a)
    modifyEntry _ op (Just e) | Just v <- fromEntry e = toEntry . mapModify op $ (v :: a)
                              | otherwise             = e
    toEntry :: a -> MapEntry
    fromEntry :: MapEntry -> Maybe a


instance MapCRDT Flag where
    type MapOperation_ Flag = FlagOp
    mapModify = modifyFlag
    fromEntry (MapFlag f) = Just f
    fromEntry _ = Nothing
    toEntry = MapFlag

instance MapCRDT Set where
    type MapOperation_ Set = SetOp
    mapModify = modify
    fromEntry (MapSet s) = Just s
    fromEntry _ = Nothing
    toEntry = MapSet

instance MapCRDT Counter where
    type MapOperation_ Counter = CounterOp
    mapModify = modify
    fromEntry (MapCounter s) = Just s
    fromEntry _ = Nothing
    toEntry = MapCounter

instance MapCRDT Register where
    type MapOperation_ Register = RegisterOp
    mapModify = modifyRegister
    fromEntry (MapRegister s) = Just s
    fromEntry _ = Nothing
    toEntry = MapRegister


instance MapCRDT Map where
    type MapOperation_ Map = MapOp
    mapModify = modify
    fromEntry (MapMap s) = Just s
    fromEntry _ = Nothing
    toEntry = MapMap


-- | CRDT types
class MapCRDT a => CRDT a where
    type Operation_ a :: *

    -- | Modify a value by applying an operation
    modify :: Operation_ a -> a -> a

    -- | Request riak a modification
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


