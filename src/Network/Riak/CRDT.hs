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

module Network.Riak.CRDT (module Network.Riak.CRDT.Types,
                          module Network.Riak.CRDT.Riak,
                          modifyCounter,
                          modifySet)
    where


import Data.Monoid
import qualified Data.Set as S
import Network.Riak.CRDT.Riak
import Network.Riak.CRDT.Types
import Network.Riak.CRDT.Ops


-- | Modify a counter by applying operations ops
modifyCounter :: [CounterOp] -> Counter -> Counter
modifyCounter ops (Counter c) = Counter (c+i)
    where Inc i = mconcat ops


-- | Modify a set by applying operations ops
modifySet :: [SetOp] -> Set -> Set
modifySet ops (Set c) = Set (c `S.union` adds S.\\ rems)
    where SetOpsComb adds rems = mconcat . map toOpsComb $ ops

