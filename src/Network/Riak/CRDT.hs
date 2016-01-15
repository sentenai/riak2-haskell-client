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
                          module Network.Riak.CRDT.Riak) where

import Network.Riak.CRDT.Riak
import Network.Riak.CRDT.Types

