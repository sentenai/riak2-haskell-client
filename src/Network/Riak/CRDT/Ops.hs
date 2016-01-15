module Network.Riak.CRDT.Ops (counterIncOp) where

import qualified Network.Riak.Protocol.DtOp as PB
import qualified Network.Riak.Protocol.CounterOp as PB
import Network.Riak.CRDT.Types
import Data.Monoid



counterIncOp :: [CounterOp] -> PB.DtOp
counterIncOp ops = PB.DtOp { PB.counter_op = Just $ PB.CounterOp (Just i),
                             PB.set_op = Nothing,
                             PB.map_op = Nothing
                           }
    where Inc i = mconcat ops

