module Network.Riak.CRDT.Request
    (get, counterInc) where

import qualified Network.Riak.Protocol.DtFetchRequest as DtFetch
import qualified Network.Riak.Protocol.DtUpdateRequest as DtUpdate
import qualified Network.Riak.Protocol.DtOp as DtOp
import Network.Riak.CRDT.Ops
import qualified Network.Riak.CRDT.Types as CRDT
import Network.Riak.Types

counterInc :: [CRDT.CounterOp]
           -> BucketType -> Bucket -> Key
           -> DtUpdate.DtUpdateRequest
counterInc ops = update (counterIncOp ops)

update :: DtOp.DtOp -> BucketType -> Bucket -> Key -> DtUpdate.DtUpdateRequest
update op t b k = DtUpdate.DtUpdateRequest {
                       DtUpdate.bucket = b,
                       DtUpdate.key = Just k,
                       DtUpdate.type' = t,
                       DtUpdate.context = Nothing,
                       DtUpdate.op = op,
                       DtUpdate.w = Nothing,
                       DtUpdate.dw = Nothing,
                       DtUpdate.pw = Nothing,
                       DtUpdate.return_body = Nothing,
                       DtUpdate.timeout = Nothing,
                       DtUpdate.sloppy_quorum = Nothing,
                       DtUpdate.n_val = Nothing,
                       DtUpdate.include_context = Nothing
                     }

get t b k = DtFetch.DtFetchRequest {
                 DtFetch.bucket = b,
                 DtFetch.key = k,
                 DtFetch.type' = t,
                 DtFetch.r = Nothing,
                 DtFetch.pr = Nothing,
                 DtFetch.basic_quorum = Nothing,
                 DtFetch.notfound_ok = Nothing,
                 DtFetch.timeout = Nothing,
                 DtFetch.sloppy_quorum = Nothing,
                 DtFetch.n_val = Nothing,
                 DtFetch.include_context = Nothing
               }
