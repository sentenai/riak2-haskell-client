module Network.Riak.CRDT.Riak where

import qualified Network.Riak.Connection as Conn
import Network.Riak.Types
import Network.Riak.Protocol.DtOp as PB
import Network.Riak.Protocol.CounterOp as PB
import qualified Network.Riak.CRDT.Types as CRDT
import Data.Int
import Control.Applicative

import qualified Network.Riak.CRDT.Request as Req
import qualified Network.Riak.CRDT.Response as Resp


counterUpdate :: Connection -> BucketType -> Bucket -> Key
           -> [CRDT.CounterOp] -> IO ()
counterUpdate conn t b k ops = Conn.exchange_ conn (Req.counterUpdate ops t b k)



get :: Connection -> BucketType -> Bucket -> Key
    -> IO (Maybe CRDT.DataType)
get conn t b k = Resp.get <$> Conn.exchange conn (Req.get t b k)
