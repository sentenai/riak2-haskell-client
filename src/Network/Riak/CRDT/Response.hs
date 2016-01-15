module Network.Riak.CRDT.Response (get) where

import Control.Applicative ((<$>))
import Network.Riak.Protocol.DtFetchResponse (DtFetchResponse,value,type')
import Network.Riak.Protocol.DtFetchResponse.DataType (DataType(..))
import Network.Riak.Protocol.DtValue (counter_value)
import Network.Riak.CRDT.Types as CRDT

get :: DtFetchResponse -> Maybe CRDT.DataType
get resp = case type' resp of
             COUNTER ->
                 DTCounter . Counter <$> (counter_value =<< value resp)
             _ ->
                 Nothing

