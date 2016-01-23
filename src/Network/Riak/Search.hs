-- | Module: Network.Riak.Search
-- 
-- Solr search
-- 
-- http://docs.basho.com/riak/2.1.3/dev/using/search/

module Network.Riak.Search where

import Network.Riak.Connection.Internal
import Network.Riak.Types.Internal
import qualified Network.Riak.Request as Req
import qualified Network.Riak.Response as Resp
import Control.Applicative

searchRaw :: Connection -> SearchQuery -> Index -> IO [SearchResult]
searchRaw conn q ix = Resp.search <$> exchange conn (Req.search q ix)

