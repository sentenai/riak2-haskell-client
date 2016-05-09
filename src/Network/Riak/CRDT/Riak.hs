-- |   module:    Network.Riak.CRDT.Riak
--     copyright: (c) 2016 Sentenai
--     author:    Antonio Nikishaev <me@lelf.lu>
--     license:   Apache
-- 
-- CRDT operations
{-# LANGUAGE ScopedTypeVariables, MultiWayIf, OverloadedStrings #-}

module Network.Riak.CRDT.Riak (counterSendUpdate,
                               setSendUpdate,
                               mapSendUpdate,
                               get)
    where

import qualified Network.Riak.Connection as Conn
import Network.Riak.Types
import Network.Riak.Protocol.DtOp as PB
import Network.Riak.Protocol.CounterOp as PB
import qualified Network.Riak.CRDT.Types as CRDT
import qualified Network.Riak.Protocol.ErrorResponse as ER
import Data.Int
import Control.Applicative
import Control.Exception (catchJust)
import qualified Data.ByteString.Lazy as BS

import qualified Network.Riak.CRDT.Request as Req
import qualified Network.Riak.CRDT.Response as Resp


counterSendUpdate :: Connection -> BucketType -> Bucket -> Key
                  -> [CRDT.CounterOp] -> IO ()
counterSendUpdate conn t b k ops = Conn.exchange_ conn $ Req.counterUpdate ops t b k


setSendUpdate :: Connection -> BucketType -> Bucket -> Key
              -> [CRDT.SetOp] -> IO ()
setSendUpdate conn t b k ops = handleEmpty . Conn.exchange_ conn $ Req.setUpdate ops t b k


mapSendUpdate :: Connection -> BucketType -> Bucket -> Key
              -> [CRDT.MapOp] -> IO ()
mapSendUpdate conn t b k ops = handleEmpty . Conn.exchange_ conn $ Req.mapUpdate ops t b k


get :: Connection -> BucketType -> Bucket -> Key
    -> IO (Maybe CRDT.DataType)
get conn t b k = Resp.get <$> Conn.exchange conn (Req.get t b k)


-- | Ignore a ‘not_present’ error on update.
-- 
-- This is a bit hacky, but that's the behaviour we want.
--
-- TODO: Add custom exceptions to riak-haskell-client and just catch a
-- NotPresent exception here
handleEmpty :: IO () -> IO ()
handleEmpty act = catchJust
                  (\(e :: ER.ErrorResponse) ->
                       if | "{precondition,{not_present,"
                               `BS.isPrefixOf` ER.errmsg e -> Just ()
                          | otherwise                      -> Nothing
                  )
                  act
                  pure

