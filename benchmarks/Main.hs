{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main

import Network.Riak.Connection.Pool as Pool (Pool, create, withConnection)
import Network.Riak.Connection (defaultClient)
import Network.Riak.CRDT
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Applicative
import Control.Monad
import Data.Maybe


main :: IO ()
main = benchmarks >>= defaultMain

bucket = "bench"

benchmarks = do
  pool <- Pool.create defaultClient 1 1 1
  already <- Pool.withConnection pool $ \c -> get c "counters" bucket "__setup"
  when (isNothing already) $ setup pool
  pure [crdt pool]

setup :: Pool -> IO ()
setup pool = Pool.withConnection pool $ \c -> do
               setupSetN c "10" 10
               setupSetN c "100" 100
               setupSetN c "1000" 1000
               sendModify c "counters" bucket "__setup" (mempty::Counter) [CounterInc 1]
               putStrLn "Bench env set up."


setupSetN c key n = sequence_ [
                     sendModify c "sets" bucket key (mempty::Set) [SetAdd (B.pack $ show i)]
                     | i <- [1..n]
                    ]



crdt :: Pool -> Benchmark
crdt pool = bgroup "CRDT" [
             bench "get a non-existent counter" . nfIO $ pooled getEmpty,
             bench "get a counter"              . nfIO $ pooled getCounter,
             bench "get a 10-elem set"          . nfIO $ pooled getSet10,
             bench "get a 100-elem set"         . nfIO $ pooled getSet100,
             bench "get a 1000-elem set"        . nfIO $ pooled getSet1000
           ]
    where pooled = Pool.withConnection pool


getEmpty c   = get c "counters" "not here" "never was"
getCounter c = get c "counters" "xxx" "yyy"
getSet10 c   = get c "sets" "bench" "10"
getSet100 c  = get c "sets" "bench" "100"
getSet1000 c = get c "sets" "bench" "1000"


