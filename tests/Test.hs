{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import qualified Data.Map                     as M
import qualified Data.Set                     as S
import           Data.List.NonEmpty           (NonEmpty(..))
import           Data.Text                    (Text)
import qualified Network.Riak                 as Riak
import qualified Network.Riak.Basic           as B
import qualified Network.Riak.CRDT            as C
import qualified Network.Riak.CRDT.Riak       as C
import qualified Network.Riak.Cluster         as Riak
import qualified Network.Riak.JSON            as J
import           Network.Riak.Resolvable      (ResolvableMonoid (..))
import           Network.Riak.Types
import qualified Properties
import qualified CRDTProperties               as CRDT
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [properties,
                           integrationalTests,
                           ping'o'death,
                           crdts
                          ]
properties :: TestTree
properties = testGroup "simple properties" Properties.tests

integrationalTests :: TestTree
integrationalTests = testGroup "integrational tests"
  [ testClusterSimple
#ifdef TEST2I
  , testIndexedPutGet
#endif
  ]

crdts :: TestTree
crdts = testGroup "CRDT" [
         testGroup "simple" [counter, set, map_],
         CRDT.tests
        ]


testClusterSimple :: TestTree
testClusterSimple = testCase "testClusterSimple" $ do
    rc <- Riak.connectToCluster [Riak.defaultClient]
    Riak.inCluster rc B.ping


testIndexedPutGet :: TestTree
testIndexedPutGet = testCase "testIndexedPutGet" $ do
    rc <- Riak.connectToCluster [Riak.defaultClient]
    let b = "riak-haskell-client-test"
        k = "test"
    keys <- Riak.inCluster rc $ \c -> do
      _ <- J.putIndexed c b k [(IndexInt "someindex" 135)] Nothing
          (RM (M.fromList [("somekey", "someval")] :: M.Map Text Text))
          Default Default
      Riak.getByIndex c b (IndexQueryExactInt "someindex" 135)
    assertEqual "" ["test"] keys

ping'o'death :: TestTree
ping'o'death = testCase "ping'o'death" $ replicateM_ 23 ping
    where ping = do
            c <- Riak.connect Riak.defaultClient
            replicateM_ 1024 $ Riak.ping c


counter :: TestTree
counter = testCase "increment" $ do
              conn <- Riak.connect Riak.defaultClient
              Just (C.DTCounter (C.Counter a)) <- act conn
              Just (C.DTCounter (C.Counter b)) <- act conn
              assertEqual "inc by 1" 1 (b-a)
    where
      act c = do C.counterUpdate c "counters" "xxx" "yyy" [C.CounterInc 1]
                 C.get c "counters" "xxx" "yyy"

set :: TestTree
set = testCase "set add" $ do
        conn <- Riak.connect Riak.defaultClient
        C.setUpdate conn btype buck key [C.SetRemove val]
        C.setUpdate conn btype buck key [C.SetAdd val]
        Just (C.DTSet (C.Set r)) <- C.get conn btype buck key
        assertBool "-foo +foo => contains foo" $ val `S.member` r
    where
      (btype,buck,key,val) = ("sets","xxx","yyy","foo")

map_ :: TestTree
map_ = testCase "map update" $ do
         conn <- Riak.connect Riak.defaultClient
         Just (C.DTMap a) <- act conn
         Just (C.DTMap b) <- act conn
         assertEqual "map update" (C.modify mapOp a) b
    where
      act c = do C.mapUpdate c btype buck key [mapOp]
                 C.get c btype buck key 

      btype = "maps"
      (buck,key) = ("xxx","yyy")

      mapOp = C.MapUpdate (C.MapPath ("X" :| "Y" : "Z" : []))
                          (C.MapCounterOp (C.CounterInc 1))


