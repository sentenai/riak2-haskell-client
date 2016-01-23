{-# LANGUAGE OverloadedStrings, FlexibleContexts, TupleSections, ScopedTypeVariables,
  GADTs, StandaloneDeriving, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module CRDTProperties (prop_counters,
                       prop_sets,
                       prop_maps,
                       tests) where

-- |
-- The idea: send arbitrary stream of commands to riak, collect each
-- command output to list :: [Maybe RiakReturnValue].  Then see it we
-- get the same list of results simulating Riak in this module.

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.ByteString.Lazy (ByteString)
import Control.Applicative
import Data.Maybe
import Data.List.NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.RWS
import Control.Exception (bracket)
import Data.Proxy

import Test.Tasty
import Test.Tasty.QuickCheck
import Common

import qualified Network.Riak.Basic as B
import qualified Network.Riak.Value as V
import qualified Network.Riak.CRDT  as C

newtype BucketType = BucketType ByteString deriving (Show,Eq,Ord)
newtype Bucket     = Bucket ByteString deriving (Show,Eq,Ord)
newtype Key        = Key ByteString deriving (Show,Eq,Ord)
newtype Value      = Value ByteString deriving Show

class Values a where values :: [a]

-- Not many. We want to hit each multiple times
instance Values BucketType where values = BucketType <$> ["sets","counters","maps"]
instance Values Bucket     where values = Bucket <$> ["A","B"]
instance Values Key        where values = Key <$> ["a","b","c"]
instance Values Value      where values = Value <$> ["1","2","3","4"]

instance Arbitrary BucketType where arbitrary = elements values
instance Arbitrary Bucket     where arbitrary = elements values
instance Arbitrary Key        where arbitrary = elements values
instance Arbitrary Value      where arbitrary = elements values

data Point = Point BucketType Bucket Key deriving (Show,Ord,Eq)
instance Values Point where
    values = [ Point t b k | t <- values, b <- values, k <- values ]

instance Arbitrary Point where arbitrary = elements values

type RiakState = Map.Map Point C.DataType


-- | observe all current values we care about (instance 'Values') in
--   riak, gather them into a map
observeRiak :: IO RiakState
observeRiak = Map.fromList . catMaybes <$> observeRiak'

observeRiak' :: IO [Maybe (Point, C.DataType)]
observeRiak' = withSomeConnection $ \c ->
               sequence [ do r <- C.get c t b k
                             pure . fmap (p,) $ r
                          | p@(Point (BucketType t) (Bucket b) (Key k)) <- values ]


-- | For each CRDT a => a,
data Op a = CGet Bucket Key                     -- ^ we can get a value
          | CUpdate Bucket Key (C.Operation_ a) -- ^ we can update a value

deriving instance (Show (C.Operation_ a), C.CRDT a) => Show (Op a)

class C.CRDT t => Action t where
    bucketType :: Proxy t -> ByteString
    emptyVal :: t
    fromDT :: C.DataType -> t
    toDT :: t -> C.DataType

instance Action C.Counter where
    bucketType _ = "counters"
    emptyVal = C.Counter 0
    fromDT (C.DTCounter c) = c
    fromDT _               = error "expected counter" -- ok for tests
    toDT = C.DTCounter

instance Action C.Set where
    bucketType _ = "sets"
    emptyVal = C.Set Set.empty
    fromDT (C.DTSet c) = c
    fromDT _           = error "expected set"
    toDT = C.DTSet

instance Action C.Map where
    bucketType _ = "maps"
    emptyVal = C.Map Map.empty
    fromDT (C.DTMap c) = c
    fromDT _           = error "expected map"
    toDT = C.DTMap

instance (Arbitrary a, Arbitrary (C.Operation_ a)) => Arbitrary (Op a) where
    arbitrary = oneof [
                 CGet <$> arbitrary <*> arbitrary,
                 CUpdate <$> arbitrary <*> arbitrary <*> arbitrary
                ]

instance Arbitrary C.Counter where
    arbitrary = C.Counter <$> arbitrary

instance Arbitrary C.CounterOp where
    arbitrary = C.CounterInc <$> choose (-16,16)

instance Arbitrary C.SetOp where
    arbitrary = oneof [
                 C.SetAdd <$> arbitrary, C.SetRemove <$> arbitrary
                ]

instance Arbitrary C.Set where
    arbitrary = C.Set . Set.fromList <$> arbitrary

instance Arbitrary ByteString where
    arbitrary = elements [ "foo", "bar", "baz" ]

instance Arbitrary C.MapOp where
    arbitrary = C.MapUpdate <$> arbitrary <*> arbitrary

instance Arbitrary C.MapPath where
    arbitrary = (\a b -> C.MapPath (a :| b)) <$> arbitrary <*> arbitrary

instance Arbitrary C.MapField where
    arbitrary = C.MapField <$> arbitrary <*> arbitrary

instance Arbitrary C.MapEntryTag where
    arbitrary = elements [ C.MapCounterTag ]

instance Arbitrary C.MapValueOp where
    arbitrary = oneof [ C.MapCounterOp <$> arbitrary ]

instance Arbitrary C.Map


riak :: forall a. Action a => a -> Proxy a
     -> [Op a] -> RWST () [Maybe C.DataType] B.Connection IO ()

riak _ _ [] = pure ()

riak v p (CGet (Bucket b) (Key k) : as) = do
  c <- get
  r <- liftIO $ C.get c (bucketType p) b k
  tell [r]
  riak v p as

riak v p (CUpdate (Bucket b) (Key k) op : as) = do
  c <- get
  liftIO $ C.sendModify c (bucketType p) b k v [op]
  riak v p as


doRiak v p ops = withSomeConnection $ \conn -> do
                   --print ops
                   (_,_,r) <- runRWST (riak v p ops) () conn
                   pure r

doPure :: Action a => RiakState
       -> a -> Proxy a -> [Op a] -> PropertyM IO [Maybe C.DataType]
doPure stat v p ops = do (_,_,r) <- runRWST (pure_ v p ops) () stat
                         pure r

pure_ :: Action a => a -> Proxy a
       -> [Op a] -> RWST () [Maybe C.DataType] RiakState (PropertyM IO) ()

pure_ _ _ [] = pure ()

pure_ x p (CGet b k : as) = do
  v <- gets (Map.lookup (Point (BucketType (bucketType p)) b k))
  tell [v]
  pure_ x p as

pure_ x p (CUpdate b k op : as) = do
  modify (Map.alter (update x op)
                    (Point (BucketType (bucketType p)) b k))
  pure_ x p as


update :: forall a. (Action a) => a -> C.Operation_ a -> Maybe C.DataType -> Maybe C.DataType
update z op Nothing = update z op (Just . toDT' $ emptyVal) -- it's ok to update non-set value
                                                            -- in riak's mind
    where toDT' :: a -> C.DataType
          toDT' = toDT
update z op (Just dt) = Just . toDT . C.modify op . fromDT' $ dt
    where fromDT' :: C.DataType -> a
          fromDT' = fromDT


prop :: Action a => a -> Proxy a -> [Op a] -> Property
prop v p ops = monadicIO $ do
                          stat <- run observeRiak
                          r1 <- doPure stat v p ops
                          r2 <- run $ doRiak v p ops
                          run . when (r1/=r2) $ print (r1,r2)
                          assert $ r1 == r2


prop_counters = prop emptyVal (Proxy :: Proxy C.Counter)
prop_sets     = prop emptyVal (Proxy :: Proxy C.Set)
prop_maps     = prop emptyVal (Proxy :: Proxy C.Map)



tests :: TestTree
tests = testGroup "CRDT quickCheck" [
         testProperty "counters" prop_counters,
         testProperty "sets" prop_sets
        ]

{-
-- Quick'n'dirty (but too slow) way to clean all riak db state
wipeClean :: IO ()
wipeClean = do r <- timeout 30 "riak restart && riak-admin wait-for-service riak_kv"
               case r of
                 Right ExitSuccess -> pure ()
                 x -> error $ "wipeClean failed, " <> show x

timeout :: Int -> String -> IO (Either () ExitCode)
timeout secs cmd = race a b
    where a = threadDelay (1000*1000*secs)
          b = bracket (spawnCommand cmd)
                      terminateProcess
                      waitForProcess

-}

