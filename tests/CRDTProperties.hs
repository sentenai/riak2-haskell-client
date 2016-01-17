{-# LANGUAGE OverloadedStrings, FlexibleContexts, TupleSections #-}
module Machine where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.ByteString.Lazy (ByteString)
import Control.Applicative
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.RWS
import Control.Exception (bracket)
import System.Process
import System.Exit (ExitCode(ExitSuccess))

import qualified Network.Riak.Basic as B
import qualified Network.Riak.Value as V
import qualified Network.Riak.CRDT  as C

newtype BucketType = BucketType ByteString deriving (Show,Eq,Ord)
newtype Bucket     = Bucket ByteString deriving (Show,Eq,Ord)
newtype Key        = Key ByteString deriving (Show,Eq,Ord)
newtype Value      = Value ByteString deriving Show

class Values a where values :: [a]
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

withSomeConnection = bracket (B.connect B.defaultClient) B.disconnect

-- | observe all current values we care about (instance 'Values') in
--   riak
observeRiak :: IO RiakState
observeRiak = Map.fromList . catMaybes <$> observeRiak'

observeRiak' :: IO [Maybe (Point, C.DataType)]
observeRiak' = withSomeConnection $ \c ->
               sequence [ do r <- C.get c t b k
                             pure . fmap (p,) $ r
                          | p@(Point (BucketType t) (Bucket b) (Key k)) <- values ]


data Action = CGet Bucket Key
            | CUpdate Bucket Key C.CounterOp
              deriving Show

instance Arbitrary C.CounterOp where
    arbitrary = C.Inc <$> choose (-16,16)

instance Arbitrary Action where
    arbitrary = oneof [
                 CGet <$> arbitrary <*> arbitrary,
                 CUpdate <$> arbitrary <*> arbitrary <*> arbitrary
                ]

countersType = "counters"

riak :: [Action] -> RWST () [Maybe C.DataType] (B.Connection,()) IO ()
riak [] = pure ()

riak (CGet (Bucket b) (Key k) : as) = do
  (c,_) <- get
  r <- liftIO $ C.get c countersType b k
  --let cv = maybe Nothing (\(C.DTCounter cnt) -> Just cnt) r
  tell [r]
  riak as

riak (CUpdate (Bucket b) (Key k) op : as) = do
  (c,_) <- get
  liftIO $ C.counterUpdate c countersType b k [op]
  riak as


doRiak conn ops = do (_,_,r) <- runRWST (riak ops) () (conn,())
                     pure r

doPure stat ops = do (_,_,r) <- runRWST (doPurx ops) () stat
                     pure r

doPurx ops = go ops
    where --go :: [Action]
          --   -> RWST () [Maybe C.DataType] RiakState (PropertyM IO) ()
          go [] = pure ()
          go (CGet b k : as) = do v <- gets (Map.lookup (Point (BucketType"counters") b k))
                                  tell [v]
                                  go as
          go (CUpdate b k op : as) = do modify (Map.alter (update op)
                                                     (Point (BucketType"counters") b k))
                                        go as

update op Nothing = Nothing
update op (Just (C.DTCounter c)) = Just . C.DTCounter . C.modifyCounter [op] $ c
update op (Just _) = error "not a counter"

prop_counters ops = monadicIO $ \c -> do
                      stat <- run observeRiak
                      r1 <- doPure stat ops
                      r2 <- run $ doRiak c ops
                      assert $ r1==r2


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

{-

data BasicAction = Put Bucket Key Value
                 | Get Bucket Key

-- ru :: [BasicAction] -> RWST _ [Maybe ByteString]
--                        (Map.Map (ByteString, ByteString) ByteString) _ ()
ru [] = pure ()
ru (Put (Bucket b) (Key k) (Value v) : as)
    = do modify (Map.alter (const $ Just v) (b,k))
         ru as
ru (Get (Bucket b) (Key k) : as)
    = do v <- gets (Map.lookup (b,k))
         tell [v]
         ru as


-}
