{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Properties where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative          ((<$>))
#endif
import qualified Data.ByteString.Lazy         as L
import           Data.IORef                   (IORef, modifyIORef, newIORef)
import           Data.Maybe
import qualified Network.Riak.Basic           as B
import           Network.Riak.Connection      (defaultClient)
import           Network.Riak.Connection.Pool (Pool, create, withConnection)
import           Network.Riak.Content         (binary)
import           Network.Riak.Types           as Riak
import           System.IO.Unsafe             (unsafePerformIO)
import           Test.QuickCheck.Monadic      (assert, monadicIO, run)
import           Test.Tasty
import           Test.Tasty.QuickCheck

instance Arbitrary L.ByteString where
    arbitrary     = L.pack `fmap` arbitrary

newtype QCBucket = QCBucket Riak.Bucket deriving Show

instance Arbitrary QCBucket where
    arbitrary = QCBucket <$> arbitrary `suchThat` (not . L.null)

newtype QCKey = QCKey Riak.Key deriving Show

instance Arbitrary QCKey where
    arbitrary = QCKey <$> arbitrary `suchThat` (not . L.null)

cruft :: IORef [(Bucket, Key)]
{-# NOINLINE cruft #-}
cruft = unsafePerformIO $ newIORef []

pool :: Pool
{-# NOINLINE pool #-}
pool = unsafePerformIO $
       create defaultClient 1 1 1

t_put_get :: QCBucket -> QCKey -> L.ByteString -> Property
t_put_get (QCBucket b) (QCKey k) v =
    monadicIO $ assert . uncurry (==) =<< run act
  where
    act = withConnection pool $ \c -> do
            modifyIORef cruft ((b,k):)
            p <- Just <$> B.put c b k Nothing (binary v) Default Default
            r <- B.get c b k Default
            return (p,r)


tests :: [TestTree]
tests = [ testProperty "t_put_get" t_put_get ]
