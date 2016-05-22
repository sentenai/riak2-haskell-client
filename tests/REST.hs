module REST (get,put) where

import Network.HTTP
import qualified Network.URI as URI
import Data.Semigroup
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Network.Riak.Types (BucketType, Bucket, Key)
import qualified Data.ByteString.Lazy as BSL

type NodeAddr = String
newtype UnescapedURI = UnescapedURI String
newtype URIEscaped = URIEscaped String
newtype URI = URI String

get :: NodeAddr -> BucketType -> Bucket -> Key -> IO (Maybe String)
get node btype buck key = do
  let URI url = accessURL node btype buck key
  r <- simpleHTTP $ getRequest url
  st <- getResponseCode r

  case st of
    (2,0,0) -> Just <$> getResponseBody r
    _ -> pure Nothing



decodeBS :: BSL.ByteString -> UnescapedURI
decodeBS = UnescapedURI . T.unpack . T.decodeUtf8 . BSL.toStrict

reencode :: BSL.ByteString -> URIEscaped
reencode = escapeURI . decodeBS

escapeURI :: UnescapedURI -> URIEscaped
escapeURI (UnescapedURI s) = URIEscaped . URI.escapeURIString f $ s
    where f '/' = False
          f c   = URI.isUnescapedInURI c

pbPort = 8098 :: Int

accessURL :: NodeAddr -> BucketType -> Bucket -> Key -> URI
accessURL node btype buck key = URI uri
    where
      uri = printf "http://%s:%d/types/%s/buckets/%s/keys/%s" node pbPort btypeU buckU keyU
      URIEscaped btypeU = reencode btype
      URIEscaped buckU  = reencode buck
      URIEscaped keyU   = reencode key


rawContentType = "application/octet-stream" :: String


put :: NodeAddr -> BucketType -> Bucket -> Key -> String -> IO ()
put node btype buck key dat = do
  let URI url = accessURL node btype buck key
  _ <- simpleHTTP $ postRequestWithBody url rawContentType dat
  pure ()


-- | Escape a string by riak rules.
escape :: String -> String
escape = concatMap f
    where f '/' = "%2F"
          f c = [c]

