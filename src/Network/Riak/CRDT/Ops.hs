module Network.Riak.CRDT.Ops (counterUpdateOp,
                              setUpdateOp, SetOpsComb(..), toOpsComb)
    where

import qualified Network.Riak.Protocol.DtOp as PB
import qualified Network.Riak.Protocol.CounterOp as PB
import qualified Network.Riak.Protocol.SetOp as PB
import Network.Riak.CRDT.Types
import Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.ByteString.Lazy (ByteString)


counterUpdateOp :: [CounterOp] -> PB.DtOp
counterUpdateOp ops = PB.DtOp { PB.counter_op = Just $ PB.CounterOp (Just i),
                                PB.set_op = Nothing,
                                PB.map_op = Nothing
                              }
    where Inc i = mconcat ops



data SetOpsComb = SetOpsComb { setAdds :: S.Set ByteString,
                               setRemoves :: S.Set ByteString }
             deriving (Show)

instance Monoid SetOpsComb where
    mempty = SetOpsComb mempty mempty
    (SetOpsComb a b) `mappend` (SetOpsComb x y) = SetOpsComb (a<>x) (b<>y)

toOpsComb (SetAdd s) = SetOpsComb (S.singleton s) S.empty
toOpsComb (SetRemove s) = SetOpsComb S.empty (S.singleton s)



setUpdateOp :: [SetOp] -> PB.DtOp
setUpdateOp ops = PB.DtOp { PB.counter_op = Nothing,
                            PB.set_op = Just $ PB.SetOp (toSeq adds) (toSeq rems),
                            PB.map_op = Nothing
                          }
    where SetOpsComb adds rems = mconcat . map toOpsComb $ ops
          toSeq = Seq.fromList . S.toList
