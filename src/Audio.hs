module Audio where

import RIO
import Codec.Audio.Wave
import Conduit
import Data.Conduit.Serialization.Binary
import Data.Binary.Get (getInt16le)

decoder :: (MonadThrow m) => ConduitT ByteString Int16 m ()
decoder = conduitGet getInt16le

