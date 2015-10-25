module Database.Toy.Internal.Util.FixedSizeSerialize where

import Data.Serialize (Serialize)
import Database.Toy.Internal.Prelude
import Foreign.Storable (sizeOf)

-- | Typeclass that guarantees that size of serialized
-- object would be the same for any instance of object
class Serialize a => FixedSizeSerialize a where
    -- | Return size of serialized instance in bytes. Function
    -- argument is unused.
    serializedSize :: a -> Int


instance FixedSizeSerialize Word8 where
    serializedSize = sizeOf

instance FixedSizeSerialize Word16 where
    serializedSize = sizeOf

instance FixedSizeSerialize Word32 where
    serializedSize = sizeOf
