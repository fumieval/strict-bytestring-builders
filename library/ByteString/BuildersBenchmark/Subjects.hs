module ByteString.BuildersBenchmark.Subjects where

import Prelude
import qualified ByteString.StrictBuilder as A
import qualified ByteString.TreeBuilder as G
import qualified Data.BufferBuilder as B
import qualified Data.Binary.Put as C
import qualified Data.Serialize.Put as E
import qualified Data.ByteString.Lazy as D
import qualified Data.ByteString.Builder as F
import qualified Data.ByteString.FastBuilder as H
import qualified Blaze.ByteString.Builder as I
import qualified Mason.Builder as M
import qualified Mason.Builder.Internal as M
import Data.Word (Word8)
import Data.Monoid (Ap(..))

data Subject = forall a. Monoid a => Subject
  { fromBytes :: ByteString -> a
  , toBytes :: a -> ByteString
  , word8 :: Word8 -> a
  }

byteStringStrictBuilder :: Subject
byteStringStrictBuilder =
  Subject A.bytes A.builderBytes A.word8
{-# INLINE byteStringStrictBuilder #-}

byteStringTreeBuilder :: Subject
byteStringTreeBuilder =
  Subject G.byteString G.toByteString G.byte
{-# INLINE byteStringTreeBuilder #-}

bufferBuilder :: Subject
bufferBuilder =
  Subject (Ap . B.appendBS) (B.runBufferBuilder . getAp) (Ap . B.appendByte)
{-# INLINE bufferBuilder #-}

binary :: Subject
binary =
  Subject C.putByteString (D.toStrict . C.runPut) C.putWord8
{-# INLINE binary #-}

cereal :: Subject
cereal =
  Subject E.putByteString E.runPut E.putWord8
{-# INLINE cereal #-}

byteString :: Subject
byteString =
  Subject F.byteString (D.toStrict . F.toLazyByteString) F.word8
{-# INLINE byteString #-}

fastBuilder :: Subject
fastBuilder =
  Subject H.byteString H.toStrictByteString H.word8
{-# INLINE fastBuilder #-}

blazeBuilder :: Subject
blazeBuilder =
  Subject I.fromByteString (D.toStrict . I.toLazyByteString) I.fromWord8
{-# INLINE blazeBuilder #-}

masonBuilder :: Subject
masonBuilder =
  Subject M.byteStringCopy M.toStrictByteString M.word8
{-# INLINE masonBuilder #-}
