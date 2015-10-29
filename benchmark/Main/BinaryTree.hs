module Main.BinaryTree where

import Main.Prelude hiding (traverse_, fold, empty)
import Foreign hiding (void)
import Control.Monad.Primitive
import qualified Data.ByteString as A
import qualified Data.ByteString.Internal as B


newtype Builder =
  Builder (BinaryTree Bytes)

data BinaryTree a =
  Void |
  Leaf a |
  Branch (BinaryTree a) (BinaryTree a)
  deriving (Functor, Foldable, Traversable)

instance Monoid Builder where
  mempty = empty
  mappend = append

instance IsString Builder where
  fromString = bytes . fromString

empty :: Builder
empty =
  Builder (Void)

append :: Builder -> Builder -> Builder
append (Builder tree1) (Builder tree2) =
  Builder (Branch tree1 tree2)

bytes :: Bytes -> Builder
bytes bytes =
  Builder (Leaf bytes)

lengthOf :: Builder -> Int
lengthOf (Builder tree) =
  case tree of
    Void -> 0
    Leaf bytes -> A.length bytes
    Branch tree1 tree2 -> lengthOf (Builder tree1) + lengthOf (Builder tree2)

traverse_ :: Applicative m => (Bytes -> m ()) -> Builder -> m ()
traverse_ action (Builder tree) =
  case tree of
    Void -> action A.empty
    Leaf bytes -> action bytes
    Branch tree1 tree2 -> traverse_ action (Builder tree1) *> traverse_ action (Builder tree2)

fold :: (a -> Bytes -> a) -> a -> Builder -> a
fold step init (Builder tree) =
  case tree of
    Void -> init
    Leaf bytes -> step init bytes
    Branch tree1 tree2 -> fold step (fold step init (Builder tree1)) (Builder tree2)

traverseEachByte_ :: Applicative m => (Word8 -> m ()) -> Builder -> m ()
traverseEachByte_ action (Builder tree) =
  case tree of
    Void -> pure ()
    Leaf bytes -> A.foldl' (\acc byte -> acc *> action byte) (pure ()) bytes
    Branch tree1 tree2 -> traverseEachByte_ action (Builder tree1) *> traverseEachByte_ action (Builder tree2)

traverseEachByteWithIndex_ :: Applicative m => ((Int, Word8) -> m ()) -> Builder -> m ()
traverseEachByteWithIndex_ action builder =
  snd $
  foldEachByte
    (\(index, acc) byte -> (succ index, acc *> action (index, byte)))
    (0, pure ())
    builder

foldEachByte :: (a -> Word8 -> a) -> a -> Builder -> a
foldEachByte step init (Builder tree) =
  case tree of
    Void -> init
    Leaf bytes -> A.foldl' step init bytes
    Branch tree1 tree2 -> foldEachByte step (foldEachByte step init (Builder tree1)) (Builder tree2)

bytesOf_thruList :: Builder -> Bytes
bytesOf_thruList (Builder tree) =
  mconcat (toList tree)

-- |
-- FIXME: Seriously needs some optimization!
-- It must perform better than \"thruList\", not worse.
-- 
-- See
-- http://hackage.haskell.org/package/bytestring-0.10.6.0/docs/src/Data.ByteString.Internal.html#unsafeCreate
bytesOf_explicitAllocation :: Builder -> Bytes
bytesOf_explicitAllocation builder =
  B.unsafeCreate (lengthOf builder) $ \ptr ->
    void $
    foldEachByte
      (\ptrIO byte -> ptrIO >>= \ptr -> poke ptr byte >> pure (plusPtr ptr 1))
      (pure ptr)
      builder
