{-# LANGUAGE RecordWildCards #-}
module ByteString.BuildersBenchmark.Actions where

import Prelude
import qualified ByteString.BuildersBenchmark.Subjects as A


type Action =
  A.Subject -> ByteString

foldl :: Int -> Action
foldl factor A.Subject{..} =
  toBytes $ Prelude.foldl' (<>) mempty $ replicate factor $
  (fromBytes "hello" <> fromBytes "asdf") <>
  fromBytes "fsndfn" <>
  (fromBytes "dfgknfg" <> fromBytes "aaaaaa")
{-# INLINE foldl #-}

foldr :: Int -> Action
foldr factor A.Subject{..} =
  toBytes $ Prelude.foldr (<>) mempty $ replicate factor $
  (fromBytes "hello" <> fromBytes "asdf") <>
  fromBytes "fsndfn" <>
  (fromBytes "dfgknfg" <> fromBytes "aaaaaa")
{-# INLINE foldr #-}

concat :: Int -> Action
concat factor A.Subject{..} =
  toBytes $ mconcat $ replicate factor $
  (fromBytes "hello" <> fromBytes "asdf") <>
  fromBytes "fsndfn" <>
  (fromBytes "dfgknfg" <> fromBytes "aaaaaa")
{-# INLINE concat #-}

regularConcat :: [ByteString] -> Action
regularConcat input A.Subject{..} =
  (toBytes . foldMap fromBytes) input
{-# INLINE regularConcat #-}

averagedAppends :: Int -> Action
averagedAppends factor A.Subject{..} =
  toBytes builder
  where
    builder =
      (Prelude.foldl' (<>) mempty $ replicate factor $ chunk) <>
      (Prelude.foldr (<>) mempty $ replicate factor $ chunk)
    chunk =
      (fromBytes "hello" <> fromBytes "asdf") <>
      fromBytes "fsndfn" <>
      (fromBytes "dfgknfg" <> fromBytes "aaaaaa")
{-# INLINE averagedAppends #-}

fibonacci :: Int -> Action
fibonacci factor A.Subject{..} = toBytes $ go factor where
  go 0 = word8 48
  go 1 = word8 49
  go n = go (n-2) <> word8 45 <> go (n-1)
{-# INLINE fibonacci #-}
