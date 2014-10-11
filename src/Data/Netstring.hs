-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Data.Netstring where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Builder              as Builder
import           Data.ByteString.Char8                   (ByteString)
import qualified Data.ByteString.Char8                as Bytes
import           Data.ByteString.Conversion
import           Data.Monoid                             ((<>), Monoid(..))
import           Prelude                          hiding (take)


data Netstring = Netstring Int ByteString
    deriving Eq

instance Show Netstring where
    show (Netstring l s) = show l ++ ":" ++ Bytes.unpack s ++ ","

instance Monoid Netstring where
    mempty = Netstring 0 Bytes.empty
    mappend (Netstring la sa) (Netstring lb sb) =
        Netstring (la + lb) (sa <> sb)

instance Ord Netstring where
    (Netstring _ sa) <= (Netstring _ sb) = sa <= sb

instance FromByteString Netstring where
    parser = do
        l <- decimal <* char ':'
        s <- take l  <* char ','
        return $ Netstring l s

instance FromByteString [Netstring] where
    parser = many' parser

instance ToByteString Netstring where
    builder (Netstring l s) =
        Builder.string7 (show l)
            <> Builder.char7 ':'
            <> Builder.byteString s
            <> Builder.char7 ','

instance ToByteString [Netstring] where
    builder = Builder.byteString . foldr ((<>) . toByteString') mempty

toNetstring :: ToByteString a => a -> Netstring
toNetstring (toByteString' -> s) = Netstring (Bytes.length s) s
