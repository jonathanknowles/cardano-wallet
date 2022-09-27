{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Support for verifying hashed passwords from the old wallet codebase.
--
-- These passwords were encrypted by the @scrypt@ package using the following
-- parameters:
--  - logN = 14
--  - r = 8
--  - p = 1
--  - outputLength = 64
--
-- It is possible to disable support for legacy password hashing by compiling
-- with the @-scrypt@ Cabal flag.
module Cardano.Wallet.Primitive.Passphrase.Legacy
  ( -- * Legacy passphrases
    checkPassphrase,
    preparePassphrase,

    -- * Testing-only scrypt password implementation
    checkPassphraseTestingOnly,
    encryptPassphraseTestingOnly,

    -- * Testing-only helper
    haveScrypt,

    -- * Internal functions
    getSalt,
    genSalt,
  )
where

import Cardano.Wallet.Primitive.Passphrase.Types
  ( Passphrase (..),
    PassphraseHash (..),
  )
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import Crypto.Hash.Utils
  ( blake2b256,
  )
import qualified Crypto.KDF.Scrypt as Scrypt
import Crypto.Random.Types
  ( MonadRandom (..),
  )
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding
  ( Base (..),
    convertFromBase,
    convertToBase,
  )
import Data.ByteString
  ( ByteString,
  )
import qualified Data.ByteString.Char8 as B8
import Data.Either.Extra
  ( eitherToMaybe,
  )
import Data.Word
  ( Word64,
  )
import Prelude

#if HAVE_SCRYPT
import Crypto.Scrypt
    ( EncryptedPass (..), Pass (..), verifyPass' )

-- | Verify a wallet spending password using the legacy Byron scrypt encryption
-- scheme.
checkPassphrase :: Passphrase "encryption" -> PassphraseHash -> Maybe Bool
checkPassphrase pwd stored = Just $
    verifyPass' (Pass (BA.convert (cborify pwd))) encryptedPass
  where
    encryptedPass = EncryptedPass (BA.convert stored)

haveScrypt :: Bool
haveScrypt = True
#else
-- | Stub function for when compiled without @scrypt@.
checkPassphrase :: Passphrase "encryption" -> PassphraseHash -> Maybe Bool
checkPassphrase _ _ = Nothing

haveScrypt :: Bool
haveScrypt = False
#endif

preparePassphrase :: Passphrase "user" -> Passphrase "encryption"
preparePassphrase = Passphrase . hashMaybe . unPassphrase
  where
    hashMaybe pw
      | pw == mempty = mempty
      | otherwise = BA.convert $ blake2b256 pw

-- | This is for use by test cases only. Use only the implementation from the
-- @scrypt@ package for application code.
checkPassphraseTestingOnly :: Passphrase "encryption" -> PassphraseHash -> Bool
checkPassphraseTestingOnly pwd stored = case getSalt stored of
  Just salt -> encryptPassphraseTestingOnly pwd salt == stored
  Nothing -> False

cborify :: Passphrase "encryption" -> Passphrase "encryption"
cborify =
  Passphrase . BA.convert . CBOR.toStrictByteString
    . CBOR.encodeBytes
    . BA.convert
    . unPassphrase

-- | Extract salt field from pipe-delimited password hash.
-- This will fail unless there are exactly 5 fields
getSalt :: PassphraseHash -> Maybe (Passphrase "salt")
getSalt (PassphraseHash stored) = case B8.split '|' (BA.convert stored) of
  [_logN, _r, _p, salt, _passHash] ->
    eitherToMaybe $
      Passphrase <$> convertFromBase Base64 salt
  _ -> Nothing

-- | This is for use by test cases only.
encryptPassphraseTestingOnly ::
  MonadRandom m =>
  Passphrase "encryption" ->
  m PassphraseHash
encryptPassphraseTestingOnly pwd = mkPassphraseHash <$> genSalt
  where
    mkPassphraseHash salt =
      PassphraseHash $
        BA.convert $
          B8.intercalate
            "|"
            [ showBS logN,
              showBS r,
              showBS p,
              convertToBase Base64 salt,
              convertToBase Base64 (passHash salt)
            ]

    passHash :: Passphrase "salt" -> ByteString
    passHash (Passphrase salt) = Scrypt.generate params (cborify pwd) salt

    params = Scrypt.Parameters ((2 :: Word64) ^ logN) r p 64
    logN = 14
    r = 8
    p = 1

    showBS = B8.pack . show

genSalt :: MonadRandom m => m (Passphrase "salt")
genSalt = Passphrase <$> getRandomBytes 32
