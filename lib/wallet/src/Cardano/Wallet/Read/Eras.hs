-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Re-export `EraValue` library.
module Cardano.Wallet.Read.Eras
  ( -- * Eras.
    KnownEras,
    knownEraIndices,

    -- * Era bounded values.
    EraValue,
    eraValueSerialize,
    extractEraValue,

    -- * Era specific prisms.
    MkEraValue (..),
    byron,
    shelley,
    allegra,
    mary,
    alonzo,
    babbage,

    -- * Era specific prism shortcuts.
    inject,
    project,

    -- * Specials.
    sequenceEraValue,

    -- * Era bounded functions.
    EraFun (..),

    -- * Composing era functions.
    (*.**),
    (*&&&*),

    -- * Applying era functions.
    applyEraFun,

    -- * Reexports from elsewhere.
    (:.:) (..),
    K (..),
    unK,
    (:*:) (..),

    -- * Conversion.
    isoInAnyCardanoEra,
  )
where

import Cardano.Wallet.Read.Eras.EraFun
  ( EraFun (..),
    applyEraFun,
    (*&&&*),
    (*.**),
  )
import Cardano.Wallet.Read.Eras.EraValue
  ( EraValue,
    MkEraValue (..),
    allegra,
    alonzo,
    babbage,
    byron,
    eraValueSerialize,
    extractEraValue,
    inject,
    mary,
    project,
    sequenceEraValue,
    shelley,
  )
import Cardano.Wallet.Read.Eras.InAnyCardanoEra
  ( isoInAnyCardanoEra,
  )
import Cardano.Wallet.Read.Eras.KnownEras
  ( KnownEras,
    knownEraIndices,
  )
import GHC.Generics
  ( (:*:) (..),
  )
import Generics.SOP
  ( K (..),
    unK,
    (:.:) (..),
  )
