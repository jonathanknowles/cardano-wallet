{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Utils.Laws
    ( testLaws
    , testLawsMany
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Proxy
    ( Proxy (..) )
import Data.Typeable
    ( Typeable, typeRep )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..) )
import Test.QuickCheck.Classes
    ( Laws (..) )

-- | Constructs a test to check that the given type class instance obeys the
--   given set of laws.
--
-- Example usage:
--
-- >>> testLaws @Natural ordLaws
--
testLaws
    :: forall a. (Arbitrary a, Eq a, Show a, Typeable a)
    => (Proxy a -> Laws)
    -> Spec
testLaws getLaws =
    describe description $
        forM_ (lawsProperties laws) $ uncurry it
  where
    description = mconcat
        [ "Testing "
        , lawsTypeclass laws
        , " laws for type "
        , show (typeRep $ Proxy @a)
        ]
    laws = getLaws $ Proxy @a

testLawsMany
    :: forall a. (Arbitrary a, Eq a, Show a, Typeable a)
    => [Proxy a -> Laws]
    -> Spec
testLawsMany getLawsMany =
    testLaws @a `mapM_` getLawsMany
