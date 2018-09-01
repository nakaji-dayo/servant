{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Description (
    -- * Combinators
    Description,
    Summary,
    Tags,
    KnownSymbols(..),
    -- * Used as modifiers
    FoldDescription,
    FoldDescription',
    reflectDescription,

    ) where

import           Data.Proxy
                 (Proxy (..))
import           Data.Typeable
                 (Typeable)
import           GHC.TypeLits
                 (KnownSymbol, Symbol, symbolVal)

-- | Add a short summary for (part of) API.
--
-- Example:
--
-- >>> type MyApi = Summary "Get book by ISBN." :> "books" :> Capture "isbn" Text :> Get '[JSON] Book
data Summary (sym :: Symbol)
    deriving (Typeable)

-- | Add more verbose description for (part of) API.
--
-- Example:
--
-- >>> :{
--type MyApi = Description
--  "This comment is visible in multiple Servant interpretations \
--  \and can be really long if necessary. \
--  \Haskell multiline support is not perfect \
--  \but it's still very readable."
-- :> Get '[JSON] Book
-- :}
data Description (sym :: Symbol)
    deriving (Typeable)

-- | Fold modifier list to decide whether argument should be parsed strictly or leniently.
--
-- >>> :kind! FoldDescription '[]
-- FoldDescription '[] :: Symbol
-- = ""
--
-- >>> :kind! FoldDescription '[Required, Description "foobar", Lenient]
-- FoldDescription '[Required, Description "foobar", Lenient] :: Symbol
-- = "foobar"
--
type FoldDescription mods = FoldDescription' "" mods

-- | Implementation of 'FoldDescription'.
type family FoldDescription' (acc :: Symbol) (mods ::  [*]) :: Symbol where
    FoldDescription' acc '[]                        = acc
    FoldDescription' acc (Description desc ': mods) = FoldDescription' desc mods
    FoldDescription' acc (mod     ': mods)          = FoldDescription' acc mods

-- | Reflect description to the term level.
--
-- >>> reflectDescription (Proxy :: Proxy '[Required, Description "foobar", Lenient])
-- "foobar"
--
reflectDescription :: forall mods. KnownSymbol (FoldDescription mods) => Proxy mods -> String
reflectDescription _ = symbolVal (Proxy :: Proxy (FoldDescription mods))

data Tags (sym :: [Symbol])
    deriving (Typeable)

class KnownSymbols a where
    symbolVals :: proxy a -> [String]
instance KnownSymbols '[] where
    symbolVals _ = []
instance (KnownSymbol h, KnownSymbols t) => KnownSymbols (h ': t) where
    symbolVals _ = symbolVal (Proxy :: Proxy h) : symbolVals (Proxy :: Proxy t)

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
-- >>> data SourceFile
-- >>> instance ToJSON SourceFile where { toJSON = undefined }
