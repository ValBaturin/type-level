{-# LANGUAGE StarIsType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import GHC.TypeLits
import Data.Typeable

type family Or (x :: Bool) (y :: Bool) :: Bool where
    Or 'True y = 'True
    Or 'False y = y

typeName :: forall a. Typeable a => String
typeName = show . typeRep $ Proxy @a

data Typelevel
main = putStrLn $ typeName @Typelevel
