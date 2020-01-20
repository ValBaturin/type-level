 {-# LANGUAGE StarIsType #-}
 {-# LANGUAGE DataKinds #-}
 {-# LANGUAGE TypeOperators #-}
 {-# LANGUAGE TypeFamilies #-}
import GHC.TypeLits

type family Or (x :: Bool) (y :: Bool) :: Bool where
    Or 'True y = 'True
    Or 'False y = y

main = putStrLn "type-level"
