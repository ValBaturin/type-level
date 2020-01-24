{-# LANGUAGE StarIsType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE GADTs #-}
 
import GHC.TypeLits
import Data.Typeable

type family Or (x :: Bool) (y :: Bool) :: Bool where
    Or 'True y = 'True
    Or 'False y = y

typeName :: forall a. Typeable a => String
typeName = show . typeRep $ Proxy @a

data Typelevel
main = putStrLn $ typeName @Typelevel

data Expr_ a
    = (a ~ Int) => LitInt_ Int
    | (a ~ Bool) => LitBool_ Bool
    | (a ~ Int) => Add_ (Expr_ Int) (Expr_ Int)
    | (a ~ Bool) => Not_ (Expr_ Bool)
    | If_ (Expr_ Bool) (Expr_ a) (Expr_ a)

data Expr a where
    LitInt :: Int -> Expr Int
    LitBool :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Not :: Expr Bool -> Expr Bool
    If :: Expr Bool -> Expr a -> Expr a -> Expr a


