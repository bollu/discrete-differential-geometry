{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where
import Control.Monad(ap, join)
import qualified Data.Map as M
import qualified Data.List as L
-- | reals
type R = Float
-- | integers
type Z = Int

data Nat = NZ | NS Nat

-- | finite maps from a to b
type (#>) a b = M.Map a b

-- | evaluate a finite map
(#$) :: Ord a => (a #> b) -> a -> b
(#$) = (M.!)




-- | homology provides a boundary operator over a graded type a
class HomologyN (a :: Nat -> *) where 
   boundaryn :: a (NS n) -> (a n)

class CohomologyN (a :: Nat -> *) where
  coboundaryn :: a n -> a (NS n)

-- d^2 = 0
class Homology a where
    boundary :: a -> a

class Cohomology where
    coboundary :: a -> a

-- | free abelian group over 'a'
data FreeAb (a :: Nat -> *) (n:: Nat)   where
    FreeAb :: [(a n, Z)] -> FreeAb a n

unFreeAb :: FreeAb a n -> [(a n, Z)]
unFreeAb (FreeAb coeffs) = coeffs

instance Show (a NZ) => Show (FreeAb a NZ) where
    show (FreeAb a) = show a

instance Show (a (NS n)) => Show (FreeAb a (NS n)) where
    show (FreeAb a) = show a

instance Ord (a NZ) => Ord (FreeAb a NZ) where
    compare = undefined

instance Ord (a (NS n)) => Ord (FreeAb a (NS n)) where
    compare = undefined

deriving instance (Eq (a n)) => Eq (FreeAb a n)
-- deriving instance (Ord (a n)) => Ord (FreeAb a n)

-- | simplify, by removing zero elements
simplifyFreeAb :: (Ord (a n)) => FreeAb a n -> FreeAb a n
simplifyFreeAb (FreeAb coeffs) = 
  let coeffs' = M.toList $ M.fromListWith (+) coeffs
  in FreeAb $ [(a, c) | (a, c) <- coeffs', c /= 0]


class GradedFunctor (f ::  (Nat -> *) -> Nat -> *) where
    gfmap :: (a n -> b m) -> f a n -> f b m

-- | should return be allowd to inject into any level?
class GradedMonad (f ::  (Nat -> *)  -> Nat -> *) where
    greturn :: a n -> f a n
    gbind :: f a n  -> (a n -> f m b) -> f m b

instance GradedFunctor FreeAb where
    gfmap f (FreeAb coeffs) = FreeAb $ [(f a, c) | (a, c) <- coeffs]

instance GradedMonad FreeAb where
    greturn a = FreeAb [(a, 1)]
    gbind (FreeAb fa) (a2fb) = FreeAb $ do
     (a, c) <- fa
     (b, c') <- unFreeAb $ a2fb a
     return $ (b, c * c')
        

gjoin :: FreeAb (FreeAb a) n -> FreeAb a n
gjoin ffa = gbind ffa id

-- instance Monad (FreeAb n) where
--   return a = FreeAb $ [(a, 1)]
--   -- FreeAb a -> (a -> FreeAb b) -> FreeAb b
--   (FreeAb fa) >>= a2fb = simplifyFreeAb $ FreeAb $ do
--     (a, c) <- fa
--     (b, c') <- unFreeAb $ a2fb a
--     return $ (b, c * c')
-- 
-- instance Applicative FreeAb where
--   (<*>) = ap
--   pure = return
--   
-- instance Functor FreeAb where
--     fmap f (FreeAb ab) = FreeAb ([(f a, c) | (a, c) <- ab])
-- 
-- 
-- instance Show a => Show (FreeAb a) where
--  show (FreeAb coeffs) = 
--    let 
--    in if null coeffs
--       then "0"
--       else "(" <> L.intercalate " + " [show z <> show a | (a, z) <- coeffs] <> ")"



instance Ord (a n) => Num (FreeAb a n) where
  fromInteger 0 = FreeAb $ []
  fromInteger x = error "unimplemented fromInteger"
  (FreeAb f) +  (FreeAb g) = simplifyFreeAb $ FreeAb $ f ++ g
  negate (FreeAb coeffs) = FreeAb $ [(a, -z) | (a, z) <- coeffs]




-- | uninhabited
data Void

-- | discrete n-dimensional manifold on abstract points b
data DiscreteManifold (b :: *) (n :: Nat) where
    Point :: b -> DiscreteManifold b NZ
    Boundary :: FreeAb (DiscreteManifold b) n -> DiscreteManifold b (NS n)

unBoundary :: DiscreteManifold b (NS n) -> FreeAb (DiscreteManifold b) n
unBoundary (Boundary chain) = chain


boundaryab :: (Ord (DiscreteManifold a n)) =>  FreeAb (DiscreteManifold a) (NS n)  -> FreeAb  (DiscreteManifold a) n
boundaryab chain_manifold = simplifyFreeAb $ gjoin $ gfmap unBoundary chain_manifold



(-.) :: Ord (DiscreteManifold b n) => DiscreteManifold b n  -> DiscreteManifold b n ->  DiscreteManifold b (NS n)
(-.) a b = Boundary $ FreeAb $ [(b, -1), (a, 1)]


-- | Create a higher dimensional manifold given the boundary
fromBoundary :: Ord (DiscreteManifold b n) => [DiscreteManifold b n] -> DiscreteManifold b  (NS n)
fromBoundary ms = Boundary $ FreeAb $ [(m, 1) | m <- ms]



-- deriving instance (Eq b, Ord b, Eq (DiscreteManifold b n), Ord (DiscreteManifold b n)) => Ord (DiscreteManifold b (NS n))
instance Eq b => Eq (DiscreteManifold b NZ) where
   Point b == Point b' = b == b'

instance (Eq b, Ord b) => Ord (DiscreteManifold b NZ) where
  Point b `compare` Point b' = b `compare` b'


instance (Eq (DiscreteManifold b n)) => Eq (DiscreteManifold b (NS n)) where
  Boundary b == Boundary b' = b == b'

instance (Eq (DiscreteManifold b n), Ord (FreeAb (DiscreteManifold b) n)) => Ord (DiscreteManifold b (NS n)) where
  Boundary b `compare` Boundary b' = b `compare` b'


instance Show b => Show (DiscreteManifold b NZ) where
   show (Point p) = show p

instance (Show (FreeAb (DiscreteManifold b) n),  Show b) => Show (DiscreteManifold b (NS n)) where
    show (Boundary b) = show b



-- | differential forms on an n-dimensional manifold over points b
data Form (b :: *) (n :: Nat) where
    Function :: (DiscreteManifold b n -> R) -> Form b n
    Differential :: Form b n -> Form b (NS n)

 

-- instance Homology (DiscreteManifold b) where
--   boundary (Boundary chainSN) = Boundary $ FreeAb $ M.fromListWith (+) $ do
--         (mapsN, c) <- M.toList $ unFreeAb $ chainSN
--         return $ (mapsN, c)
    

instance CohomologyN (Form b) where
    -- | automatic optimisation: boundary of boundary is zero
    coboundaryn (Differential (Differential _)) = Function (\_ -> 0)
    coboundaryn x = Differential x

-- Integral(omega) (dS) = integral (dOmega) s
class (HomologyN a, CohomologyN b) => PairingN a b | a -> b, b -> a where
   integrate :: a n -> b n -> R

a, b, c :: DiscreteManifold Char NZ
a = Point 'a'
b = Point 'b'
c = Point 'c'

ab, bc, ca :: DiscreteManifold Char (NS NZ)
ab = b -. a
bc = c -. b
ca = a -. c

loop :: DiscreteManifold Char (NS (NS NZ))
loop = fromBoundary [ab, bc, ca]

-- Simplifying using loops is monadic!
loopBoundary :: FreeAb (DiscreteManifold Char) NZ
loopBoundary = simplifyFreeAb $  unBoundary  loop `gbind` unBoundary 




main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
