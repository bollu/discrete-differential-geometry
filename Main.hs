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
import Control.Monad(ap)
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
class Homology (a :: Nat -> *) where 
   boundary :: a (NS n) -> (a n)

class Cohomology (a :: Nat -> *) where
  coboundary :: a n -> a (NS n)
-- | free abelian group over 'a'
data FreeAb a where
    FreeAb :: [(a, Z)] ->FreeAb a

unFreeAb :: FreeAb a -> [(a, Z)]
unFreeAb (FreeAb a) = a

deriving instance Eq a => Eq (FreeAb a)
deriving instance Ord a => Ord (FreeAb a)

-- | simplify, by removing zero elements
simplifyFreeAb :: FreeAb a -> FreeAb a 
simplifyFreeAb (FreeAb coeffs) = 
  let coeffs' = coeffs -- M.toList $ M.fromListWith (+) coeffs
  in FreeAb $ [(a, c) | (a, c) <- coeffs', c /= 0]

instance Monad FreeAb where
  return a = FreeAb $ [(a, 1)]
  -- FreeAb a -> (a -> FreeAb b) -> FreeAb b
  (FreeAb fa) >>= a2fb = simplifyFreeAb $ FreeAb $ do
    (a, c) <- fa
    (b, c') <- unFreeAb $ a2fb a
    return $ (b, c * c')

instance Applicative FreeAb where
  (<*>) = ap
  pure = return
  
instance Functor FreeAb where
    fmap f (FreeAb ab) = FreeAb ([(f a, c) | (a, c) <- ab])


instance Show a => Show (FreeAb a) where
 show (FreeAb coeffs) = 
   let 
   in if null coeffs
      then "0"
      else "(" <> L.intercalate " + " [show z <> show a | (a, z) <- coeffs] <> ")"



instance Ord a => Num (FreeAb a) where
  fromInteger 0 = FreeAb $ []
  fromInteger x = error "unimplemented fromInteger"
  (FreeAb f) +  (FreeAb g) = simplifyFreeAb $ FreeAb $ f ++ g
  negate (FreeAb coeffs) = FreeAb $ [(a, -z) | (a, z) <- coeffs]


-- | uninhabited
data Void

-- | discrete n-dimensional manifold on abstract points b
data DiscreteManifold (b :: *) (n :: Nat) where
    Point :: b -> DiscreteManifold b NZ
    Boundary :: FreeAb (DiscreteManifold b n) -> DiscreteManifold b (NS n)

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

instance (Eq (DiscreteManifold b n), Ord (FreeAb (DiscreteManifold b n))) => Ord (DiscreteManifold b (NS n)) where
  Boundary b `compare` Boundary b' = b `compare` b'


instance Show b => Show (DiscreteManifold b n) where
   show (Point p) = show p
   show (Boundary b) = show b

-- | differential forms on an n-dimensional manifold over points b
data Form (b :: *) (n :: Nat) where
    Function :: (DiscreteManifold b n -> R) -> Form b n
    Differential :: Form b n -> Form b (NS n)

  
 

-- instance Homology (DiscreteManifold b) where
--   boundary (Boundary chainSN) = Boundary $ FreeAb $ M.fromListWith (+) $ do
--         (mapsN, c) <- M.toList $ unFreeAb $ chainSN
--         return $ (mapsN, c)
    

instance Cohomology (Form b) where
    -- | automatic optimisation: boundary of boundary is zero
    coboundary (Differential (Differential _)) = Function (\_ -> 0)
    coboundary x = Differential x

-- Integral(omega) (dS) = integral (dOmega) s
class (Homology a, Cohomology b) => Pairing a b | a -> b, b -> a where
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


main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
