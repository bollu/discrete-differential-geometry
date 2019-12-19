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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Main where
import qualified GHC.TypeNats as TN
import Control.Monad(ap, join)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Prob as P
import qualified Arr as A
import qualified ChainGraded as CG
import qualified ChainUngraded as CUG
import qualified ChainFromFreeAb as CAB
import Data.Proxy
-- TODO: use http://penrose.ink/ to generate diagrams!
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


singletonFreeAb :: a n -> Z -> FreeAb a n
singletonFreeAb a i = FreeAb [(a, i)]

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

-- | evaluate a function on a free abelian group
-- nice. (a -> r) -> r. TODO: can we think of this as continuation?
evalFreeAb :: Num r => FreeAb a n -> (a n -> r) -> r
evalFreeAb (FreeAb coeffs) f = sum $ [(fromIntegral c) * (f a) | (a, c) <- coeffs ]


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



-- | find the image of a discrete manifold under a mapping of points.
mapDiscreteManifold :: (a -> b) -> DiscreteManifold a n -> DiscreteManifold b n
mapDiscreteManifold f (Point a) = Point (f a)
mapDiscreteManifold f (Boundary ab) = Boundary $ gfmap (mapDiscreteManifold f) ab 



getManifoldBoundary :: DiscreteManifold b (NS n) -> FreeAb (DiscreteManifold b) n
getManifoldBoundary (Boundary chain) = chain


-- ACHIEVEMENT 1: HAVE A THEORY OF CHAINS
getChainBoundary :: FreeAb (DiscreteManifold b) (NS n) -> FreeAb (DiscreteManifold b) n
getChainBoundary chain =  chain `gbind` getManifoldBoundary



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


-- S = dim (n + 1) | boundary(S) = dimension n
-- df = dim (n + 1) | f = dimension n
-- | integral_{S} domega = integral_dS omega
-- ACHIEVEMENT 2: HAVE A THEORY OF COCHAINS AND INTEGRATION
integrateForm :: Form b n -> FreeAb (DiscreteManifold b) n -> R
integrateForm (Function f) m =  evalFreeAb m f
integrateForm (Differential f) m = integrateForm f (getChainBoundary m)


pullbackForm :: (DiscreteManifold a n -> DiscreteManifold b m) -> Form b m -> Form a n
pullbackForm fwd (Function f) = Function (f . fwd)
pullbackForm fwd (Differential f) = undefined


instance CohomologyN (Form b) where
    -- | automatic optimisation: boundary of boundary is zero
    -- coboundaryn (Differential (Differential _)) = Function (\_ -> 0)
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

-- | Simplifying using loops is monadic!
loopBoundary :: FreeAb (DiscreteManifold Char) NZ
loopBoundary = simplifyFreeAb $  getManifoldBoundary  loop `gbind` getManifoldBoundary

f :: Form Char NZ
f = Function $ 
    \d -> if d == a then 1 else if d == b then 2 else if d == c then 4 else error "unknown"


mainForms :: IO ()
mainForms = do
  print $ integrateForm  (Differential  f) (getManifoldBoundary loop) -- 0
  print $ integrateForm  (Differential (Differential  f)) (singletonFreeAb loop 1)-- 0
  print $ integrateForm f (singletonFreeAb a 1)
  print $ integrateForm f (singletonFreeAb b 1)
  print $ integrateForm f (singletonFreeAb c 1)
  print $ integrateForm f (getManifoldBoundary ab)

-- Step 2: Build probability theory
-- ================================
-- Discrete mapping, takes points to ponts. Can we determine everything else
-- from this?

-- | A distribution is a 0-form on the space

type Distribution a = Form a NZ

data RandomVariable a where 
    Return :: a -> RandomVariable a
    Sample01 :: (Bool -> RandomVariable a) -> RandomVariable a
   deriving(Functor)

instance Monad RandomVariable where
    return = Return
    (Return a) >>= a2mb = a2mb a
    (Sample01 maproducer) >>= a2mb = 
        Sample01 $ \b -> (maproducer b) >>= a2mb

instance Applicative RandomVariable where
    pure = return
    (<*>) = ap

-- | bilinear function between points
type Metric a = DiscreteManifold a NZ -> DiscreteManifold a NZ -> R

-- | Given a random variable, approximate the distribution
-- | Can we _create_ an over-approximation of the domain where we sample
-- from, and then use that as a manifold???
calculateDistribution :: Eq a => RandomVariable a -> Distribution a
calculateDistribution (Return a) = undefined


-- | Sample from a given distribution
sampleDistribution :: Eq a => Distribution a -> IO [a]
sampleDistribution = undefined


-- ================================================================
-- ================================================================
-- ================================================================
-- ================================================================
-- ================================================================
-- ================================================================
-- ================================================================
-- ================================================================
-- ================================================================
-- ================================================================
-- ================================================================
-- ================================================================
-- ================================================================
-- ================================================================
-- ================================================================
-- ================================================================
-- ================================================================
-- ================================================================
-- ================================================================
-- ================================================================
-- TRYING TO DEFINE CHAIN

data Chain (n :: Nat) a where
    C0 :: a -> Chain NZ a
    CS :: [(Chain n a, Z)] -> Chain (NS n) a

deriving instance Show a => Show (Chain n a)

-- | Re-weigh the outermost elements of the chain by weight w
chainWeigh :: Z -> Chain n a -> Chain n a
chainWeigh _ (C0 a) = C0 a
chainWeigh w (CS acs) = CS [(a, w*c) | (a, c) <- acs]

instance Functor (Chain n) where
    fmap f (C0 a) = C0 (f a)
    fmap f (CS coeffs) = CS [(fmap f chain, c) | (chain, c) <- coeffs]


type family PrevN a where
    PrevN NZ = NZ
    PrevN (NS n) = n

type family PlusN a b where
    PlusN NZ NZ = NZ
    PlusN NZ a = a
    PlusN a NZ = a
    PlusN (NS n) m = NS (PlusN n m)

class GradedMonad2 (f :: Nat -> * -> *) where
    greturn2 :: a -> f NZ a
    gbind2 :: f (NS (NS n)) a -> (f (NS n) a -> f (PrevN m) b) -> f m b

instance GradedMonad2 Chain where
    greturn2 = C0

chainNormalize :: Ord (Chain (PrevN n) a) => Chain n a -> Chain n a
chainNormalize (C0 a) = C0 a
chainNormalize (CS coeffs) = CS $ M.toList $ M.fromListWith (+) coeffs

chainBoundary :: Chain (NS (NS n)) a -> Chain (NS n) a
chainBoundary (CS coeffs_n_plus_1) = CS $ do
    (CS chain_n_plus_1, c) <- coeffs_n_plus_1
    (chain_n, c') <- chain_n_plus_1
    return (chain_n, c * c')


a', b', c' :: Chain NZ Char
a' = C0 'a'
b' = C0 'b'
c' = C0 'c'

ab', bc', ca' :: Chain (NS NZ) Char
ab' = CS [(b', 1), (a', -1)]
bc' = CS [(c', 1), (b', -1)]
ca' = CS [(c', 1), (a', -1)]

abc' :: Chain (NS (NS NZ)) Char
abc' = CS [(ab', 1), (bc', 1), (ca', 1)]

main :: IO ()
main = do
  mainForms
  A.main
  -- CG.main
  CUG.main
  CAB.main
