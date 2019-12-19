{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module ChainGraded where
import qualified Data.Map as M
import Data.List (intercalate)

data Nat = NZ | NS Nat

data NatSing (n :: Nat) where
    NZSing :: NatSing NZ
    NSSing :: NatSing n -> NatSing (NS n)

instance Eq (NatSing n) where
    _ == _ = True


instance Ord (NatSing n) where
    compare _ _ = EQ


reifynatsing :: NatSing n -> Int
reifynatsing NZSing = 0
reifynatsing (NSSing n) = reifynatsing n + 1

-- | a is the index set
data Chain (n :: Nat) a where
    Vertex :: NatSing NZ -> a -> Chain NZ a
    Boundary :: NatSing (NS n)
        -> FreeAb n (Chain n a) -> Chain (NS n) a 

instance Show a => Show (Chain NZ a) where
  show (Vertex _ a) = show a

instance (Show a, Show (Chain n a)) => Show (Chain (NS n) a) where
  show (Boundary _ b) = "<" <> show b <> ">"

instance Eq a => Eq (Chain NZ a) where
    Vertex _ a == Vertex _ a' = a == a'

instance (Eq a, Eq (Chain n a)) => Eq (Chain (NS n) a) where
  Boundary  _ ais == Boundary _ ais' = ais == ais'

instance Ord a => Ord (Chain NZ a) where
    compare (Vertex _ a) (Vertex _ a') = compare a a'

instance (Ord a, Ord (Chain n a)) => Ord (Chain (NS n) a) where
  compare (Boundary  _ ais)  (Boundary _ ais') = compare ais ais'

vertex :: a -> Chain NZ a
vertex a = Vertex NZSing a

sealChain :: FreeAb n (Chain n a) -> Chain (NS n) a
sealChain f@(FreeAb n ais) = Boundary (NSSing n) f

class Monoid a => Group a where
    inv :: a -> a
    -- | group subtrace
    (<>-) :: a -> a -> a
    a <>- b = a <> (inv b)

-- | Do we really want to encode this in this generality? probably not.
-- class (Ring r, Group a) => Module r a where 
class Group a => Zmodule a  where
  scale :: Int -> a -> a
  (<>*) :: Int -> a -> a
  i <>* a = scale i a
    
data  FreeAb (n :: Nat) a where
  FreeAb :: NatSing n -> M.Map a Int -> FreeAb n a deriving(Eq, Ord)

instance Show a => Show (FreeAb n a) where
    show (FreeAb _ ias) = 
        intercalate "+" [ show i <> show a | (a, i) <- M.toList $ M.filter (/= 0) ias]

freeabScale :: Int -> FreeAb n a -> FreeAb n a
freeabScale i (FreeAb n a) = FreeAb n $ M.map (* i) a

freeabBind :: Ord b => FreeAb n a ->  (a -> FreeAb m b) -> FreeAb m b
freeabBind (FreeAb _ ais) f = 
        sconcat [ freeabScale i (f a) | (a, i) <- M.toList ais ]


instance Ord a => Semigroup (FreeAb n a) where
  (FreeAb n a) <> (FreeAb _ b) = 
      FreeAb n $ M.mergeWithKey (\_ i j -> Just (i + j)) id id a b

sconcat :: Semigroup a => [a] -> a
sconcat [a] = a
sconcat (a:as) = a <> sconcat as


a', b', c' :: Chain NZ Char
a' = vertex 'a'
b' = vertex 'b'
c' = vertex 'c'

ab', bc', ca' :: Chain (NS NZ) Char
ab' = sealChain $ FreeAb (NZSing) $ M.fromList [(b', 1), (a', -1)]
bc' = sealChain $ FreeAb (NZSing) $ M.fromList [(c', 1), (b', -1)]
ca' = sealChain $ FreeAb (NZSing) $ M.fromList [(c', 1), (a', -1)]

abc' :: Chain (NS (NS NZ)) Char
abc' = sealChain $ FreeAb (NSSing NZSing) $ M.fromList [(ab', 1), (bc', 1), (ca', 1)]

unChain :: Chain (NS n) a -> FreeAb n (Chain n a)
unChain (Boundary _ b) = b

boundaryChain :: Ord a => Ord (Chain n a) => Chain (NS n) a -> FreeAb n (Chain n a)
boundaryChain (Boundary n b) = b

main :: IO ()
main = do
  putStrLn "vvv[ChainGraded]vvv"
  putStrLn $ "ABC:" <> show abc'
  putStrLn $ "boundary ABC:" <> show (boundaryChain abc')
  -- putStrLn $ "boundary . boundary $ ABC:" <> show (boundaryChain . boundaryChain $ abc')
  putStrLn $ "AB:" <> show (ab')
  putStrLn $ "boundary AB:" <> show (boundaryChain ab')
  putStrLn "^^^[ChainGraded]^^^"
