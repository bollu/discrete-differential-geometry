{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module ChainFromFreeAb where
import qualified Data.Map as M
import Data.List (intercalate)

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
    
newtype FreeAb a = FreeAb (M.Map a Int) deriving(Eq, Ord)

instance Show a => Show (FreeAb a) where
    show (FreeAb ias) = 
        case [ show i <> show a | (a, i) <- M.toList $ M.filter (/= 0) ias] of
         [] -> "EMPTY"
         s -> intercalate "+" s

freeabScale :: Int -> FreeAb a -> FreeAb a
freeabScale i (FreeAb a) = FreeAb $ M.map (* i) a

-- we need to flatten (Int -> (Int -> b)) to (Int -> b)
inject :: a  -> FreeAb a
inject a = FreeAb (M.singleton a 1)

freeabBind :: Ord b => FreeAb a ->  (a ->  FreeAb b) -> FreeAb b
freeabBind (FreeAb ais) f = 
        mconcat [ freeabScale i (f a) | (a, i) <- M.toList ais ]

-- | This does not work, since we have a (Ord b) constraint :(.
-- Either we do some weird Coyoneda style thing and have the Ord instance
-- in the constructor, or we find some way to circumvent the requirement of
-- Ord for performance. This entire situation is just sad. @edwardk: do you have
-- any idea?
-- instance Monad FreeAb where
--   ret = inject
--   (>>=) = freeabBind
-- instance Applicative FreeAb 
-- instance Functor FreeAb


instance Ord a => Semigroup (FreeAb a) where
  (FreeAb a) <> (FreeAb b) = 
      FreeAb $ M.mergeWithKey (\_ i j -> Just (i + j)) id id a b

instance Ord a => Monoid (FreeAb a) where mempty = FreeAb mempty
instance Ord a => Group (FreeAb a) where 
  inv (FreeAb a) = FreeAb $ M.map negate a

instance Ord a => Zmodule (FreeAb a)  where
  scale = freeabScale


data Nat = NZ | NS Nat

-- | This would be so much nicer in C++, where we can define upcasting rules to have 
-- `Chain k a` automatically become `FreeAb (Chain k a)`. While we can replicate
-- this with typeclass machinery, I'm _really_ not a fan.
data Chain (n :: Nat) a where 
    Chain :: FreeAb (Chain k a) -> Chain (NS k) a
    Ret :: a -> Chain NZ a

instance Show a => Show (Chain n a) where
  show (Ret a) = show a
  show (Chain ab) = "<" <> show ab <> ">"


instance Eq a => Eq (Chain n a) where
    (Ret a) == (Ret a') = a == a'
    (Chain a) == Chain a' = a == a'

instance Ord a => Ord (Chain n a) where
    compare (Ret a) (Ret a') = compare a a'
    compare (Chain a) (Chain a') = compare a a'

data NatSing (n :: Nat) where
  NZSing :: NatSing NZ
  NSSing :: NatSing k -> NatSing (NS k)


unchain :: Chain (NS n) a -> FreeAb (Chain n a)
unchain (Chain ais) = ais

-- | this is nice, that it shows up as >>=. However, I can't actually *implement*
-- this, since I need to do something different depending on the *value* of n,
-- and I can't reflect on `n` that easily :( I really don't want to
-- `singleton`, but it looks like I might have to :/
chainBoundary :: Ord a => Chain (NS n) a -> FreeAb (Chain n a)
chainBoundary (Chain ais) = ais


a, b, c :: Chain NZ Char
a = Ret 'a'
b = Ret 'b'
c = Ret 'c'

ab, bc, ca :: Chain (NS NZ) Char
ab = Chain $ inject b <>- inject a
bc = Chain $ inject c <>- inject b
ca = Chain $ inject a <>- inject c

abc :: Chain (NS (NS NZ)) Char
abc = Chain $ inject ab <> inject bc <> inject ca

main :: IO ()
main = do
  putStrLn "vvv[ChainFromFreeAb]vvv"
  putStrLn $  "abc: " <> show abc
  putStrLn $  "boundary abc: " <> show (chainBoundary abc)
  putStrLn $  "boundary . boundary abc: " <> show ((chainBoundary abc) `freeabBind` unchain)
  -- | putStrLn $  "boundary ab: " <> show (chainBoundary ab)
  -- TODO: get this boundary working
  -- putStrLn $  "boundary ab: " <> show (chainBoundary ab)
  putStrLn "^^^[ChainFromFreeAb]^^^"
